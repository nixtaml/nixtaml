(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
(* Stage per-input verification workspaces under [$TMPDIR] & evaluate the real
	lock loader against candidate lockfiles — testing the same setup as the
	build rather than reimplementing any of it. *)
type stale = {
	patch: Name.Patch.t;
	index: int; (* 0-based position within the input’s [patches] list *)
	log: string;
}
[@@deriving show, eq]

type error = [
	| `Stale of stale
	| `Stage of string
]
[@@deriving show, eq]

let pp_error ppf = function
	| `Stale {patch; index; log} ->
		Fmt.pf
			ppf
			"patch %a (patch %d of the input’s series) no longer applies:@.@.%s"
			Name.Patch.pp
			patch
			(index + 1)
			log
	| `Stage msg ->
		Fmt.pf ppf "patch-check setup failed: %s" msg

(* Manifest-relative patch/context path → tmpdir-relative staging path.
	[file:] prefix & single leading [./] stripped; [.] segments dropped; [..]
	normalized lexically. Must stay within the run root (one level of [..]
	lands in it, which is fine); absolute & escaping paths are rejected. *)
let stage_rel (manifest_path : string) : (string, string) result =
	let stripped =
		if String.starts_with ~prefix: "file:" manifest_path then
			String.sub manifest_path 5 (String.length manifest_path - 5)
		else
			manifest_path
	in
	if stripped = "" then
		Error "empty patch path"
	else if String.starts_with ~prefix: "/" stripped then
		Error (Fmt.str "absolute patch path: %s" stripped)
	else
		let rec normalize acc ups = function
			| [] -> List.rev acc, ups
			| ("" | ".") :: rest -> normalize acc ups rest
			| ".." :: rest ->
				(
					match acc with
					| [] -> normalize [] (ups + 1) rest
					| _ :: acc' -> normalize acc' ups rest
				)
			| seg :: rest -> normalize (seg :: acc) ups rest
		in
		let parts, ups = normalize [] 0 (String.split_on_char '/' stripped) in
		if parts = [] then
			Error (Fmt.str "empty patch path: %s" stripped)
		else if ups > 1 then
			Error (Fmt.str "patch path escapes staging: %s" stripped)
		else
			Ok (String.concat "/" (List.init ups (fun _ -> "..") @ parts))

let patch_markers = [
	"patch: ****";
	"patching file";
	"can't find file to patch";
	"Hunk";
	"FAILED";
	"malformed patch";
	"Reversed (or previously applied)";
]

let contains_substring haystack needle =
	let h = String.length haystack
	and n = String.length needle
	in
	let rec loop i =
		i + n <= h && (String.sub haystack i n = needle || loop (i + 1))
	in
	n = 0 || n <= h && loop 0

(* patch(1) always narrates its own failures; anything else (eval errors,
	missing files, infra) is a setup problem, not a stale patch. Heuristic
	but load-bearing for signal quality. *)
let is_patch_failure (log : string) : bool =
	List.exists (contains_substring log) patch_markers

(* Run [cmd], returning trimmed stderr when the child fails. Non-zero exit is
	the expected failure signal here, not an exception. *)
let run_for_log ~proc_mgr ?proc_env (cmd : string list) : (unit, string) result =
	let stderr_buf = Buffer.create 8192 in
	let stderr_sink = Eio.Flow.buffer_sink stderr_buf in
	match Eio.Process.run proc_mgr ?env: proc_env ~stdout: Eio.Flow.null ~stderr: stderr_sink cmd with
	| () -> Ok ()
	| exception Eio.Io (Eio.Process.E (Eio.Process.Child_error _), _) ->
		Error (String.trim (Buffer.contents stderr_buf))
	| exception exn ->
		match String.trim (Buffer.contents stderr_buf) with
		| "" ->
			Error (Printexc.to_string exn)
		| stderr ->
			Error (Fmt.str "%s: %s" (Printexc.to_string exn) stderr)

type job = {
	name: Name.Input.t;
	patches: Name.Patch.t list; (* full ordered series *)
	loader: string; (* rendered default.nix *)
	files: (string * string) list; (* tmp-relpath * content *)
	render_subset: Name.Patch.t list -> string option; (* subset → lock.json content *)
}

let save_string ~path content : (unit, string) result =
	try
		Eio.Path.save ~create: (`Or_truncate 0o644) path content;
		Ok ()
	with
		| exn -> Error (Printexc.to_string exn)

let mkdirs dir : (unit, string) result =
	try
		Eio.Path.mkdirs ~perm: 0o755 ~exists_ok: true dir;
		Ok ()
	with
		| exn -> Error (Printexc.to_string exn)

(* Filenames mirror [Lock_loader.filename] & [Lockfile.filename] (kept as
	literals here to avoid a dependency cycle back through the loader). *)
let default_nix_filename = "default.nix"
let lock_json_filename = "lock.json"
let checks_nix_filename = "checks.nix"

(* Stage subdirectories are sanitized input names: deterministic, readable,
	debuggable. Collisions require adversarial self-inflicted naming (two
	inputs sanitizing identically), in which case checks share a dir —
	accepted, documented, vanishingly rare. *)
let sanitize_segment s =
	String.map
		(fun c ->
			match c with
			| 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' -> c
			| _ -> '_'
		)
		s

(* All non-empty initial segments as (newest element × its index × full subset),
	shortest first. *)
let cumulative_subsets (patches : 'a list) : ('a * int * 'a list) list =
	let rec loop idx subset acc = function
		| [] -> List.rev acc
		| p :: tail ->
			let grown = subset @ [p] in
			loop (idx + 1) grown ((p, idx, grown) :: acc) tail
	in
	loop 0 [] [] patches

(* Pure: file content evaluating to a list with one derivation per subset, in
	subset order. Importing the same nixpkgs store path N times costs a single
	eval (Nix memoizes identical imports within an evaluation). All paths
	relative: the file lives beside the case dirs it references. Below, plain
	nix-instantiate prints one .drv path per line & then each resulting path is
	realized in order — evaluate everything first, realize path by path. *)
let checks_nix ~(name : Name.Input.t) count : string =
	let name_esc = Util.String.escape_nix (Name.Input.take name) in
	let case i =
		Fmt.str {|(builtins.getAttr "%s" (import ./case-%d/default.nix {}))|} name_esc i
	in
	Fmt.str "[ %s ]\n" (String.concat " " (List.init count case))

(* Like run_for_log but keeps stdout: instantiate prints drv paths there. *)
let run_and_capture ~proc_mgr ?proc_env (cmd : string list) : (string, string) result =
	let stdout_buf = Buffer.create 4096
	and stderr_buf = Buffer.create 8192
	in
	let stdout_sink = Eio.Flow.buffer_sink stdout_buf
	and stderr_sink = Eio.Flow.buffer_sink stderr_buf
	in
	match Eio.Process.run proc_mgr ?env: proc_env ~stdout: stdout_sink ~stderr: stderr_sink cmd with
	| () -> Ok (String.trim (Buffer.contents stdout_buf))
	| exception Eio.Io (Eio.Process.E (Eio.Process.Child_error _), _) ->
		Error (String.trim (Buffer.contents stderr_buf))
	| exception exn ->
		match String.trim (Buffer.contents stderr_buf) with
		| "" -> Error (Printexc.to_string exn)
		| stderr -> Error (Fmt.str "%s: %s" (Printexc.to_string exn) stderr)

let find_out ~proc_mgr ~tmpbase job =
	let {name; patches; loader; files; render_subset} = job in
	let dir = Eio.Path.(tmpbase / sanitize_segment (Name.Input.take name)) in
	let (let*) = Result.bind in
	let stage res = Result.map_error (fun msg -> `Stage msg) res in
	let* () = stage (mkdirs dir) in
	let* subsets =
		List.fold_left
			(fun acc (p, idx, subset) ->
				let* acc = acc in
				let* rendered =
					match render_subset subset with
					| Some rendered -> Ok rendered
					| None -> Error (`Stage "could not render candidate lockfile")
				in
				let case_dir = Eio.Path.(dir / Fmt.str "case-%d" idx) in
				let* () = stage (mkdirs case_dir) in
				let* () = stage (save_string ~path: Eio.Path.(case_dir / default_nix_filename) loader) in
				let* () = stage (save_string ~path: Eio.Path.(case_dir / lock_json_filename) rendered) in
				let* () =
					stage (
						List.fold_left
							(fun acc (rel, content) ->
								let* () = acc in
								let* () = mkdirs Eio.Path.(case_dir / Filename.dirname rel) in
								save_string ~path: Eio.Path.(case_dir / rel) content
							)
							(Ok ())
							files
					)
				in
				Ok ((p, idx) :: acc)
			)
			(Ok [])
			(cumulative_subsets patches)
		|> Result.map List.rev
	in
	let* drv_paths =
		match subsets with
		| [] -> Ok []
		| _ :: _ ->
			let* () = stage (save_string ~path: Eio.Path.(dir / checks_nix_filename) (checks_nix ~name (List.length subsets))) in
			match run_and_capture ~proc_mgr ["nix-instantiate"; Eio.Path.native_exn Eio.Path.(dir / checks_nix_filename)] with
			| Error log -> Error (`Stage log)
			| Ok stdout ->
				let drv_paths =
					String.split_on_char '\n' stdout
					|> List.map String.trim
					|> List.filter (fun line -> line <> "")
				in
				if List.length drv_paths <> List.length subsets then
					Error (`Stage "subset/drv count mismatch")
				else
					Ok drv_paths
	in
	let rec narrow = function
		| [] -> Ok ()
		| ((p, idx), drv) :: rest ->
			match run_for_log ~proc_mgr ["nix-store"; "--realize"; drv] with
			| Ok() -> narrow rest
			| Error log ->
				if is_patch_failure log then
					Error (`Stale {patch = p; index = idx; log})
				else
					Error (`Stage log)
	in
	(* Lengths checked equal above, so combining cannot raise. *)
	let pairs = List.combine subsets drv_paths in
	(* Happy path first: realize the full series. Success, the common case, costs
		one build & we are done; only genuine patch failures pay for narrowing below
		(a single-patch failure re-realizes once there: cheap, & keeps one total code
		path instead of a partial single-case shortcut). *)
	match List.rev pairs with
	| [] -> Ok ()
	| ((_, _), full_drv) :: _ ->
		match run_for_log ~proc_mgr ["nix-store"; "--realize"; full_drv] with
		| Ok() -> Ok ()
		| Error full_log ->
			if not (is_patch_failure full_log) then
				Error (`Stage full_log)
			else
				narrow pairs
