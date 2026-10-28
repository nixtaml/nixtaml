(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
let working_directory : [`Dir] Eio.Path.t option ref = ref None

let get () =
	match !working_directory with
	| Some dir -> dir
	| None -> failwith "Working directory not set up"

let set ~directory =
	working_directory := Some directory

(* Build a path from a directory string. A relative string is joined to [cwd];
	a full path (one starting with [/]) is joined to the filesystem root — unless
	it sits under [cwd], in which case we return [cwd / rel] instead. We do this
	since Eio guards [openat2] with [RESOLVE_BENEATH], so [openat2(cwd_fd,
	"/abs/…", RESOLVE_BENEATH)] fails with [EXDEV] (Invalid cross-device link).
	The rest of Nixtamal also builds paths from [cwd], so a full-path working
	folder would break the VCS-ignore logic. Eio shows [cwd]’s native path as
	["."], so we find its full path with [Eio_posix.Low_level.realpath] & then
	test the given folder’s full path against it. If it starts with [cwd]’s,
	we take off that leading part & return [cwd / rel]; else [fs / dir_str].
	*)
let of_path_string ~env (dir_str : string) : [`Dir] Eio.Path.t =
	let cwd = Eio.Stdenv.cwd env
	and fs = Eio.Stdenv.fs env
	in
	if String.length dir_str > 0 && dir_str.[0] = '/' then
		(
			let cwd_abs = Eio_posix.Low_level.realpath (Eio.Path.native_exn cwd) in
			match (try Some (Eio_posix.Low_level.realpath dir_str) with _ -> None) with
			| Some d_abs when d_abs = cwd_abs -> cwd
			| Some d_abs when String.starts_with ~prefix: (cwd_abs ^ "/") d_abs ->
				let rel =
					String.sub
						d_abs
						(String.length cwd_abs + 1)
						(String.length d_abs - String.length cwd_abs - 1)
				in
				Eio.Path.(cwd / rel)
			| _ -> Eio.Path.(fs / dir_str)
		)
	else Eio.Path.(cwd / dir_str)

let resolve ~env () =
	let cwd = Eio.Stdenv.cwd env in
	match !working_directory with
	| Some dir -> dir
	| None ->
		let proc_mgr = Eio.Stdenv.process_mgr env in
		match Eio.Process.getenv_opt proc_mgr "NIXTAMAL_DIRECTORY" with
		| Some d when String.length d > 0 -> of_path_string ~env d
		| _ -> Eio.Path.(cwd / "nix" / "tamal")

let set_default ~env () =
	working_directory := Some (resolve ~env ())

let pp_native_path =
	Fmt.using
		Eio.Path.native
		(Fmt.option (fun ppf -> Fmt.pf ppf " @@ %a" Fmt.string))

let silo_dir = ".silo"
let silo_input_dir = "input"
let silo_patch_dir = "patch"

let darcs_context_dir = "darcs_context"

(* Vendored patches commit to the repository (unlike the ignored generated dirs
	above), so this stays out of the ignore content. *)
let vendored_patches_dir = "vendored_patches"

(* Patch name plus the source URL’s extension (such as [.diff] kept), defaulting
	to [.patch]. *)
let vendored_filename (name : Name.Patch.t) (url : string) : string =
	let strip_after c s =
		match String.index_opt s c with
		| Some i -> String.sub s 0 i
		| None -> s
	in
	let base =
		let stem = strip_after '#' (strip_after '?' url) in
		match String.rindex_opt stem '/' with
		| Some i -> String.sub stem (i + 1) (String.length stem - i - 1)
		| None -> stem
	in
	let ext =
		match String.rindex_opt base '.' with
		| Some i when i > 0 && i < String.length base - 1 ->
			String.sub base i (String.length base - i)
		| _ -> ".patch"
	in
	(Name.Patch.take name) ^ ext

(* Manifest-relative path (``./…``), matching local patch convention. *)
let vendored_relpath (name : Name.Patch.t) (url : string) : string =
	Fmt.str "./%s/%s" vendored_patches_dir (vendored_filename name url)

(* Without the need for magic strings, we can use tabs in Nix! *)
let root_editor_config_content =
	{|root = true

[*]
charset = utf-8
end_of_line = lf
indent_style = tab
insert_final_newline = true
trim_trailing_whitespace = true
|}

let root_ignore_content =
	let open Fmt in
	str
		"%a@."
		(list ~sep: (Fmt.any "@.") string)
		[
			darcs_context_dir;
			silo_dir;
		]

let set_up_editor_config ~dir ~content =
	let editor_config_file = Eio.Path.(dir / ".editorconfig") in
	Logs.info (fun m -> m "Writing new Nixtamal EditorConfig%a …" pp_native_path editor_config_file);
	Eio.Path.with_open_out ~follow: false ~create: (`Or_truncate 0o644) editor_config_file @@ fun flow ->
	Eio.Buf_write.with_flow flow @@ fun writer ->
	Eio.Buf_write.string writer content

let set_up_ignore ~dir ~content =
	let ignore_file = Eio.Path.(dir / ".ignore") in
	Logs.info (fun m -> m "Writing new Nixtamal ignore%a …" pp_native_path ignore_file);
	Eio.Path.with_open_out ~follow: false ~create: (`Or_truncate 0o644) ignore_file @@ fun flow ->
	Eio.Buf_write.with_flow flow @@ fun writer ->
	Eio.Buf_write.string writer content

let set_up_silo () =
	let dir = Eio.Path.(get () / silo_dir) in
	match Eio.Path.kind ~follow: true dir with
	| `Directory ->
		()
	| `Not_found ->
		Logs.info (fun m -> m "Making Nixtamal’s silo directory%a" pp_native_path dir);
		Eio.Path.mkdirs ~perm: 0o755 dir;
	| _ ->
		failwith @@ Fmt.str "There is a Nixtamal path, but is not a directory%a" pp_native_path dir

let set_up_root () =
	let dir = get () in
	match Eio.Path.kind ~follow: true dir with
	| `Not_found ->
		Logs.info (fun m -> m "Making Nixtamal directory%a" pp_native_path dir);
		Eio.Path.mkdirs ~perm: 0o755 dir;
		set_up_editor_config ~dir ~content: root_editor_config_content;
		set_up_ignore ~dir ~content: root_ignore_content;
		if Setting.gc_enabled () then
			set_up_silo ();
	| `Directory ->
		Logs.warn (fun m -> m "Nixtamal directory already exists%a" pp_native_path dir)
	| _ ->
		failwith @@ Fmt.str "There is a Nixtamal path, but is not a directory%a" pp_native_path dir

let darcs_context_editor_config_content =
	{|root = true
|}

let set_up_darcs_context_if_needed () : (unit, string) result =
	let dir = Eio.Path.(get () / darcs_context_dir) in
	match Eio.Path.kind ~follow: true dir with
	| `Directory ->
		Ok ()
	| `Not_found ->
		Logs.info (fun m -> m "Making Nixtamal’s darcs_context directory%a" pp_native_path dir);
		Eio.Path.mkdirs ~perm: 0o755 dir;
		(* as these files are just copied over, unset rules *)
		set_up_editor_config ~dir ~content: darcs_context_editor_config_content;
		Ok ()
	| _ ->
		Error (Fmt.str "There is a Nixtamal path, but is not a directory%a" pp_native_path dir)

let vendored_patches_editor_config_content =
	{|root = true
|}

let set_up_vendored_patches_if_needed () : (unit, string) result =
	let dir = Eio.Path.(get () / vendored_patches_dir) in
	match Eio.Path.kind ~follow: true dir with
	| `Directory ->
		Ok ()
	| `Not_found ->
		Logs.info (fun m -> m "Making Nixtamal’s vendored patches directory%a" pp_native_path dir);
		Eio.Path.mkdirs ~perm: 0o755 dir;
		(* as these files are just copied over, unset rules *)
		set_up_editor_config ~dir ~content: vendored_patches_editor_config_content;
		Ok ()
	| _ ->
		Error (Fmt.str "There is a Nixtamal path, but is not a directory%a" pp_native_path dir)
