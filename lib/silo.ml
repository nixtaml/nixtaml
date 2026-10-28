(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
module Htbl = Saturn.Htbl

(*
	Layout:

	.silo/${name}/
	├── milpa/ (generation timeline)
	│   ├── 1 → /nix/store/... (GC root)
	│   └── 2 → /nix/store/... (GC root)
	└── elotl → milpa/2 (current generation)

	Each milpa line is a GC root made by nix-store --add-root.
	elotl points to the current generation for easy access.
 *)

module Generation = struct
	(* uint32 displayed as hex *)
	type t =
		Generation of Stdint.uint32
	[@@unboxed]

	let [@inline]make n = Generation (Stdint.Uint32.of_int n)
	let [@inline]take (Generation n) = n
	let [@inline]of_string s =
		match int_of_string_opt ("0x" ^ s) with
		| Some i when i >= 0 -> Some (make i)
		| Some _ | None -> None
	let to_string t = Fmt.str "%0lx" (Stdint.Uint32.to_int32 (take t))
	let pp ppf t = Fmt.pf ppf "%0lx" (Stdint.Uint32.to_int32 (take t))
	let equal (Generation a) (Generation b) = Stdint.Uint32.compare a b = 0

	let min = make 0
	let [@inline]succ (Generation n) = Generation (Stdint.Uint32.succ n)
	let [@inline]pred (Generation n) = Generation (Stdint.Uint32.pred n)
	let [@inline](+) (Generation a) (Generation b) = Generation (Stdint.Uint32.add a b)
	let [@inline](-) (Generation a) (Generation b) = Generation (Stdint.Uint32.sub a b)

	let gen = QCheck.map make QCheck.int
end

let has_silo_entry (entry_name : [`Input of Name.Input.t | `Patch of Name.Patch.t]) : bool =
	let path =
		match entry_name with
		| `Input name ->
			Eio.Path.(Working_directory.(get () / silo_dir / silo_input_dir) / Name.Input.take name / "elotl")
		| `Patch name ->
			Eio.Path.(Working_directory.(get () / silo_dir / silo_patch_dir) / Name.Patch.take name / "elotl")
	in
	match Eio.Path.kind ~follow: false path with
	| `Symbolic_link -> true
	| _ -> false

let unlink_or_rm ~(path : _ Eio.Path.t) =
	match Eio.Path.kind ~follow: false path with
	| `Symbolic_link ->
		Eio.Path.unlink path;
		Logs.debug (fun m -> m "Silo: unlinked %a." Eio.Path.pp path)
	| `Not_found ->
		()
	| `Directory | `Regular_file | `Socket ->
		Eio.Path.rmtree path;
		Logs.debug (fun m -> m "Silo: removed %a." Eio.Path.pp path)
	| _ ->
		Logs.warn (fun m -> m "Silo: unhandled file kind for %a" Eio.Path.pp path)

let unlink_or_rm_silo ~(at : [`Path of _ Eio.Path.t | `InputName of Name.Input.t | `PatchName of Name.Patch.t]) =
	let path =
		match at with
		| `Path p -> p
		| `InputName n -> Eio.Path.(Working_directory.(get () / silo_dir / silo_input_dir) / Name.Input.take n)
		| `PatchName n -> Eio.Path.(Working_directory.(get () / silo_dir / silo_patch_dir) / Name.Patch.take n)
	in
	unlink_or_rm ~path

let make_sure_directory ~(path : _ Eio.Path.t) =
	match Eio.Path.kind ~follow: false path with
	| `Directory ->
		()
	| `Not_found ->
		Eio.Path.mkdirs ~perm: 0o755 path
	| `Symbolic_link ->
		Eio.Path.unlink path;
		Eio.Path.mkdirs ~perm: 0o755 path
	| `Regular_file | `Socket ->
		Eio.Path.rmtree path;
		Eio.Path.mkdirs ~perm: 0o755 path
	| _ ->
		Logs.warn (fun m -> m "Silo: unhandled file kind for %a" Eio.Path.pp path)

let read_dir_raw ~(path : _ Eio.Path.t) : string array =
	let dir_path = Eio.Path.native_exn path in
	Sys.readdir dir_path
(* Use Sys.readdir instead of Eio.Path.read_dir to avoid cross-device
	symlink resolution via openat2 RESOLVE_NO_XDEV when directory contains
	symlinks pointing to Nix store paths on different filesystems. *)

let get_next_generation ~(milpa_path : _ Eio.Path.t) : Generation.t =
	let paths = read_dir_raw ~path: milpa_path in
	Array.fold_left
		(fun max_now path_name ->
			match Generation.of_string path_name with
			| Some t -> max max_now t
			| None -> max_now
		)
		Generation.min
		paths
	|> Generation.succ

let find_generation_by_store_path ~(milpa_path : _ Eio.Path.t) ~(store_path : Store_path.t) : Generation.t option =
	let paths = read_dir_raw ~path: milpa_path in
	let result = ref None in
	Array.iter
		(fun path_name ->
			match Generation.of_string path_name with
			| Some gen ->
				let link_path = Eio.Path.(milpa_path / path_name) in
				(
					match Eio.Path.kind ~follow: false link_path with
					| `Symbolic_link ->
						let target = Eio.Path.read_link link_path in
						if String.equal target (Store_path.take store_path) then
							result := Some gen
					| _ -> ()
				)
			| None -> ()
		)
		paths;
	!result

let gc_prune_input ~(gc_gen_cap : Generation.t) ~(elotl_path : _ Eio.Path.t) ~(milpa_path : _ Eio.Path.t) =
	match Eio.Path.kind ~follow: false elotl_path with
	| `Symbolic_link ->
		let elotl_target = Eio.Path.read_link elotl_path in
		let prefix = "milpa/" in
		if String.starts_with ~prefix elotl_target then
			let gen_now =
				let follow_elotl =
					String.sub elotl_target (String.length prefix) (String.length elotl_target - String.length prefix)
				in
				match Generation.of_string follow_elotl with
				| Some n -> n
				| None -> Generation.min
			in
			let cutoff =
				if gc_gen_cap > gen_now then Generation.min
				else Generation.(gen_now - gc_gen_cap)
			in
			let paths = read_dir_raw ~path: milpa_path in
			Array.iter
				(fun link_name ->
					match Generation.of_string link_name with
					| Some gen when gen <= cutoff ->
						let path = Eio.Path.(milpa_path / link_name) in
						unlink_or_rm ~path
					| _ -> ()
				)
				paths
		else
			Logs.warn (fun m -> m "Symlink not in milpa %s" elotl_target)
	| _ -> ()

let gc_prune_silo ~(gc_gen_cap : Generation.t) ~(silo_path : _ Eio.Path.t) =
	let elotl_path = Eio.Path.(silo_path / "elotl")
	and milpa_path = Eio.Path.(silo_path / "milpa")
	in
	gc_prune_input ~gc_gen_cap ~elotl_path ~milpa_path

let get_default_gc_gen_cap () : Generation.t =
	match Option.bind (Sys.getenv_opt "NIXTAMAL_GC_GENERATIONS") int_of_string_opt with
	| Some i when i >= 0 -> Generation.make i
	| _ -> Generation.make 2

let make_sure_silo_directories ~base_path ~name =
	let (let*) = Result.bind in
	let* () =
		match Eio.Path.kind ~follow: false base_path with
		| `Directory -> Ok ()
		| `Not_found ->
			let main_silo_path = Eio.Path.(Working_directory.(get () / silo_dir)) in
			if String.equal (Eio.Path.native_exn base_path) (Eio.Path.native_exn main_silo_path) then
				Working_directory.set_up_silo ()
			else
				Eio.Path.mkdirs ~perm: 0o755 base_path;
			Ok ()
		| _ -> Error (`Silo_not_directory (Fmt.str "%a" Eio.Path.pp base_path))
	in
	let silo_path = Eio.Path.(base_path / name) in
	let* () =
		match Eio.Path.kind ~follow: false silo_path with
		| `Directory -> Ok ()
		| `Not_found -> Ok ()
		| _ -> Error (`Silo_not_directory (Fmt.str "%a" Eio.Path.pp silo_path))
	in
	let milpa_path = Eio.Path.(silo_path / "milpa")
	and elotl_path = Eio.Path.(silo_path / "elotl")
	in
	make_sure_directory ~path: milpa_path;
	Ok (milpa_path, elotl_path, silo_path)

let register_nix_root ~proc_mgr ~milpa_dir ~store_path =
	let stdout_buf = Buffer.create 1024
	and stderr_buf = Buffer.create 1024
	in
	let stdout_sink = Eio.Flow.buffer_sink stdout_buf
	and stderr_sink = Eio.Flow.buffer_sink stderr_buf
	in
	Eio.Process.run proc_mgr ~stdout: stdout_sink ~stderr: stderr_sink [
		"nix-store";
		"--indirect";
		"--add-root";
		Eio.Path.native_exn milpa_dir;
		"--realize";
		(Store_path.take store_path);
	];
	Logs.debug (fun m -> m "nix-store output: %s" (Buffer.contents stdout_buf));
	Logs.debug (fun m -> m "nix-store stderr: %s" (Buffer.contents stderr_buf))

let create_gc_root_at_path ~proc_mgr ~milpa_path ~elotl_path ~store_path ~name =
	match find_generation_by_store_path ~milpa_path ~store_path with
	| Some existing_gen ->
		Logs.info (fun m -> m "Silo: %s already at %a (gen %a), updating elotl" name Store_path.pp store_path Generation.pp existing_gen);
		let gen_milpa_dir = Eio.Path.(milpa_path / Generation.to_string existing_gen) in
		register_nix_root ~proc_mgr ~milpa_dir: gen_milpa_dir ~store_path;
		let elotl_relative = Fmt.str "milpa/%a" Generation.pp existing_gen in
		unlink_or_rm ~path: elotl_path;
		Eio.Path.symlink ~link_to: elotl_relative elotl_path;
		Ok ()
	| None ->
		let new_generation = get_next_generation ~milpa_path in
		let new_milpa_dir = Eio.Path.(milpa_path / Generation.to_string new_generation) in
		Logs.info (fun m -> m "Silo: %s now %a (gen %a)" name Store_path.pp store_path Generation.pp new_generation);
		Eio.Path.symlink ~link_to: (Store_path.take store_path) new_milpa_dir;
		register_nix_root ~proc_mgr ~milpa_dir: new_milpa_dir ~store_path;
		let elotl_relative = Fmt.str "milpa/%a" Generation.pp new_generation in
		unlink_or_rm ~path: elotl_path;
		Eio.Path.symlink ~link_to: elotl_relative elotl_path;
		Ok ()

let make_silo_gc_root ~proc_mgr ~name ~store_path =
	let (let*) = Result.bind in
	let name_str = Name.Input.take name in
	let base_path = Eio.Path.(Working_directory.(get () / silo_dir / silo_input_dir)) in
	let* (milpa_path, elotl_path, silo_path) = make_sure_silo_directories ~base_path ~name: name_str in
	if Setting.gc_enabled () then
		begin
			let* () = create_gc_root_at_path ~proc_mgr ~milpa_path ~elotl_path ~store_path ~name: name_str in
			gc_prune_silo ~gc_gen_cap: (get_default_gc_gen_cap ()) ~silo_path;
			Ok ()
		end
	else
		Ok ()

let make_silo_gc_root_for_patch ~proc_mgr ~name ~store_path =
	let (let*) = Result.bind in
	let name_str = Name.Patch.take name in
	let base_path = Eio.Path.(Working_directory.(get () / silo_dir / silo_patch_dir)) in
	let* (milpa_path, elotl_path, silo_path) = make_sure_silo_directories ~base_path ~name: name_str in
	if Setting.gc_enabled () then
		begin
			let* () = create_gc_root_at_path ~proc_mgr ~milpa_path ~elotl_path ~store_path ~name: ("patch " ^ name_str) in
			gc_prune_silo ~gc_gen_cap: (get_default_gc_gen_cap ()) ~silo_path;
			Ok ()
		end
	else
		Ok ()

(* A missing silo directory means there is nothing to clean — so no-op.
	Catching [Not_found] rather than probing with [Eio.Path.kind] avoids an
	extra stat on every run & any check-then-act race. *)
let read_dir_entries_or_empty ~(path : _ Eio.Path.t) =
	try
		Eio.Path.read_dir_entries path
	with
		| Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> []

let clean_unlisted_inputs (inputs : (Name.Input.t, 'a) Htbl.t) =
	if Setting.gc_enabled () then
		begin
			Logs.debug (fun m -> m "Silo: cleaning unlisted inputs …");
			let silo_path = Eio.Path.(Working_directory.(get () / silo_dir / silo_input_dir)) in
			(
				read_dir_entries_or_empty ~path: silo_path
				|> List.iter (fun (kind, nm) ->
						match kind with
						| `Directory ->
							let name = Name.Input.make nm in
							if not (Htbl.mem inputs name) then
								unlink_or_rm_silo ~at: (`InputName name)
						| _ -> ()
					)
			)
		end

let clean_unlisted_patches (patches : (Name.Patch.t, 'a) Htbl.t) =
	if Setting.gc_enabled () then
		begin
			Logs.debug (fun m -> m "Silo: cleaning unlisted patches …");
			let path = Eio.Path.(Working_directory.(get () / silo_dir / silo_patch_dir)) in
			read_dir_entries_or_empty ~path
			|> List.iter (fun (kind, nm) ->
					match kind with
					| `Directory ->
						let name = Name.Patch.make nm in
						if not (Htbl.mem patches name) then
							unlink_or_rm_silo ~at: (`PatchName name)
					| _ -> ()
				)
		end

let gc ?(gc_gen_cap = get_default_gc_gen_cap ()) () =
	if gc_gen_cap = Generation.min then
			()
	else
		let run_gc dir =
			read_dir_entries_or_empty ~path: dir
			|> List.iter (fun (kind, name) ->
					match kind with
					| `Directory ->
						let silo_path = Eio.Path.(dir / name) in
						gc_prune_silo ~gc_gen_cap ~silo_path
					| _ -> ()
				)
		in
		let input_dir = Eio.Path.(Working_directory.(get () / silo_dir / silo_input_dir)) in
		let patch_dir = Eio.Path.(Working_directory.(get () / silo_dir / silo_patch_dir)) in
		run_gc input_dir;
		run_gc patch_dir
