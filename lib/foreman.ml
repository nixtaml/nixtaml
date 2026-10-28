(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
type error = Error.foreman_error

module Htbl = Saturn.Htbl

type t = (Name.Input.t, Input.t) Htbl.t

let inputs : t =
	Htbl.create
		~hashed_type: (module struct
			type t = Name.Input.t
			let equal = Name.Input.equal
			let hash n = Hashtbl.hash (Name.Input.take n)
		end)
		~min_buckets: 8
		~max_buckets: 1024
		()

type patches_table = (Name.Patch.t, Input.Patch.t) Htbl.t

let patches : patches_table =
	Htbl.create
		~hashed_type: (module struct
			type t = Name.Patch.t
			let equal = Name.Patch.equal
			let hash n = Hashtbl.hash (Name.Patch.take n)
		end)
		~min_buckets: 4
		~max_buckets: 256
		()

let pp fmt inputs' =
	let name_map : Input.t Name.InputMap.t =
		Htbl.to_seq inputs'
		|> Seq.fold_left
				(fun acc (name, input) -> Name.InputMap.add name input acc)
				Name.InputMap.empty
	in
	Fmt.pf fmt "%a" (Name.InputMap.pp Input.pp) name_map

(* Ugly code, but *shrug* *)
let pp_for_earthlings pff =
	let hp_k_v ppf' (k, v) =
		let open Fmt in
		pf ppf' "\t%a%a %s" (styled `Blue string) k (styled `Faint string) ":" v
	in
	let hp_betupled_input ppf' (name, kind, data) =
		let open Fmt in
		pf
			ppf'
			"%a: %a@;"
			(styled `Green string)
			(Name.Input.take name)
			(styled `Faint (parens (styled `None (styled `Yellow string))))
			kind;
		pf ppf' "%a" (list ~sep: (any "@.") hp_k_v) data;
	and betuple (input : Input.t) : Name.Input.t * string * (string * string) list =
		let models = Input.jg_models2 input in
		let fill = Input.Template.fill ~models in
		let kind_name, kind_tuples =
			match input.kind with
			| `File f ->
				"file",
				List.concat [
					["fetch-time", Input.Fetch_time.(value f.fetch_time |> to_string);
					"url", fill f.url];
					List.map (fun m -> "mirror", fill m) f.mirrors;
				]
			| `Archive a ->
				"archive",
				List.concat [
					["fetch-time", Input.Fetch_time.(value a.fetch_time |> to_string);
					"url", fill a.url];
					List.map (fun m -> "mirror", fill m) a.mirrors;
				]
			| `Git g ->
				"git",
				List.concat [
					["fetch-time", Input.Fetch_time.(value g.fetch_time |> to_string);
					"repository", fill g.repository];
					(List.map (fun m -> "mirror", fill m) g.mirrors);
					(
						match g.reference with
						| `Branch b -> ["branch", b]
						| `Tag t -> ["tag", t]
						| `Ref r -> ["ref", r]
					);
					Option.fold ~none: [] ~some: (fun d -> ["datetime", d]) g.datetime;
					["submodules", Fmt.str "%a" Fmt.bool g.submodules;
					"lfs", Fmt.str "%a" Fmt.bool g.lfs;
					];
					Option.fold ~none: [] ~some: (fun r -> ["latest-revision", r]) g.latest_revision;
				]
			| `Darcs d ->
				"darcs",
				List.concat [
					["repository", fill d.repository];
					(List.map (fun m -> ("mirror", fill m)) d.mirrors);
					(
						match d.reference with
						| `Context (`Assumed None) -> []
						| `Context (`Assumed (Some ac)) -> ["context (assumed)", ac]
						| `Context (`Stated sc) -> ["context (stated)", sc]
						| `Tag t -> [("tag", t)]
					);
					Option.fold ~none: [] ~some: (fun d -> ["datetime", d]) d.datetime;
					Option.fold ~none: [] ~some: (fun w -> ["latest-weak-hash", w]) d.latest_weak_hash;
				]
			| `Pijul p ->
				"pijul",
				List.concat [
					[("remote", fill p.remote)];
					(List.map (fun m -> "mirror", fill m) p.mirrors);
					(
						match p.reference with
						| `Channel c -> ["channel", c]
						| `State s -> ["state", s]
						| `Change c -> ["change", c]
					);
					Option.fold ~none: [] ~some: (fun d -> ["datetime", d]) p.datetime;
					Option.fold ~none: [] ~some: (fun s -> ["latest-state", s]) p.latest_state;
				]
			| `Fossil f ->
				"fossil",
				List.concat [
					[("repository", fill f.repository)];
					(
						match f.reference with
						| `Branch b -> ["branch", b]
						| `Tag t -> ["tag", t]
						| `Checkin c -> ["check-in", c]
					);
					Option.fold ~none: [] ~some: (fun d -> ["date", d]) f.date;
					Option.fold ~none: [] ~some: (fun c -> ["latest-check-in", c]) f.latest_checkin;
				]
		in
		let data_tuples : (string * string) list =
			List.concat [
				kind_tuples;
				(
					match input.fresh.cmd with
					| None -> []
					| Some (cmd, cmds) ->
						let cmd_str_filled ({prog; args}: Input.Fresh.Cmd.cmd) =
							List.map fill (prog :: args)
						in
						let cmds' =
							List.map cmd_str_filled (cmd :: cmds)
						and formatter =
							Fmt.list ~sep: (Fmt.any " | ") (Fmt.list ~sep: (Fmt.any " ") Fmt.string)
						in
							[("fresh-cmd", Fmt.str "$ %a" formatter cmds')]
				);
				Option.fold ~none: [] ~some: (fun v -> ["fresh-value", v]) input.fresh.value;
				["hash-algorithm", Input.Hash.algorithm_to_string input.hash.algorithm];
				Option.fold ~none: [] ~some: (fun r -> ["hash-value", r]) input.hash.value;
				Option.fold ~none: [] ~some: (fun r -> ["hash-expected", r]) input.hash.expected;
				["frozen", Fmt.str "%a" Fmt.bool input.frozen];
				(
					if List.is_empty input.patches then
							[]
					else
							["patches", String.concat " " (List.map Name.Patch.take input.patches)]
				);
			]
		in
			(input.name, kind_name, data_tuples)
	in
	Htbl.to_seq inputs
	|> Seq.fold_left (fun acc ((name), input) -> (name, betuple input) :: acc) []
	|> List.stable_sort (fun (name_a, _) (name_b, _) -> Name.Input.compare name_a name_b)
	|> List.map (fun (_, s) -> s)
	|> Fmt.pf pff "%a" (Fmt.list ~sep: (Fmt.any "@.@.") hp_betupled_input)

let get name : (Input.t, error) result =
	Logs.debug (fun m -> m "Get input %a" Name.Input.pp name);
	match Htbl.find_opt inputs name with
	| Some s -> Ok s
	| None -> Error (`Model_op (`Input name, `Get))

let set name input : (unit, error) result =
	Logs.debug (fun m -> m "Set input ⟨%a, %a⟩" Name.Input.pp name Input.pp input);
	if Htbl.try_set inputs name input then
		Ok ()
	else
		Error (`Model_op (`Input name, `Set))

let add name input : (unit, error) result =
	Logs.debug (fun m -> m "Add input ⟨%a, %a⟩" Name.Input.pp name Input.pp input);
	if Htbl.try_add inputs name input then
		Ok ()
	else
		Error (`Model_op (`Input name, `Add))

let drop name : (unit, error) result =
	Logs.debug (fun m -> m "Drop input %a" Name.Input.pp name);
	if Htbl.try_remove inputs name then
		Ok ()
	else
		Error (`Model_op (`Input name, `Drop))

let set_patch name patch : (unit, error) result =
	Logs.debug (fun m -> m "Set patch %a" Name.Patch.pp name);
	if Htbl.try_set patches name patch then
		Ok ()
	else
		Error (`Model_op (`Patch name, `Set))

let add_patch name patch : (unit, error) result =
	Logs.debug (fun m -> m "Add patch ⟨%a, %a⟩" Name.Patch.pp name Input.Patch.pp patch);
	if Htbl.try_add patches name patch then
		Ok ()
	else
		Error (`Model_op (`Patch name, `Add))

let get_patch name : (Input.Patch.t, error) result =
	Logs.debug (fun m -> m "Get patch %a" Name.Patch.pp name);
	match Htbl.find_opt patches name with
	| Some s -> Ok s
	| None -> Error (`Model_op (`Patch name, `Get))

let nix () =
	ignore (Htbl.remove_all inputs ());
	ignore (Htbl.remove_all patches ())

let get_all_inputs () : (Name.Input.t * Input.t) Seq.t =
	Htbl.to_seq inputs

let to_manifest mk =
	Htbl.to_seq inputs
	|> Seq.fold_left (fun acc (name, input) -> (name, mk input) :: acc) []
	|> List.stable_sort (fun (name_a, _) (name_b, _) -> Name.Input.compare name_a name_b)
	|> List.concat_map (fun (_, manifest_node) -> manifest_node)

let to_lockfile mk =
	let errs = ref [] in
	let map =
		Htbl.to_seq inputs
		|> Seq.fold_left
				(fun acc (name, input) ->
					match mk input with
					| Ok v -> Name.InputMap.add name v acc
					| Error e -> errs := (`Lockfile (`Input name, e)) :: !errs; acc
				)
				Name.InputMap.empty
	in
	match !errs with
	| [] -> Ok map
	| _ -> Error (`Many_errors (List.rev !errs))

let to_lockfile_patches mk =
	let errs = ref [] in
	let map =
		Htbl.to_seq patches
		|> Seq.fold_left
				(fun acc (name, patch) ->
					match mk patch with
					| Ok v -> Name.PatchMap.add name v acc
					| Error e -> errs := (`Lockfile (`Patch name, e)) :: !errs; acc
				)
				Name.PatchMap.empty
	in
	match !errs with
	| [] -> Ok map
	| _ -> Error (`Many_errors (List.rev !errs))

(* Local patches plus vendored remotes: both resolve to committed project
	files, rendered into the loader map & staged for checking alike. *)
let local_patches () : (Name.Patch.t * string) list =
	Saturn.Htbl.to_seq patches
	|> List.of_seq
	|> List.fold_left
			(fun acc (name, (patch : Input.Patch.t)) ->
				match patch.source with
				| Input.Patch.Source.Local path -> (name, path) :: acc
				| Input.Patch.Source.Remote {vendored = false; _} -> acc
				| Input.Patch.Source.Remote {vendored = true; url; _} ->
					let models = Input.Patch.jg_models2 patch in
					let rel = Working_directory.vendored_relpath name (Input.Template.fill ~models url) in
					(name, rel) :: acc
			)
			[]
	|> List.stable_sort (fun (name_a, _) (name_b, _) -> Name.Patch.compare name_a name_b)

let clean_unlisted_from_silo () =
	Silo.clean_unlisted_inputs inputs;
	Silo.clean_unlisted_patches patches

let cp_darcs_context ~env ~(name : Name.Input.t) ~context =
	let (let*) = Result.bind in
	let original_path =
		if String.starts_with ~prefix: "/" context then
			Eio.Path.(Eio.Stdenv.fs env / context)
		else
			Eio.Path.(Working_directory.get () / context)
	in
	let* () = Working_directory.set_up_darcs_context_if_needed () in
	let path =
		Eio.Path.(
			Working_directory.(get () / darcs_context_dir / (Fmt.str "%s.txt" (Name.Input.take name)))
		)
	in
	Logs.info (fun m ->
		m
			"Copying Darcs context file for %a from %a to %a …"
			Name.Input.pp
			name
			Eio.Path.pp
			original_path
			Eio.Path.pp
			path
	);
	let () =
		Eio.Path.with_open_in original_path @@ fun input ->
		Eio.Path.with_open_out ~follow: false ~create: (`Or_truncate 0o644) path @@ fun output ->
		Eio.Flow.copy input output
	in
	Ok (Fmt.str "./%s/%s.txt" Working_directory.darcs_context_dir (Name.Input.take name))

let vendored_file_present ~(name : Name.Patch.t) (url : string) : bool =
	let abs = Eio.Path.(Working_directory.get () / Working_directory.vendored_relpath name url) in
	Eio.Path.is_file abs

let cp_vendored_patch ~env ~(name : Name.Patch.t) ~url ~(realized : string) =
	let (let*) = Result.bind in
	let* () = Working_directory.set_up_vendored_patches_if_needed () in
	let path = Eio.Path.(Working_directory.get () / Working_directory.vendored_relpath name url) in
	Logs.info (fun m ->
		m
			"Vendoring patch %a to %a …"
			Name.Patch.pp
			name
			Eio.Path.pp
			path
	);
	let () =
		Eio.Path.with_open_in Eio.Path.(Eio.Stdenv.fs env / realized) @@ fun input ->
		Eio.Path.with_open_out ~follow: false ~create: (`Or_truncate 0o644) path @@ fun output ->
		Eio.Flow.copy input output
	in
	Ok ()

let verify_store_path (path : string) (store_name : Name.Store.t option) : (unit, Error.prefetch_error) result =
	match store_name with
	| None -> Ok ()
	| Some sn ->
		let expected = Name.Store.take sn in
		if String.ends_with ~suffix: expected path then
			Ok ()
		else
			Error (`Invalid_store_path (path, expected))

(* Helper for Foreman errors *)
let parse_store_path
		(target : [`Input of Name.Input.t | `Patch of Name.Patch.t])
		(path : string)
		: (Store_path.t, error) result
	=
	match Store_path.parse path with
	| Ok store_path -> Ok store_path
	| Error (`Invalid_store_name (bad, _, _)) ->
		Error (`Prefetch (target, `Invalid_store_path (bad, "store-name characters plus / separators")))

(* Local patch paths are repo-relative ([./…], [../…], [file:…]); resolve them
	against the project root so downstream consumers always see absolute paths. *)
let resolve_local_patch (path : string) : string =
	let stripped = Input.Patch.strip_file_prefix path in
	if String.starts_with ~prefix: "/" stripped then
		stripped
	else
		Eio.Path.(native_exn (Working_directory.get () / stripped))

let prefetch ~env ~proc_mgr ~name ?on_phase () : (unit, error) result =
	begin
		match on_phase with
		| None -> Logs.app (fun m -> m "Prefetching input %a … (this may take a while)" Name.Input.pp name)
		| Some _ -> ()
	end;
	let open Input in
	let (let*) = Result.bind in
	let* input = get name in
	let* (new_input, new_silo_path) : Input.t * string =
		Result.map_error (fun err -> `Prefetch (`Input name, err)) @@ begin
			let* fresh_result = Prefetch.get_latest ~env ~proc_mgr input in
			match input.kind, fresh_result with
			| `File _, `File f_data ->
				let* () = verify_store_path f_data.path input.store_name in
				Ok (
					{input with hash = {input.hash with value = Some f_data.hash_value}},
					f_data.path
				)
			| `Archive _, `Archive a_data ->
				let* () = verify_store_path a_data.path input.store_name in
				Ok (
					{input with hash = {input.hash with value = Some a_data.hash_value}},
					a_data.path
				)
			| `Git g, `Git g_data ->
				if input.hash.algorithm <> g_data.hash.algorithm then
					Error (`Algorithm_mismatch (`Git, input.hash.algorithm, g_data.hash.algorithm))
				else
					let* () = verify_store_path g_data.path input.store_name in
					Ok (
						{input with
							kind =
							`Git {g with
								latest_revision = Some g_data.rev;
								datetime = g_data.datetime;
							};
							hash = {input.hash with
								algorithm = g_data.hash.algorithm;
								value = Some g_data.hash.value;
							};
						},
						g_data.path
					)
			| `Darcs d, `Darcs d_data ->
				if input.hash.algorithm <> d_data.hash.algorithm then
					Error (`Algorithm_mismatch (`Darcs, input.hash.algorithm, d_data.hash.algorithm))
				else
					let* () = verify_store_path d_data.path input.store_name in
					let* reference =
						match d.reference with
						| `Context (`Assumed _) ->
							let* new_ctx =
								cp_darcs_context ~env ~name ~context: d_data.context
								|> Result.map_error (fun err -> `Darcs_context err)
							in
							Ok (`Context (`Assumed (Some new_ctx)))
						| _ ->
							Ok d.reference
					in
					Ok (
						{input with
							kind =
							`Darcs {d with
								reference;
								datetime = d_data.datetime;
								latest_weak_hash = Some d_data.weak_hash;
							};
							hash = {input.hash with
								algorithm = d_data.hash.algorithm;
								value = Some d_data.hash.value;
							};
						},
						d_data.path
					)
			| `Pijul p, `Pijul p_data ->
				if input.hash.algorithm <> p_data.hash.algorithm then
					Error (`Algorithm_mismatch (`Pijul, input.hash.algorithm, p_data.hash.algorithm))
				else
					let* () = verify_store_path p_data.path input.store_name in
					Ok (
						{input with
							kind =
							`Pijul {p with
								datetime = p_data.datetime;
								latest_state = Some p_data.state;
							};
							hash = {input.hash with
								algorithm = p_data.hash.algorithm;
								value = Some p_data.hash.value;
							};
						},
						p_data.path
					)
			| `Fossil f, `Fossil f_data ->
				let* () = verify_store_path f_data.path input.store_name in
				Ok (
					{input with
						kind =
						`Fossil {f with
							latest_checkin = Some f_data.checkin;
							date = f_data.datetime;
						};
						hash = {input.hash with
							algorithm = f_data.hash.algorithm;
							value = Some f_data.hash.value;
						};
					},
					f_data.path
				)
			| _, _ -> failwith "Prefetch kind mismatch"
		end
	in
	(match on_phase with None -> Logs.app (fun m -> m "Prefetched %a." Name.Input.pp input.name) | Some _ -> ());
	let* store_path = parse_store_path (`Input name) new_silo_path in
	let* () = set name {new_input with fetch_status = Fetched store_path} in
	let* () =
		if Setting.gc_enabled () && Input.GC.value input.gc = Input.GC.Track then
			Silo.make_silo_gc_root ~proc_mgr ~name ~store_path
			|> Result.map_error (fun err -> `Silo (`Input name, err))
		else
			Ok ()
	in
	Ok ()

let prefetch_patch
		~env
		~proc_mgr
		~(name : Name.Patch.t)
		?on_phase
		?(siloing = false)
		?(force = false)
		()
		: (unit, error) result
	=
	let (let*) = Result.bind in
	let* patch : Input.Patch.t = get_patch name in
	let* () =
		match patch.source with
		| Input.Patch.Source.Local _path ->
			begin
				(
					match on_phase with
					| None -> Logs.app (fun m -> m "Skipped prefetch for local patch %a" Name.Patch.pp name)
					| Some _ -> ()
				);
				Option.iter (fun f -> f name `Biding) on_phase;
				Option.iter (fun f -> f name (`Done `Local_patch)) on_phase;
				Ok ()
			end
		| Input.Patch.Source.Remote remote when remote.frozen && Option.is_some remote.hash.value ->
			begin
				(
					match on_phase with
					| None -> Logs.app (fun m -> m "Skipped prefetch for frozen patch %a" Name.Patch.pp name)
					| Some _ -> ()
				);
				Option.iter (fun f -> f name `Biding) on_phase;
				Option.iter (fun f -> f name (`Done `Freeze)) on_phase;
				Ok ()
			end
		| Input.Patch.Source.Remote remote ->
			let models = Input.Patch.jg_models2 patch in
			let url = Input.Template.fill ~models remote.url in
			let vendored_missing =
				remote.vendored
				&& Option.is_some remote.hash.value
				&& not (vendored_file_present ~name url)
			in
			if not force && not vendored_missing && Option.is_some remote.hash.value then
				begin
					(
						match on_phase with
						| None ->
							Logs.app (fun m -> m "Patch %a already locked; moving on (use refresh to update it)." Name.Patch.pp name)
						| Some _ -> ()
					);
					Option.iter (fun f -> f name `Biding) on_phase;
					Option.iter (fun f -> f name (`Done `Already_fresh)) on_phase;
					Ok ()
				end
			else
				let* _prefetching = Phase.to_prefetching_patch ?on_phase Phase.Biding name in
				begin
					match on_phase with
					| None when vendored_missing ->
						Logs.app (fun m -> m "Vendored file missing for patch %a; fetching it …" Name.Patch.pp name)
					| None ->
						Logs.app (fun m -> m "Prefetching patch %a …" Name.Patch.pp name)
					| Some _ -> ()
				end;
				let store_name = remote.store_name in
				let* hash_value =
					Prefetch.prefetch_patch ~url ~hash: remote.hash ?store_name ~proc_mgr ()
					|> Result.map_error (fun err -> `Prefetch (`Patch name, err))
				in
				let* realized =
					Prefetch.prefetch_patch_realize ~url ~hash_value ?store_name ~proc_mgr ()
					|> Result.map_error (fun err -> `Prefetch (`Patch name, err))
				in
				let* store_path = parse_store_path (`Patch name) realized in
				let* () =
					verify_store_path realized store_name
					|> Result.map_error (fun err -> `Prefetch (`Patch name, err))
				in
				let* () =
					if Setting.gc_enabled () && Input.GC.value remote.gc = Input.GC.Track then
						Silo.make_silo_gc_root_for_patch ~proc_mgr ~name ~store_path
						|> Result.map_error (fun err -> `Silo (`Patch name, err))
					else
						Ok ()
				in
				let new_hash = Input.Hash.make ~algorithm: remote.hash.algorithm ~value: hash_value () in
				let remote = {remote with Input.Patch.Source.hash = new_hash; fetch_status = Fetched store_path} in
				let patch = {patch with Input.Patch.source = Input.Patch.Source.Remote remote} in
				let* () = set_patch name patch in
				let* () =
					if remote.vendored then
						cp_vendored_patch ~env ~name ~url ~realized
						|> Result.map_error (fun err -> `Prefetch (`Patch name, `Vendored_patch err))
					else
						Ok ()
				in
				begin
					match on_phase with
					| None -> Logs.app (fun m -> m "Prefetched patch %a." Name.Patch.pp name)
					| Some f -> f name (`Done (if siloing then `Siloed else `Prefetched))
				end;
				Ok ()
	in
	Ok ()

(* STAGE 2: find out if patches apply  *****************************************
   Runs after the prefetch barrier: every input & patch has had its chance to
   fetch, so resolution below never races an in-flight prefetch. Phase tokens
   are *not* threaded across the barrier (that would need shared tables, a
   graph, or parallel_iter hackery); instead the [Patching] token is constructed
   fresh *after* prereqs resolve on data (a real check vs. the tokens we try to
   manage). Inputs without [patches] never enter this stage at all. *)

let get_patch_targets ~input_names ~patch_names () : Name.Input.t list =
	let patch_set = Name.PatchSet.of_list patch_names in
	let build_up_patches acc (name, (input : Input.t)) =
		if input.patches = [] then
			acc
		else if List.mem name input_names
			|| List.exists (fun patch_name -> Name.PatchSet.mem patch_name patch_set) input.patches then
			name :: acc
		else
			acc
	in
	get_all_inputs ()
	|> Seq.fold_left build_up_patches []

let named_records (input : Input.t) : ((Name.Patch.t * Input.Patch.t) list, error) result =
	let (let*) = Result.bind in
	List.fold_left
		(fun acc patch_name ->
			let* acc = acc in
			let* patch = get_patch patch_name in
			Ok ((patch_name, patch) :: acc)
		)
		(Ok [])
		input.patches
	|> Result.map List.rev

let read_project_bytes ~env (abs : string) : (string, exn) result =
	try
		Ok (
			Eio.Path.with_open_in ~follow: false Eio.Path.(Eio.Stdenv.fs env / abs) @@ fun flow ->
			let buf = Eio.Buf_read.of_flow flow ~max_size: max_int in
			Eio.Buf_read.take_all buf
		)
	with
		| exn -> Error exn

(* (tmp-relpath * content) pairs for everything the check build reads from the
   project tree: local patches + Darcs context files. *)
let stage_project_files
		~env
		~(name : Name.Input.t)
		(input : Input.t)
		(named : (Name.Patch.t * Input.Patch.t) list)
		: ((string * string) list, error) result
	=
	let (let*) = Result.bind in
	let one_file ~display manifest_rel =
		let* rel =
			Patch_check.stage_rel manifest_rel
			|> Result.map_error (fun msg -> `Patch_check (name, `Stage (Fmt.str "%s: %s" display msg)))
		in
		let abs = resolve_local_patch manifest_rel in
		let* _ = parse_store_path (`Input name) abs in
		match read_project_bytes ~env abs with
		| Ok content -> Ok (rel, content)
		| Error exn -> Error (`Patch_check (name, `Stage (Fmt.str "%s unreadable @@ %s: %s" display abs (Printexc.to_string exn))))
	in
	let* patch_files =
		List.fold_left
			(fun acc (pname, (patch : Input.Patch.t)) ->
				let* acc = acc in
				match patch.source with
				| Input.Patch.Source.Local s ->
					let* entry = one_file ~display: (Fmt.str "local patch %a" Name.Patch.pp pname) s in
					Ok (entry :: acc)
				| Input.Patch.Source.Remote {vendored = true; url; _} ->
					let models = Input.Patch.jg_models2 patch in
					let rel = Working_directory.vendored_relpath pname (Input.Template.fill ~models url) in
					let* entry = one_file ~display: (Fmt.str "vendored patch %a" Name.Patch.pp pname) rel in
					Ok (entry :: acc)
				| Input.Patch.Source.Remote _ -> Ok acc
			)
			(Ok [])
			named
	in
	let* context_files =
		match input.kind with
		| `File _ | `Archive _ | `Git _ | `Pijul _ | `Fossil _ -> Ok []
		| `Darcs {reference; _} ->
			match reference with
			| `Tag _ -> Ok []
			| `Context (`Stated s) | `Context (`Assumed (Some s)) ->
				let* entry = one_file ~display: "Darcs context" s in
				Ok [entry]
			| `Context (`Assumed None) ->
				Error (`Patch_check (name, `Stage (Fmt.str "Darcs input %a has no context file to stage" Name.Input.pp name)))
	in
	Ok (List.rev patch_files @ context_files)

(* Pre-run lockfile state for patch-change detection, built by the caller (which
	owns the lockfile layer): patch series order plus content hashes. Compared
	against post-prefetch models — values, not fetch flags, so a forced refetch
	that resolves identically still counts as unchanged. *)
type old_lock_data = {
	series: Name.Patch.t list Name.InputMap.t;
	input_hashes: string option Name.InputMap.t;
	patch_hashes: string option Name.PatchMap.t;
}
[@@deriving show]

let patch_check_one ~env ~proc_mgr ~(name : Name.Input.t) ~loader ~(tmpbase : _ Eio.Path.t) ~(lock_subset_renders : Name.Input.t -> ([`No_bootstrap | `Ready of (Name.Patch.t list -> string option)], error) result) ~(old_lock_data : old_lock_data) ?on_phase ?on_patch_phase () : (unit, error) result =
	let (let*) = Result.bind in
	let* input = get name in
	if input.frozen || input.patches = [] then
		Ok ()
	else
		let* named = named_records input in
		let series_changed =
			match Name.InputMap.find_opt name old_lock_data.series with
			| None -> true
			| Some old -> old <> input.patches
		in
		(* Content comparison, not fetch flags: a forced refetch resolving to
			the same hash is unchanged. [None] on either side means new or
			failed — verify. *)
		let hash_changed (old_h : string option) (new_h : UTF8.t option) : bool =
			match old_h, new_h with
			| Some a, Some b -> not (String.equal a b)
			| _ -> true
		in
		let input_changed =
			hash_changed (Name.InputMap.find_opt name old_lock_data.input_hashes |> Option.join) input.hash.value
		in
		let remote_changed =
			List.exists
				(fun (pname, (patch : Input.Patch.t)) ->
					match patch.source with
					| Input.Patch.Source.Local _ -> false
					| Input.Patch.Source.Remote remote ->
						hash_changed (Name.PatchMap.find_opt pname old_lock_data.patch_hashes |> Option.join) remote.hash.value
				)
				named
		in
		(* Local patches are unversioned by design, so their content is only
			known while the input is being fetched; otherwise skip. *)
		let has_locals =
			List.exists
				(fun (_, (patch : Input.Patch.t)) ->
					match patch.source with
					| Input.Patch.Source.Local _ -> true
					| Input.Patch.Source.Remote _ -> false
				)
				named
		in
		let local_refresh = input.fetch_status <> Unfetched && has_locals in
		let changed = series_changed || input_changed || remote_changed || local_refresh in
		if not changed then
			(
				Logs.debug (fun m -> m "Skipping patch check for %a: input, patches & hashes unchanged" Name.Input.pp name);
				Ok ()
			)
		else
			let () =
				let reasons =
					List.filter_map
						Fun.id
						[
							(if series_changed then Some "patch series changed" else None);
							(if input_changed then Some "input hash changed" else None);
							(if remote_changed then Some "a patch hash changed" else None);
							(if local_refresh then Some "input refreshed with local patches" else None);
						]
				in
				Logs.debug (fun m -> m "Re-verifying patches for %a: %a" Name.Input.pp name Fmt.(list ~sep: comma string) reasons)
			in
			let* () =
				match input.hash.value with
				| Some _ -> Ok ()
				| None -> prefetch ~env ~proc_mgr ~name ?on_phase ()
			in
			let* () =
				List.fold_left
					(fun acc (pname, (patch : Input.Patch.t)) ->
						let* () = acc in
						match patch.source with
						| Input.Patch.Source.Local _ -> Ok ()
						| Input.Patch.Source.Remote remote ->
							match remote.hash.value with
							| Some _ -> Ok ()
							| None -> prefetch_patch ~env ~proc_mgr ~name: pname ?on_phase: on_patch_phase ()
					)
					(Ok ())
					named
			in
			let* input = get name in
			let* named = named_records input in
			let* readiness = lock_subset_renders name in
			match readiness with
			| `No_bootstrap ->
				Logs.warn (fun m ->
					m
						"Skipping patch check for %a: no nixpkgs input to bootstrap the check build. Locking unverified — build may fail later; add a “nixpkgs” input to enable checking."
						Name.Input.pp
						name
				);
				Ok ()
			| `Ready render_subset ->
				let* patching = Phase.to_patching_input ?on_phase Phase.Prefetching name in
				let* files =
					match stage_project_files ~env ~name input named with
					| Ok files -> Ok files
					| Error err ->
						ignore (Phase.to_done_input ?on_phase patching name (`Errored err));
						Error err
				in
				let job : Patch_check.job = {
					name;
					patches = input.patches;
					loader;
					files;
					render_subset;
				}
				in
				match Patch_check.find_out ~proc_mgr ~tmpbase job with
				| Ok() ->
					ignore (Phase.to_done_input ?on_phase patching name `Patched);
					Ok ()
				| Error (`Stale failure) ->
					let at_rev =
						match input.fresh.value with
						| Some rev -> Fmt.str " @@ %s" rev
						| None -> ""
					in
					Logs.warn (fun m ->
						m
							"Input %a%s: patch %d/%d %a no longer applies (hopefully merged upstream‽). Try removing it from %a’s patches list. Checker said:@.@.%s"
							Name.Input.pp
							name
							at_rev
							(failure.index + 1)
							(List.length input.patches)
							Name.Patch.pp
							failure.patch
							Name.Input.pp
							name
							failure.log
					);
					ignore (Phase.to_done_input ?on_phase patching name `Stale_patch);
					(* Warn-&-lock: the warning above plus [Stale_patch] are the signal; the
						lock advances. *)
					Ok ()
				| Error (`Stage msg) ->
					let err = `Patch_check (name, `Stage msg) in
					ignore (Phase.to_done_input ?on_phase patching name (`Errored err));
					Error err

let run_pipeline ~sw ~proc_mgr ~(models : Input.jg_models2) cmds =
	let open Input.Fresh.Cmd in
	let rec build_pipeline ?stdin = function
		| {prog; args}, [] ->
			begin
				let stdout_buf = Buffer.create 1024
				and stderr_buf = Buffer.create 1024
				in
				let stdout_sink = Eio.Flow.buffer_sink stdout_buf
				and stderr_sink = Eio.Flow.buffer_sink stderr_buf
				and cmd = List.map (Input.Template.fill ~models) (prog :: args)
				in
				try
					Eio.Process.run proc_mgr ?stdin ~stdout: stdout_sink ~stderr: stderr_sink cmd;
					Option.iter Eio.Resource.close stdin;
					(* close pipe input after last process *)
					Ok stdout_buf
				with
					| exn -> Error (exn, stderr_buf)
			end
		| {prog; args}, next :: rest ->
			begin
				let pipe_in, pipe_out = Eio.Process.pipe ~sw proc_mgr in
				let stderr_buf = Buffer.create 1024 in
				let stderr_sink = Eio.Flow.buffer_sink stderr_buf
				and cmd = List.map (Input.Template.fill ~models) (prog :: args)
				in
				try
					Eio.Process.run proc_mgr ?stdin ~stdout: pipe_out ~stderr: stderr_sink cmd;
					Eio.Resource.close pipe_out;
					(* close writer after child is spawned *)
					build_pipeline ~stdin: pipe_in (next, rest)
				with
					| exn -> Error (exn, stderr_buf)
			end
	in
	build_pipeline cmds

let get_fresh ~sw ~proc_mgr input : (string option, error) result =
	match Input.fresh_cmd input with
	| None ->
		Ok None
	| Some cmds ->
		let name = input.name
		and models = Input.jg_models2 input
		in
		match run_pipeline ~sw ~proc_mgr ~models cmds with
		| Error (exn, stderr) ->
			Error (`Fresh (name, `Cmd_exception (exn, String.trim @@ Buffer.contents stderr)))
		| Ok stdout_buf ->
			match String.trim @@ Buffer.contents stdout_buf with
			| "" ->
				Logs.err (fun m -> m "fresh-cmd for %a came back empty" Name.Input.pp name);
				Error (`Fresh (name, `Cmd_output_empty))
			| fresh_str ->
				Ok (Some fresh_str)

module Fresh_result = struct
	type t =
		| Lacks_cmd
		| Already_fresh
		| New_fresh_value of string
	[@@deriving show, eq]
end

let lock_one ~env ~sw ~proc_mgr ~force ~(name : Name.Input.t) ?on_phase ?(siloing = false) () =
	(
		match on_phase with
		| None ->
			Logs.info (fun m ->
				if force then m "Locking input %a …" Name.Input.pp name
				else m "Locking input %a if needed …" Name.Input.pp name
			)
		| Some f ->
			f name `Biding
	);
	let (let*) = Result.bind in
	let* input = get name in
	let* checking =
		match input.fresh.cmd, input.fresh.value, force, input.frozen with
		| Some _, None, _, false
		| Some _, _, true, false ->
			begin
				match on_phase with
				| None -> Logs.app (fun m -> m "Fetching fresh value for %a …" Name.Input.pp name)
				| Some _ -> ()
			end;
			Phase.to_checking_input ?on_phase Phase.Biding name
		| _, _, _, _ -> Ok Phase.Biding
	in
	let needs_prefetch : bool =
		force
		|| if Option.is_none input.hash.value then
			true
		else
			match input.kind with
			| `File _ -> false
			| `Archive _ -> false
			| `Git g -> Option.is_none g.latest_revision
			| `Darcs d -> Option.is_none d.latest_weak_hash
			| `Pijul p -> Option.is_none p.latest_state
			| `Fossil p -> Option.is_none p.latest_checkin
	in
	if needs_prefetch then
		begin
			(* Populate fresh.value before prefetching so templates can be filled *)
			let* () =
				match checking with
				| Phase.Checking ->
					begin
						match get_fresh ~sw ~proc_mgr input with
						| Error err -> Error err
						| Ok None ->
							Logs.info (fun m -> m "%a no fresh value; will prefetch" Name.Input.pp name);
							Ok ()
						| Ok (Some new_value) ->
							let fresh = {input.fresh with value = Some new_value} in
							set input.name {input with fresh}
					end
				| _ -> Ok ()
			in
			let* _prefetching = Phase.to_prefetching_input ?on_phase checking name in
			let* () = prefetch ~env ~proc_mgr ~name ?on_phase () in
			assert (
				match get name with
				| Ok i -> Option.is_some i.hash.value
				| Error _ -> false
			);
			let end_phase = if siloing then `Siloed else `Prefetched in
			Ok (Phase.to_done_input ?on_phase _prefetching name end_phase)
		end
	else
		Ok (Phase.to_done_input ?on_phase checking name `Already_fresh)

let refresh_one ~env ~sw ~proc_mgr ~(name : Name.Input.t) ?on_phase ?(siloing = false) () =
	Option.iter (fun f -> f name `Biding) on_phase;
	let (let*) = Result.bind in
	let* input = get name in
	if input.frozen && Option.is_some input.hash.value then
		begin
			(
				match on_phase with
				| None -> Logs.app (fun m -> m "Skipped refresh for frozen input %a" Name.Input.pp name)
				| Some _ -> ()
			);
			Ok (Phase.to_done_input ?on_phase Phase.Biding name `Freeze)
		end
	else
		begin
			(
				match on_phase with
				| None -> Logs.app (fun m -> m "Refreshing input %a …" Name.Input.pp name)
				| Some _ -> ()
			);
			let* (_ : [> `Checking] Phase.input_phase) = Phase.to_checking_input ?on_phase Phase.Biding name in
			let* fresh_result : Fresh_result.t =
				begin
					(
						match on_phase with
						| None -> Logs.app (fun m -> m "Checking fresh value for %a …" Name.Input.pp name)
						| Some _ -> ()
					);
					match get_fresh ~sw ~proc_mgr input with
					| Error err ->
						Option.iter (fun f -> f name (`Done (`Errored err))) on_phase;
						Error err
					| Ok None ->
						Logs.info (fun m -> m "%a no fresh value available; will prefetch" Name.Input.pp name);
						Ok Fresh_result.Lacks_cmd
					| Ok (Some(new_value : string)) ->
						Logs.info (fun m -> m "%a old fresh value: %a" Name.Input.pp name (Fmt.option ~none: (Fmt.const Fmt.string "∅") Fmt.string) input.fresh.value);
						Logs.info (fun m -> m "%a new fresh value: %a" Name.Input.pp name Fmt.string new_value);
						let is_outdated : string option -> bool = function
							| None -> true
							| Some value -> value <> new_value
						in
						if is_outdated input.fresh.value then
							Ok (Fresh_result.New_fresh_value new_value)
						else
							Ok Fresh_result.Already_fresh
				end
			in
			match fresh_result with
			| Fresh_result.Lacks_cmd ->
				(
					match on_phase with
					| None -> Logs.warn (fun m -> m "No fresh value; fetching from scratch.")
					| Some _ -> ()
				);
				let* _prefetching = Phase.to_prefetching_input ?on_phase Phase.Checking name in
				begin
					match prefetch ~env ~proc_mgr ~name ?on_phase () with
					| Ok() -> Ok (Phase.to_done_input ?on_phase _prefetching name `Lacked_fresh_cmd)
					| Error err ->
						Option.iter (fun f -> f name (`Done (`Errored err))) on_phase;
						Error err
				end
			| Fresh_result.Already_fresh ->
				(
					match on_phase with
					| None -> Logs.app (fun m -> m "%a already fresh; moving on." Name.Input.pp name)
					| Some _ -> ()
				);
				Ok (Phase.to_done_input ?on_phase Phase.Checking name `Already_fresh)
			| Fresh_result.New_fresh_value new_value ->
				let fresh = {input.fresh with value = Some new_value} in
				let* () = set input.name {input with fresh} in
				let* _prefetching = Phase.to_prefetching_input ?on_phase Phase.Checking name in
				begin
					match prefetch ~env ~proc_mgr ~name ?on_phase () with
					| Ok() ->
						let end_phase = if siloing then `Siloed else `Prefetched in
						Ok (Phase.to_done_input ?on_phase _prefetching name end_phase)
					| Error err ->
						Option.iter (fun f -> f name (`Done (`Errored err))) on_phase;
						Error err
				end
		end

type parallel_item =
	| Input of Name.Input.t
	| Patch of Name.Patch.t

type parallel_result =
	| All_passed
	| Some_failed of (parallel_item * error) list
	| All_failed of (parallel_item * error) list

let parallel_iter
		?(on_done = fun _ _ -> ())
		~(domain_count : int)
		~(f : parallel_item -> (unit, error) result)
		(items : parallel_item list)
		: parallel_result
	=
	let sem = Eio.Semaphore.make domain_count in
	let results = ref []
	and result_lock = Eio.Mutex.create ()
	in
	let processes =
		List.map
			(fun item () ->
				Eio.Semaphore.acquire sem;
				Fun.protect
					~finally: (fun () -> Eio.Semaphore.release sem)
					(fun () ->
						let result = f item in
						Eio.Mutex.lock result_lock;
						Fun.protect
							~finally: (fun () -> Eio.Mutex.unlock result_lock)
							(fun () ->
								results := (item, result) :: !results;
								on_done item result
							)
					)
			)
			items
	in
	Eio.Fiber.all processes;
	let success_count = List.fold_left (fun acc (_, r) -> acc + if r = Ok () then 1 else 0) 0 !results in
	let failed : (parallel_item * error) list =
		List.fold_left
			(fun acc (item, result) ->
				match result with
				| Ok() -> acc
				| Error err -> (item, err) :: acc
			)
			[]
			!results
	in
	match success_count, List.length failed with
	| n, 0 when n = List.length items -> All_passed
	| 0, _ -> All_failed failed
	| _, _ -> Some_failed failed

let get_parallel_items
		?(skip_patches = false)
		?(input_names = [])
		?(patch_names = [])
		()
		: parallel_item list
	=
	match input_names, patch_names with
	| [], [] ->
		let input_names' =
			Htbl.to_seq inputs
			|> Seq.fold_left (fun acc (name, _) -> Input name :: acc) []
		in
		if skip_patches then
			input_names'
		else
			Htbl.to_seq patches
			|> Seq.fold_left (fun acc (name, _) -> Patch name :: acc) input_names'
	| _, _ ->
		List.map (fun name -> Input name) input_names @ (
			if skip_patches then []
			else List.map (fun name -> Patch name) patch_names
		)

let list_stale ~env: _ ~sw ~proc_mgr ~domain_count ?skip_patches () : (unit, error) result =
	Logs.debug (fun m ->
		m "Probing stale for inputs."
	);
	let items = get_parallel_items ~input_names: [] ?skip_patches () in
	let f : parallel_item -> (unit, error) result = fun item ->
		match item with
		| Input name ->
			let (let*) = Result.bind in
			let* input = get name in
			begin
				match get_fresh ~sw ~proc_mgr input with
				| Error _ -> Ok ()
				| Ok None -> Ok ()
				| Ok (Some new_value) ->
					let is_outdated : string option -> bool =
						Option.fold ~none: true ~some: (Fun.compose not (String.equal new_value))
					in
					if is_outdated input.fresh.value then
						(
							Logs.app (fun m -> m "%a: %s" Fmt.(styled `Green string) (Name.Input.take name) new_value);
							Ok ()
						)
					else
						Ok ()
			end
		| Patch _ -> Ok ()
	in
	match parallel_iter ~domain_count ~f items with
	| All_passed -> Ok ()
	| Some_failed errs ->
		List.iter
			(fun (name, err) ->
				match name with
				| Input n -> Logs.warn (fun m -> m "Couldn’t probe stale input %a: %a" Name.Input.pp n Error.pp_foreman_error err)
				| Patch n -> Logs.warn (fun m -> m "Couldn’t probe stale patch %a: %a" Name.Patch.pp n Error.pp_foreman_error err)
			)
			errs;
		begin
			match errs with
			| [_, err] -> Error err
			| errs -> Error (`Many_errors (List.map snd errs))
		end
	| All_failed errs ->
		begin
			match errs with
			| [_, err] -> Error err
			| errs -> Error (`Many_errors (List.map snd errs))
		end

let patch_check_wave
		~env
		~proc_mgr
		~domain_count
		~input_names
		~patch_names
		~(loader : string)
		~(lock_subset_renders : Name.Input.t -> ([`No_bootstrap | `Ready of (Name.Patch.t list -> string option)], error) result)
		~(old_lock_data : old_lock_data)
		?on_input_phase
		?on_patch_phase
		()
		: (unit, error) result
	=
	match get_patch_targets ~input_names ~patch_names () with
	| [] -> Ok ()
	| patch_targets ->
		let tmpbase =
			let tmpdir =
				Eio.Process.getenv_opt proc_mgr "TMPDIR"
				|> Option.value ~default: "/tmp"
			and patchdir =
				Fmt.str "nixtamal-patch-check-%d" (Unix.getpid ())
			in
			Eio.Path.(Eio.Stdenv.fs env / tmpdir / patchdir)
		in
		Fun.protect
			~finally: (fun () ->
				try
					match Eio.Path.kind ~follow: false tmpbase with
					| `Directory -> Eio.Path.rmtree tmpbase
					| _ -> ()
				with
					| exn ->
						Logs.warn (fun m ->
							m "Could not cleanup verification workspace @@ %a: %s" Eio.Path.pp tmpbase (Printexc.to_string exn)
						)
			)
			(fun () ->
				let f : parallel_item -> (unit, error) result = function
					| Input name ->
						patch_check_one ~env ~proc_mgr ~name ~loader ~tmpbase ~lock_subset_renders ~old_lock_data ?on_phase: on_input_phase ?on_patch_phase: on_patch_phase ()
					| Patch _ ->
						Ok ()
				in
				match parallel_iter ~domain_count ~f (List.map (fun name -> Input name) patch_targets) with
				| All_passed ->
					Ok ()
				| Some_failed errs ->
					List.iter
						(fun (name, err) ->
							match name with
							| Input n ->
								Logs.warn (fun m -> m "Couldn’t verify patches for input %a: %a" Name.Input.pp n Error.pp_foreman_error err)
							| Patch n ->
								Logs.warn (fun m -> m "Couldn’t verify patch %a: %a" Name.Patch.pp n Error.pp_foreman_error err)
						)
						errs;
					Ok ()
				| All_failed errs ->
					begin
						match errs with
						| [_, err] -> Error err
						| errs -> Error (`Many_errors (List.map snd errs))
					end
			)

let lock
		~env
		~sw
		~proc_mgr
		~domain_count
		?(force = false)
		?(input_names = [])
		?(patch_names = [])
		?(skip_patch_check = false)
		~(loader : string)
		~(lock_subset_renders : Name.Input.t -> ([`No_bootstrap | `Ready of (Name.Patch.t list -> string option)], error) result)
		~(old_lock_data : old_lock_data)
		?on_input_phase
		?on_patch_phase
		()
		: (unit, error) result
	=
	match get_parallel_items ~input_names ~patch_names () with
	| [] -> Ok ()
	| items ->
		let (let*) = Result.bind in
		let f : parallel_item -> (unit, error) result = fun item ->
			let (let*) = Result.bind in
			match item with
			| Input name ->
				let* input = get name in
				let tracked = Input.GC.value input.gc = Input.GC.Track in
				let siloing = Setting.gc_enabled () && tracked && not (Silo.has_silo_entry (`Input name)) in
				let force = force || siloing in
				let* _phase = lock_one ~env ~sw ~proc_mgr ~force ~siloing ~name ?on_phase: on_input_phase () in
				Ok ()
			| Patch name ->
				let* patch = get_patch name in
				let tracked =
					match patch.source with
					| Input.Patch.Source.Local _ -> false
					| Input.Patch.Source.Remote remote -> Input.GC.value remote.gc = Input.GC.Track
				in
				let siloing = Setting.gc_enabled () && tracked && not (Silo.has_silo_entry (`Patch name)) in
				prefetch_patch ~env ~proc_mgr ~name ~siloing ~force ?on_phase: on_patch_phase ()
		in
		let* () =
			match parallel_iter ~domain_count ~f items with
			| All_passed -> Ok ()
			| Some_failed errs ->
				List.iter
					(fun (name, err) ->
						match name with
						| Input n -> Logs.warn (fun m -> m "Couldn’t lock input %a: %a" Name.Input.pp n Error.pp_foreman_error err)
						| Patch n -> Logs.warn (fun m -> m "Couldn’t lock patch %a: %a" Name.Patch.pp n Error.pp_foreman_error err)
					)
					errs;
				Ok ()
			| All_failed errs ->
				begin
					match errs with
					| [_, err] -> Error err
					| errs -> Error (`Many_errors (List.map snd errs))
				end
		in
		if skip_patch_check then
			Ok ()
		else
			patch_check_wave ~env ~proc_mgr ~domain_count ~input_names ~patch_names ~loader ~lock_subset_renders ~old_lock_data ?on_input_phase ?on_patch_phase ()

let refresh
		~env
		~sw
		~proc_mgr
		~domain_count
		?(input_names = [])
		?(skip_patches = false)
		?(patch_names = [])
		?(skip_patch_check = false)
		~(loader : string)
		~(lock_subset_renders : Name.Input.t -> ([`No_bootstrap | `Ready of (Name.Patch.t list -> string option)], error) result)
		~(old_lock_data : old_lock_data)
		?on_input_phase
		?on_patch_phase
		()
		: (unit, error) result
	=
	let items = get_parallel_items ~input_names ~skip_patches ~patch_names () in
	if items = [] then
		Ok ()
	else
		let f : parallel_item -> (unit, error) result = fun item ->
			let (let*) = Result.bind in
			match item with
			| Input name ->
				let* input = get name in
				let tracked = Input.GC.value input.gc = Input.GC.Track in
				let siloing = Setting.gc_enabled () && tracked && not (Silo.has_silo_entry (`Input name)) in
				let* _phase = refresh_one ~env ~sw ~proc_mgr ~siloing ~name ?on_phase: on_input_phase () in
				Ok ()
			| Patch name ->
				let* patch = get_patch name in
				let tracked =
					match patch.source with
					| Input.Patch.Source.Local _ -> false
					| Input.Patch.Source.Remote remote -> Input.GC.value remote.gc = Input.GC.Track
				in
				let siloing = Setting.gc_enabled () && tracked && not (Silo.has_silo_entry (`Patch name)) in
				prefetch_patch ~env ~proc_mgr ~name ~siloing ~force: true ?on_phase: on_patch_phase ()
		in
		let (let*) = Result.bind in
		let* () =
			match parallel_iter ~domain_count ~f items with
			| All_passed -> Ok ()
			| Some_failed errs ->
				List.iter
					(fun (name, err) ->
						match name with
						| Input n -> Logs.warn (fun m -> m "Couldn’t refresh input %a: %a" Name.Input.pp n Error.pp_foreman_error err)
						| Patch n -> Logs.warn (fun m -> m "Couldn’t refresh patch %a: %a" Name.Patch.pp n Error.pp_foreman_error err)
					)
					errs;
				Ok ()
			| All_failed errs ->
				begin
					match errs with
					| [_, err] -> Error err
					| errs -> Error (`Many_errors (List.map snd errs))
				end
		in
		if skip_patch_check then
			Ok ()
		else
			patch_check_wave ~env ~proc_mgr ~domain_count ~input_names ~patch_names ~loader ~lock_subset_renders ~old_lock_data ?on_input_phase ?on_patch_phase ()
