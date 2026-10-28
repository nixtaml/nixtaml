(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
module Error = Error
module Name = Name
module Store_path = Store_path
module Setting = Setting
module Schema = Schema
module Manifest = Manifest
module Lockfile = Lockfile
module Lock_loader = Lock_loader
module Features = Features
module Input = Input
module Patch_check = Patch_check
module Foreman = Foreman
module Phase = Phase
module Silo = Silo
module Working_directory = Working_directory
module Blueprint = Blueprint
module VCS = VCS
module KDL = KDL
module Locator = Locator
module TUI = TUI
module XDG = XDG
module Buffered_logs = Buffered_logs

type error = Error.t

(* NOTE: NOT melding fetch_time from lockfile *)
let meld_input_with_lock (input : Input.t) (lock : Lockfile.Input'.t) : (Input.t, Error.meld_error) result =
	let (let*) = Result.bind in
	let* kind =
		match input.kind, lock.kind with
		| `File file, `File _ ->
			Ok (`File file)
		| `Archive archive, `Archive _ ->
			Ok (`Archive archive)
		| `Git git, `Git({datetime; latest_revision; _}: Lockfile.Git.t) ->
			Ok (`Git {git with datetime; latest_revision})
		| `Darcs darcs, `Darcs({datetime; reference; latest_weak_hash; _}: Lockfile.Darcs.t) ->
			Ok (
				`Darcs {darcs with
					datetime;
					latest_weak_hash;
					reference = (
						match darcs.reference, reference with
						| `Context (`Assumed None), `Context (`Assumed Some _) -> reference
						| _ -> darcs.reference
					);
				}
			)
		| `Pijul pijul, `Pijul({datetime; latest_state; _}: Lockfile.Pijul.t) ->
			Ok (`Pijul {pijul with datetime; latest_state})
		| `Fossil fossil, `Fossil({date; latest_checkin; _}: Lockfile.Fossil.t) ->
			Ok (`Fossil {fossil with date; latest_checkin})
		| ((#Input.Kind.t as ik)), ((#Lockfile.Kind.t as lk)) ->
			let to_error = function
				| `File _ -> Error.File
				| `Archive _ -> Error.Archive
				| `Git _ -> Error.Git
				| `Darcs _ -> Error.Darcs
				| `Pijul _ -> Error.Pijul
				| `Fossil _ -> Error.Fossil
			in
			Error (`Input_kind_mismatch (input.name, to_error ik, to_error lk))
	in
	let hash =
		if input.hash.algorithm = lock.hash.algorithm then
				{input.hash with value = lock.hash.value}
		else
			input.hash
	and fresh = {input.fresh with value = lock.fresh_value}
	and store_name =
		(* rather manifest-stated store name *)
		match input.store_name, lock.store_name with
		| Some sn, _ -> Some sn
		| None, Some sn -> Some sn
		| None, None -> Input.derive_store_name input.name
	and fetch_status = Fetch_status.default
	in
	let melded = Input.set_fetch_time {input with store_name; kind; hash; fresh; fetch_status} in
	Ok melded

let meld_patch_with_lock (manifest : Manifest.Patch.t) (lock : Lockfile.Patch.t) (name : Name.Patch.t) : Input.Patch.t =
	let open Input in
	match manifest.Manifest.Patch.source with
	| Manifest.Patch.Source.Local path ->
		Input.Patch.make ~name ~source: (Patch.Source.Local path)
	| Manifest.Patch.Source.Remote {url; hash = mnfst_hash; fresh_cmd; frozen; gc; store_name = mnfst_sn; vendored} ->
		let fresh =
			{(Manifest.Fresh_cmd.of_manifest fresh_cmd) with value = lock.fresh_value}
		and hash =
			let algorithm = mnfst_hash.algorithm
			and value =
				if mnfst_hash.algorithm = lock.hash.algorithm then
					lock.hash.value
				else None
			and expected = mnfst_hash.expected
			in
			Hash.make ~algorithm ?value ?expected ()
		and store_name =
			(* rather manifest-stated store name *)
			match mnfst_sn, lock.store_name with
			| Some sn, _ -> Some sn
			| None, Some sn -> Some sn
			| None, None -> Input.derive_patch_store_name name
		in
		let source = Patch.Source.Remote (Patch.Source.make_remote ~url ~fresh ~hash ~frozen ?gc ?store_name ~vendored ()) in
		Input.Patch.make ~name ~source

let read_manifest_and_lockfile
		?(force_names : Name.InputSet.t option)
		()
		: (Name.Input.t list * Manifest.Patch.t list, error) result
	=
	let (let*) = Result.bind in
	Foreman.nix ();
	let* manifest =
		Error.tag_manifest @@ begin
			match Manifest.read () with
			| Ok(kdl : KDL.t) ->
				Manifest.document_to_t kdl
				|> Result.map_error (fun err -> `Parsing err)
			| Error(e : KDL.error) ->
				let v_errs : KDL.Valid.err list = [`Parse_error e] in
				Error (`Parsing v_errs)
		end
	in
	let* lockfile_opt =
		let read_result = Lockfile.read () in
		(* Force-locking specific inputs: swallow parse errors, propagate others *)
		if Option.is_some force_names then
			begin
				match read_result with
				| Ok opt -> Ok opt
				| Error (`Parsing _) ->
					Logs.warn (fun m -> m "Could not read lockfile (likely empty/corrupt), continuing without it");
					Ok None
				| Error e -> Error.tag_lockfile (Result.Error e)
			end
		else
			(* Normal mode: propagate all errors *)
			begin
				match read_result with
				| Ok opt -> Ok opt
				| Error e -> Error.tag_lockfile (Result.Error e)
			end
	in
	match lockfile_opt with
	| Some lock when not (Schema.Version.equal manifest.version lock.version) ->
		Error (`Version_mismatch (manifest.version, lock.version))
	| _ ->
		let to_input d =
			let input = Manifest.Input'.of_manifest d in
			let found_input =
				Option.bind lockfile_opt (fun lock -> Name.InputMap.find_opt input.name lock.inputs)
			in
			match found_input, force_names with
			| None, _ ->
				begin
					let input' =
						match input.store_name with
						| None -> {input with store_name = Input.derive_store_name input.name}
						| Some _ -> input
					in
					Ok input'
				end
			(* if forced & in the force_names list, skip the lockfile data *)
			| Some _, Some force_names' when Name.InputSet.mem input.name force_names' ->
				begin
					Logs.info (fun m -> m "Skipping meld for %a (force-locking)" Name.Input.pp input.name);
					let input' =
						match input.store_name with
						| None -> {input with store_name = Input.derive_store_name input.name}
						| Some _ -> input
					in
					Ok input'
				end
			| Some locked, _ ->
				begin
					Logs.info (fun m -> m "Melding %a with lockfile" Name.Input.pp input.name);
					meld_input_with_lock input locked
				end
		in
		(* collects names + melds inputs (or propagating errors) *)
		let* names, inputs =
			Error.tag_meld @@ begin
				let rec loop names inputs = function
					| [] -> Ok (names, inputs)
					| d :: rest ->
						let* input = to_input d in
						loop (input.Input.name :: names) (input :: inputs) rest
				in
				loop [] [] manifest.inputs
			end
		in
		Error.tag_foreman @@ begin
			let rec add_patches_and_make_set patch_set = function
				| [] -> Ok patch_set
				| manifest_patch :: rest ->
					let name = manifest_patch.Manifest.Patch.name in
					let open Input in
					Logs.info (fun m -> m "Processing patch %a from manifest" Name.Patch.pp name);
					let found_patch =
						Option.bind lockfile_opt (fun lock -> Name.PatchMap.find_opt name lock.patches)
					in
					let patch : Input.Patch.t =
						match found_patch with
						| Some lock_patch ->
							Logs.debug (fun m -> m "Patch %a found in lockfile" Name.Patch.pp name);
							meld_patch_with_lock manifest_patch lock_patch name
						| None ->
							Logs.debug (fun m -> m "Patch %a not found in lockfile" Name.Patch.pp name);
							let patch = Manifest.Patch.of_manifest manifest_patch in
							begin
								match patch.Input.Patch.source with
								| Input.Patch.Source.Local _ -> patch
								| Input.Patch.Source.Remote remote ->
									begin
										match remote.Input.Patch.Source.store_name with
										| Some _ -> patch
										| None ->
											let remote = {remote with Input.Patch.Source.store_name = Input.derive_patch_store_name name} in
												{patch with Input.Patch.source = Input.Patch.Source.Remote remote}
									end
							end
					in
					(
						match patch.source with
						| Patch.Source.Local path ->
							Logs.info (fun m -> m "Adding patch %a as Local %s" Name.Patch.pp name path)
						| Patch.Source.Remote remote ->
							Logs.info (fun m -> m "Adding patch %a as Remote %s" Name.Patch.pp name (Input.Template.take remote.url))
					);
					let* () = Foreman.add_patch name patch in
					add_patches_and_make_set (Name.PatchSet.add name patch_set) rest
			in
			let* patch_set = add_patches_and_make_set Name.PatchSet.empty manifest.patches in
			let* () =
				let rec add_all_inputs = function
					| [] -> Ok ()
					| input :: rest ->
						let missing = Name.PatchSet.diff (Name.PatchSet.of_list input.Input.patches) patch_set in
						let* () =
							if Name.PatchSet.is_empty missing then
								Ok ()
							else
								Error (`Patch_missing (input.Input.name, Name.PatchSet.elements missing))
						in
						(* now that it’s all processed/checked, we add the inputs to the Foreman *)
						let* () = Foreman.add input.Input.name input in
						add_all_inputs rest
				in
				add_all_inputs inputs
			in
			Logs.debug (fun m ->
				m "Names from reckoning manifest × lockfile: %a" Fmt.(brackets (list ~sep: semi Name.Input.pp)) names
			);
			Ok (names, manifest.patches)
		end

(* assumes read_manifest_and_lockfile has been run given this unused input *)
let are_versions_latest (_ : Name.Input.t list * Manifest.Patch.t list) : bool =
	match !Lockfile.lockfile with
	| Some lock when lock.version <> Lazy.(force Schema.Version.latest) ->
		false
	| _ ->
		Option.fold ~none: false ~some: Manifest.is_document_latest_version !Manifest.manifest

let check_version_only () : (unit, error) result =
	let (let*) = Result.bind in
	let* manifest_version =
		match !Manifest.manifest with
		| Some kdl_doc ->
			let open KDL.L in
			let open KDL.Valid in
			(
				match ll @@ kdl_doc.@(node "version" // arg 0 // string_value) with
				| Ok version_str ->
					(
						match Schema.Version.of_string version_str with
						| Some v -> Ok v
						| None -> Error (`Manifest (`Parsing [`Invalid_version version_str]))
					)
				| Error _ -> Error (`Manifest `Version_not_found)
			)
		| None ->
			let working_dir = Working_directory.get () in
			let manifest_path = Eio.Path.(working_dir / Manifest.filename) in
			if not (Eio.Path.is_file manifest_path) then
				Error (`Manifest `Not_set_up)
			else
				let content =
					Eio.Path.with_open_in ~follow: false manifest_path @@ fun flow ->
					let buf = Eio.Buf_read.of_flow flow ~max_size: max_int in
					Eio.Buf_read.take_all buf
				in
				let lines = String.split_on_char '\n' content in
				let rec find = function
					| [] -> Error (`Manifest `Version_not_found)
					| line :: rest ->
						let trimmed = String.trim line in
						if String.starts_with ~prefix: "version" trimmed then
							let rest = String.trim (String.sub trimmed 7 (String.length trimmed - 7)) in
							let len = String.length rest in
							if len >= 2 && rest.[0] = '"' then
								match String.index_from_opt rest 1 '"' with
								| Some close ->
									let version_str = String.sub rest 1 (close - 1) in
									begin
										match Schema.Version.of_string version_str with
										| Some v -> Ok v
										| None -> Error (`Manifest (`Parsing [`Invalid_version version_str]))
									end
								| None -> Error (`Manifest `Version_not_found)
							else
								Error (`Manifest `Version_not_found)
						else
							find rest
				in
				find lines
	in
	Logs.info (fun m -> m "Manifest version: %a" Schema.Version.pp manifest_version);
	let* lockfile_version =
		match !Lockfile.lockfile with
		| Some lock -> Ok lock.version
		| None ->
			match Lockfile.read () with
			| Ok (Some lock) -> Ok lock.version
			(* No lockfile means it will be rebuilt at the latest version *)
			| Ok None -> Ok Lazy.(force Schema.Version.latest)
			| Error msg -> Error (`Lockfile msg)
	in
	Logs.info (fun m -> m "Lockfile version: %a" Schema.Version.pp lockfile_version);
	let latest = Lazy.(force Schema.Version.latest) in
	if manifest_version < latest || lockfile_version < latest then
		Error `Version_not_latest
	else
		Ok ()

let set_up
		~env
		?default_hash_algorithm
		?default_fetch_time
		~vcs_ignore_mode
		?nixpkgs: nixpkgs_opt
		?blueprint: blueprint_opt
		()
		: (unit, error) result
	=
	let (let*) = Result.bind in
	Eio.Switch.run @@ fun sw ->
	let proc_mgr = Eio.Stdenv.process_mgr env in
	Logs.app (fun m -> m "%t@." Banner.pp);
	Working_directory.set_up_root ();
	if Setting.gc_enabled () then
		VCS.set_up_ignore ~env ~mode: vcs_ignore_mode;
	Option.iter
		(fun dha -> Input.Hash.default_algorithm := dha)
		default_hash_algorithm;
	Option.iter (fun ft -> Input.Fetch_time.default := ft) default_fetch_time;
	let* () =
		if Manifest.exists () then
			begin
				Logs.warn (fun m ->
					m
						"Found existing “%s”, so project is already set up. Skipping."
						Manifest.filename
				);
				Ok ()
			end
		else
			let* () =
				match nixpkgs_opt with
				| None -> Ok ()
				| Some npkgs ->
					let* () =
						Error.tag_foreman @@ begin
							let* () = Foreman.add Input.Nixpkgs.name npkgs in
							let* _phase = Foreman.lock_one ~env ~sw ~proc_mgr ~force: true ~name: Input.Nixpkgs.name () in
							Ok ()
						end
					in
					let _lockfile = Lockfile.make_lockfile () in
					Ok ()
			in
			let* () =
				Error.tag_manifest @@ begin
					let () = Manifest.make ?default_hash_algorithm ?default_fetch_time () in
					Manifest.write ()
				end
			in
			let* () = Lockfile.write ~create: (`Exclusive 0o644) () in
			Lock_loader.write ();
			Ok ()
	in
	match blueprint_opt with
	| None -> Ok ()
	| Some bp ->
		Blueprint.set_up_and_write ~env bp
		|> Result.map_error (fun e -> `Blueprint e)

let check_soundness ~env: _ () : (unit, error) result =
	let (let*) = Result.bind in
	let* reckoning = read_manifest_and_lockfile () in
	Logs.info (fun m -> m "Manifest & lockfiles were readable.");
	let latest = are_versions_latest reckoning in
	assert latest;
	if latest then
		Logs.info (fun m -> m "All versions are latest.");
	(* Check Darcs context files exist *)
	let* () =
		let inputs = Foreman.get_all_inputs () in
		let rec check_darcs_ctx (ips : (Name.Input.t * Input.t) Seq.t) =
			match Seq.uncons ips with
			| None ->
				Ok ()
			| Some ((name, input), tail) ->
				let* () =
					match input.Input.kind with
					| `Darcs d ->
						begin
							match d.Input.Darcs.reference with
							| `Context (`Assumed (Some ctx_path)) ->
								let ctx_file = Eio.Path.(Working_directory.get () / ctx_path) in
								if Eio.Path.is_file ctx_file then
									Ok ()
								else
									let msg = Fmt.str "Context file not found for %a @@ %a" Name.Input.pp name Eio.Path.pp ctx_file in
									Error (`Darcs_context msg)
							| `Context (`Stated _) ->
								Ok ()
							| `Context (`Assumed None) ->
								let msg = Fmt.str "Context missing for %a; input needs locking" Name.Input.pp name in
								Error (`Darcs_context msg)
							| _ -> Ok ()
						end
					| _ -> Ok ()
				in
				check_darcs_ctx tail
		in
		let* () = check_darcs_ctx inputs in
		Logs.info (fun m -> m "No Darcs context issues.");
		Ok ()
	in
	(* Check vendored patch files exist *)
	let* () =
		let _, manifest_patches = reckoning in
		let rec check_vendored = function
			| [] ->
				Ok ()
			| manifest_patch :: rest ->
				let* () =
					match manifest_patch.Manifest.Patch.source with
					| Manifest.Patch.Source.Local _ -> Ok ()
					| Manifest.Patch.Source.Remote {vendored = false; _} -> Ok ()
					| Manifest.Patch.Source.Remote {vendored = true; _} ->
						let name = manifest_patch.Manifest.Patch.name in
						let* patch = Error.tag_foreman (Foreman.get_patch name) in
						begin
							match patch.Input.Patch.source with
							| Input.Patch.Source.Local _ -> Ok ()
							| Input.Patch.Source.Remote remote ->
								let models = Input.Patch.jg_models2 patch in
								let rel = Working_directory.vendored_relpath name (Input.Template.fill ~models remote.url) in
								let file = Eio.Path.(Working_directory.get () / rel) in
								if Eio.Path.is_file file then
									Ok ()
								else
									let msg = Fmt.str "Vendored file not found for %a @@ %a; run `nixtamal lock` to fetch it" Name.Patch.pp name Eio.Path.pp file in
									Error (`Vendored_patch msg)
						end
				in
				check_vendored rest
		in
		let* () = check_vendored manifest_patches in
		Logs.info (fun m -> m "No vendored patch issues.");
		Ok ()
	in
	Logs.app (fun m -> m "All sound.");
	Ok ()

let tweak ~env () : (unit, error) result =
	let working_dir = Working_directory.get () in
	let manifest_file : string =
		let path = Eio.Path.(working_dir / Manifest.filename) in
		Eio.Path.native_exn path
	in
	let () = Editor.run_on manifest_file in
	check_soundness ~env ()

let show ~env: _ () : (unit, error) result =
	let (let*) = Result.bind in
	let* reckoning = read_manifest_and_lockfile () in
	Fmt.pr "%t" Foreman.pp_for_earthlings;
	if not (are_versions_latest reckoning) then
		Logs.warn (fun m -> m "Manifest ∨ lockfile is not using the latest schema version");
	Ok ()

let list_stale ~env ~domain_count ?skip_patches () : (unit, error) result =
	Eio.Switch.run @@ fun sw ->
	let (let*) = Result.bind in
	let proc_mgr = Eio.Stdenv.process_mgr env in
	let* reckoning = read_manifest_and_lockfile () in
	assert (are_versions_latest reckoning);
	Error.tag_foreman @@ begin
		Foreman.list_stale ~env ~sw ~proc_mgr ~domain_count ?skip_patches ()
	end

let uses_blake3 ~(inputs : Name.Input.t list) ~(patches : Name.Patch.t list) : bool =
	let input_uses =
		List.exists
			(fun name ->
				match Foreman.get name with
				| Ok input -> input.Input.hash.algorithm = Input.Hash.BLAKE3
				| Error _ -> false
			)
			inputs
	and patch_uses =
		lazy(
			List.exists
				(fun name ->
					match Foreman.get_patch name with
					| Ok patch ->
						(
							match patch.Input.Patch.source with
							| Input.Patch.Source.Local _ -> false
							| Input.Patch.Source.Remote remote -> remote.hash.Input.Hash.algorithm = Input.Hash.BLAKE3
						)
					| Error _ -> false
				)
				patches
		)
	in
	input_uses || Lazy.(force patch_uses)

let check_blake3_fitness ~env ~proc_mgr () : (unit, error) result =
	let check_passed_dir = XDG.check_passed_dir env in
	let check_marker = Eio.Path.(check_passed_dir / "BLAKE3") in
	if Eio.Path.is_file check_marker then
		Ok ()
	else
		let nix_version =
			let stdout_buf = Buffer.create 64 in
			let stdout_sink = Eio.Flow.buffer_sink stdout_buf in
			(Eio.Process.run proc_mgr ~stdout: stdout_sink ["nix"; "--version"]);
			Buffer.contents stdout_buf |> String.trim
		in
		let eval_has_feature =
			try
				Eio.Process.run
					proc_mgr
					~stdout: Eio.Flow.null
					["nix"; "hash"; "file"; "--type"; "blake3"; "/dev/null"];
				true
			with
				| _ -> false
		in
		if not eval_has_feature then
			let b3e = (`Feature_missing nix_version: Error.blake3_error) in
			Error (`BLAKE3_not_supported b3e)
		else
			let eval_feature_enabled =
				try
					Eio.Process.run
						proc_mgr
						~stdout: Eio.Flow.null
						~stderr: Eio.Flow.null
						["nix"; "eval"; "--expr"; "builtins ? hashFile"];
					true
				with
					| _ -> false
			in
			if eval_feature_enabled then
				begin
					Eio.Path.mkdirs ~perm: 0o755 check_passed_dir;
					Eio.Path.with_open_out ~follow: false ~create: (`Or_truncate 0o644) check_marker (fun _ -> ());
					Ok ()
				end
			else
				let b3e = (`Feature_not_enabled nix_version: Error.blake3_error) in
				Error (`BLAKE3_not_supported b3e)

(* Pre-run lockfile state for patch-change detection: per-input patch series
	(order-sensitive — reordering or adding/removing hashless local patches
	changes the combination without changing any hash) plus content hashes to
	compare post-prefetch models against. Sourced from the already-loaded
	lockfile (read once at startup); unset means no lockfile, which fails
	safe toward verifying everything. *)
let old_lock_data () : Foreman.old_lock_data =
	let hash_value (h : Lockfile.Hash.t) = h.Lockfile.Hash.value in
	match !Lockfile.lockfile with
	| Some lock ->
		{
			Foreman.series =
			Name.InputMap.map (fun (entry : Lockfile.Input'.t) -> entry.Lockfile.Input'.patches) lock.Lockfile.inputs;
			input_hashes =
			Name.InputMap.map (fun (entry : Lockfile.Input'.t) -> hash_value entry.Lockfile.Input'.hash) lock.Lockfile.inputs;
			patch_hashes =
			Name.PatchMap.map (fun (entry : Lockfile.Patch.t) -> hash_value entry.Lockfile.Patch.hash) lock.Lockfile.patches;
		}
	| None ->
		{Foreman.series = Name.InputMap.empty; input_hashes = Name.InputMap.empty; patch_hashes = Name.PatchMap.empty}

let lock ~env ~domain_count ?force ?names ?patches ?skip_patch_check ~(use_tui : bool) () : (unit, error) result =
	if use_tui then
		Buffered_logs.enable ();
	Fun.protect
		~finally: (fun () ->
			if use_tui then
				Buffered_logs.disable ()
		)
		(fun () ->
			let force_names = Option.map (Name.InputSet.of_list) names in
			Eio.Switch.run @@ fun sw ->
			let (let*) = Result.bind in
			let proc_mgr = Eio.Stdenv.process_mgr env in
			let* (all_names, all_patches) = read_manifest_and_lockfile ?force_names () in
			assert (are_versions_latest (all_names, all_patches));
			let selected_inputs =
				match names, patches with
				| _, Some (_ :: _) -> []
				| Some ns, _ -> ns
				| None, _ -> all_names
			and selected_patches =
				let patch_names = List.map (fun p -> p.Manifest.Patch.name) all_patches in
				match patches with
				| None | Some [] -> patch_names
				| Some ps ->
					let ps_set = Name.PatchSet.of_list ps in
					List.filter (fun p -> Name.PatchSet.mem p ps_set) patch_names
			in
			(* --force only applies when inputs are selected; patches on
				the lock path are always if-needed *)
			let force = Option.value ~default: false force && selected_inputs <> [] in
			let* () =
				if uses_blake3 ~inputs: selected_inputs ~patches: selected_patches then
					check_blake3_fitness ~env ~proc_mgr ()
				else
					Ok ()
			in
			let on_input_phase, on_patch_phase =
				if use_tui then
					begin
						let tui = TUI.make ~input_count: (List.length selected_inputs) ~patch_count: (List.length selected_patches) () in
						let () = TUI.set_up_items tui selected_inputs selected_patches in
						let input_phase name phase = TUI.on_phase tui (TUI.Input name) phase
						and patch_phase name phase = TUI.on_phase tui (TUI.Patch name) phase
						in
						Some input_phase, Some patch_phase
					end
				else
					None, None
			in
			let loader = Lock_loader.render_jingoo_template ~version: Lazy.(force Schema.Version.latest) () in
			let old_lock_data = old_lock_data () in
			let* () =
				if selected_inputs = [] && selected_patches = [] then
					Ok ()
				else
					Error.tag_foreman @@
						Foreman.lock
							~env
							~sw
							~proc_mgr
							~domain_count
							~force
							~input_names: selected_inputs
							~patch_names: selected_patches
							~loader
							~lock_subset_renders: Lockfile.lock_subset_renders
							~old_lock_data
							~skip_patch_check: (Option.value ~default: false skip_patch_check)
							?on_input_phase
							?on_patch_phase
							()
			in
			let* () = Lockfile.write () in
			Lock_loader.write ();
			Foreman.clean_unlisted_from_silo ();
			Ok ()
		)

let refresh ~env ~domain_count ?skip_patches ?skip_patch_check ?names ?patches ~(use_tui : bool) () : (unit, error) result =
	if use_tui then
		Buffered_logs.enable ();
	Fun.protect
		~finally: (fun () ->
			if use_tui then
				Buffered_logs.disable ()
		)
		(fun () ->
			Eio.Switch.run @@ fun sw ->
			let (let*) = Result.bind in
			let proc_mgr = Eio.Stdenv.process_mgr env in
			let* (all_names, all_patches) = read_manifest_and_lockfile () in
			assert (are_versions_latest (all_names, all_patches));
			let selected_inputs =
				match names, patches with
				| Some ns, _ -> ns
				| None, Some (_ :: _) -> []
				| None, _ -> all_names
			in
			let selected_patches =
				match skip_patches with
				| Some true -> []
				| _ ->
					let patch_names = List.map (fun p -> p.Manifest.Patch.name) all_patches in
					let explicit =
						match patches with
						| Some (_ :: _ as ps) ->
							let ps_set = Name.PatchSet.of_list ps in
							List.filter (fun p -> Name.PatchSet.mem p ps_set) patch_names
						| _ -> []
					in
					match names with
					| Some (_ :: _ as ns) ->
						let known = Name.PatchSet.of_list patch_names in
						List.fold_left
							(fun acc name ->
								match Foreman.get name with
								| Error _ -> acc
								| Ok input ->
									List.fold_left
										(fun acc p ->
											if Name.PatchSet.mem p known && not (List.mem p acc) then p :: acc else acc
										)
										acc
										input.Input.patches
							)
							explicit
							ns
					| _ ->
						match patches with
						| Some (_ :: _) -> explicit
						| _ -> patch_names
			in
			let* () =
				if uses_blake3 ~inputs: selected_inputs ~patches: selected_patches then
					check_blake3_fitness ~env ~proc_mgr ()
				else
					Ok ()
			in
			let on_input_phase, on_patch_phase =
				if use_tui then
					begin
						let tui = TUI.make ~input_count: (List.length selected_inputs) ~patch_count: (List.length selected_patches) () in
						let () = TUI.set_up_items tui selected_inputs selected_patches in
						let input_phase name phase = TUI.on_phase tui (TUI.Input name) phase
						and patch_phase name phase = TUI.on_phase tui (TUI.Patch name) phase
						in
						Some input_phase, Some patch_phase
					end
				else
					None, None
			in
			let loader = Lock_loader.render_jingoo_template ~version: Lazy.(force Schema.Version.latest) () in
			let old_lock_data = old_lock_data () in
			let* () =
				if selected_inputs = [] && selected_patches = [] then
					Ok ()
				else
					Error.tag_foreman @@
						Foreman.refresh
							~env
							~sw
							~proc_mgr
							~domain_count
							~input_names: selected_inputs
							~skip_patches: (Option.value ~default: false skip_patches)
							~patch_names: selected_patches
							~loader
							~lock_subset_renders: Lockfile.lock_subset_renders
							~old_lock_data
							~skip_patch_check: (Option.value ~default: false skip_patch_check)
							?on_input_phase
							?on_patch_phase
							()
			in
			let* () = Lockfile.write () in
			Lock_loader.write ();
			Foreman.clean_unlisted_from_silo ();
			Ok ()
		)

let garbage_collect ?(gc_gen_cap = Silo.get_default_gc_gen_cap ()) () : (unit, error) result =
	try
		Silo.gc ~gc_gen_cap ();
		Ok ()
	with
		| exn ->
			Error (`Silo (`GC_exception exn))

let backup_path (filename : string) : string = filename ^ ".bak"

let upgrade
		?from
		?(to_ = Lazy.(force Schema.Version.latest))
		?(dry_run = false)
		()
		: (unit, error) result
	=
	let (let*) = Result.bind in
	Foreman.nix ();
	let working_dir = Working_directory.get () in
	let lockfile_path = Eio.Path.(working_dir / Lockfile.filename) in
	let manifest_path = Eio.Path.(working_dir / Manifest.filename) in
	let manifest_content =
		Eio.Path.with_open_in ~follow: false manifest_path @@ fun flow ->
		let buf = Eio.Buf_read.of_flow flow ~max_size: max_int in
		Eio.Buf_read.take_all buf
	in
	Logs.info (fun m -> m "Current schema version: %a" Schema.Version.pp Lazy.(force Schema.Version.latest));
	let* manifest_version =
		match !Manifest.manifest with
		| Some kdl_doc ->
			let open KDL.L in
			let open KDL.Valid in
			(
				match ll @@ kdl_doc.@(node "version" // arg 0 // string_value) with
				| Ok version_str ->
					(
						match Schema.Version.of_string version_str with
						| Some v -> Ok v
						| None -> Error (`Manifest (`Parsing [`Invalid_version version_str]))
					)
				| Error _ -> Error (`Manifest `Version_not_found)
			)
		| None ->
			let open KDL.L in
			let open KDL.Valid in
			match Eio.Path.with_open_in ~follow: false manifest_path KDL.of_flow with
			| Error _ -> Error (`Manifest `Not_set_up)
			| Ok kdl_doc ->
				match ll @@ kdl_doc.@(node "version" // arg 0 // string_value) with
				| Error _ -> Error (`Manifest `Version_not_found)
				| Ok version_str ->
					match Schema.Version.of_string version_str with
					| Some v -> Ok v
					| None -> Error (`Manifest (`Parsing [`Invalid_version version_str]))
	in
	Logs.info (fun m -> m "Manifest version: %a" Schema.Version.pp manifest_version);
	let* lockfile_version_opt =
		match !Lockfile.lockfile with
		| Some lock -> Ok (Some lock.version)
		| None ->
			match Lockfile.read () with
			| Ok (Some lock) -> Ok (Some lock.version)
			| Ok None ->
				Logs.warn (fun m -> m "Lockfile missing; skipping lockfile upgrade.");
				Ok None
			| Error msg -> Error (`Lockfile msg)
	in
	Logs.info (fun m ->
		m
			"Lockfile version: %a"
			Fmt.(option Schema.Version.pp)
			lockfile_version_opt
	);
	let* () =
		match from, lockfile_version_opt with
		| None, _ -> Ok ()
		| Some from', _ when from' <> manifest_version ->
			begin
				Logs.err (fun m -> m "Manifest doesn’t match --from value @@ %a" Schema.Version.pp from');
				Error (`Version_mismatch (manifest_version, from'))
			end
		| Some from', Some lockfile_version when from' <> lockfile_version ->
			begin
				Logs.err (fun m -> m "Lockfile doesn’t match --from value @@ %a" Schema.Version.pp from');
				Error (`Version_mismatch (lockfile_version, from'))
			end
		| Some _, _ -> Ok ()
	in
	let needs_manifest_upgrade = manifest_version < to_
	and needs_lock_upgrade =
		Option.fold ~none: false ~some: (fun v -> v < to_) lockfile_version_opt
	in
	if not needs_lock_upgrade && not needs_manifest_upgrade then
		begin
			Logs.app (fun m -> m "Already @@ %a" Schema.Version.pp to_);
			Ok ()
		end
	else if dry_run then
		begin
			let* () =
				if needs_manifest_upgrade then
					begin
						Logs.app (fun m ->
							m
								"Would upgrade %s: %a → %a"
								Manifest.filename
								Schema.Version.pp
								manifest_version
								Schema.Version.pp
								to_
						);
						Ok ()
					end
				else
					Ok ()
			in
			let* () =
				if needs_lock_upgrade then
					match lockfile_version_opt with
					| Some lockfile_version ->
						begin
							Logs.app (fun m ->
								m
									"Would upgrade %s: %a → %a"
									Lockfile.filename
									Schema.Version.pp
									lockfile_version
									Schema.Version.pp
									to_
							);
							Ok ()
						end
					| None ->
						begin
							Logs.err (fun m -> m "Missing %s ∴ won’t be able to upgrade" Lockfile.filename);
							Error (`Lockfile `Not_set_up)
						end
				else
					Ok ()
			in
			Logs.app (fun m -> m "No changes applied.");
			Ok ()
		end
	else
		let manifest_backup_path = Eio.Path.(working_dir / backup_path Manifest.filename)
		and lock_backup_path = Eio.Path.(working_dir / backup_path Lockfile.filename)
		and manifest_backup_created = ref false
		and lock_backup_created = ref false
		in
		let cleanup_backups () =
			Logs.info (fun m -> m "Cleaning up backups…");
			if !manifest_backup_created then
				try
					Eio.Path.(Sys.remove (native_exn manifest_backup_path));
					Logs.info (fun m -> m "Cleaned up.")
				with
					| exn ->
						Logs.err (fun m -> m "Failed to cleanup manifest backup: %s" (Printexc.to_string exn));
						if !lock_backup_created then
							try
								Eio.Path.(Sys.remove (native_exn lock_backup_path))
							with
								| exn ->
									Logs.err (fun m -> m "Failed to cleanup lock backup: %s" (Printexc.to_string exn))
		in
		let rollback () =
			Logs.info (fun m -> m "Rolling back backups…");
			if !manifest_backup_created then
				begin
					try
						Eio.Path.(Sys.rename (native_exn manifest_backup_path) (native_exn manifest_path))
					with
						| exn ->
							Logs.err (fun m -> m "Failed to rollback manifest: %s" (Printexc.to_string exn));
				end;
			if !lock_backup_created then
				begin
					try
						Eio.Path.(Sys.rename (native_exn lock_backup_path) (native_exn lockfile_path))
					with
						| exn ->
							Logs.err (fun m -> m "Failed to rollback lock: %s" (Printexc.to_string exn));
				end;
			Logs.info (fun m -> m "Rolled back.");
			cleanup_backups ()
		in
		try
			let* () =
				if needs_lock_upgrade then
					match lockfile_version_opt with
					| Some lockfile_version ->
						Error.tag_lockfile @@ begin
							Logs.app (fun m -> m "Upgrading %s: %a → %a" Lockfile.filename Schema.Version.pp lockfile_version Schema.Version.pp to_);
							let* lockfile_opt = Lockfile.read () in
							let* lockfile =
								match lockfile_opt with
								| Some l -> Ok l
								| None -> Error `Not_set_up
							in
							let* upgraded = Lockfile.upgrade to_ lockfile in
							Eio.Path.(Sys.rename (native_exn lockfile_path) (native_exn lock_backup_path));
							let* () =
								Lockfile.write ~lock: upgraded ()
								|> Result.map_error (function
										| `Lockfile e -> e
										| _ -> `Parsing "lockfile write error"
									)
							in
							lock_backup_created := true;
							Ok ()
						end
					| None ->
						Logs.err (fun m -> m "Missing lockfile ∴ cannot upgrade");
						Error (`Lockfile `Not_set_up)
				else
					Ok ()
			in
			let* () =
				if needs_manifest_upgrade then
					Error.tag_manifest @@ begin
						Logs.app (fun m ->
							m
								"Upgrading %s: %a → %a"
								Manifest.filename
								Schema.Version.pp
								manifest_version
								Schema.Version.pp
								to_
						);
						let* upgraded = Manifest.upgrade manifest_content in
						Eio.Path.(Sys.rename (native_exn manifest_path) (native_exn manifest_backup_path));
						Eio.Path.with_open_out ~follow: false ~create: (`Exclusive 0o644) manifest_path @@ fun flow ->
						Eio.Flow.copy_string upgraded flow;
						manifest_backup_created := true;
						Ok ()
					end
				else
					Ok ()
			in
			let* _ =
				match read_manifest_and_lockfile () with
				| Ok v ->
					Logs.info (fun m -> m "Manifest × lockfile reloaded.");
					Ok v
				| Error _ ->
					rollback ();
					Error (`Upgrade (`Could_not_verify `Manifest))
			in
			cleanup_backups ();
			Lock_loader.write ~version: to_ ();
			Logs.app (fun m -> m "Upgrade complete.");
			Ok ()
		with
			| exn ->
				rollback ();
				Error (`Upgrade (`Run_exception exn))
