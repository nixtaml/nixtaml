(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
(* IMPORTANT: Cmdliner doesn’t support fish; keep script/completion.fish in
	sync with these commands/converters. *)
module Manifest = Nixtamal.Manifest

let prefixed_env_info ?doc ?deprecated var =
	Cmdliner.Cmd.Env.info ("NIXTAMAL_" ^ var) ?doc ?deprecated

let common_man = [
	`S "LICENSE";
	`P "GNU General Public License, version 3.0 later (GPL-3.0-or-later)";
	`S "MAKER";
	`P "toastal <https://toast.al/contact/>";
	`S "FUNDING";
	`P "See: https://toast.al/funding/";
	`S "BUGS";
	`P "No tracker is set up; contact the maker directly or join the XMPP chatroom @ <xmpp:nixtamal@chat.toastal.in.th?join>.";
	`S "SEE ALSO";
	`I ("nixtamal(1)", "fulfilling input pinning for Nix");
	`I ("nixtamal-manifest(5)", "manifest.kdl format");
]

let exit_of_result : (unit, Nixtamal.Error.t) result -> Cmdliner.Cmd.Exit.code = function
	| Ok() ->
		0
	| Error err ->
		Fmt.epr
			"%a %a@."
			(Fmt.styled `Red Fmt.string)
			"Error:"
			Nixtamal.Error.pp
			err;
		let internal_error = Cmdliner.Cmd.Exit.internal_error in
		let open Error in
		let rec foreman_exit_code = function
			| `Fresh (_, `Cmd_exception _) -> to_code Fresh_exception
			| `Fresh (_, `Cmd_output_empty) -> to_code Fresh_empty_output
			| `Model_op _ -> internal_error
			| `Pool_exception _ -> internal_error
			| `Many_errors (e :: _) -> foreman_exit_code e
			| `Many_errors [] -> internal_error
			| `Silo _ -> to_code Silo_error
			| `Lockfile _ -> to_code Lockfile_serializing
			| `Prefetch _ -> to_code Prefetch_exception
			| `Patch_check _ -> to_code Patch_doesn't_apply
			| `Patch_missing _ -> to_code Patch_missing
		in
		match err with
		| `Manifest `Not_set_up -> to_code Manifest_not_set_up
		| `Manifest `Version_not_found
		| `Manifest `Version_invalid
		| `Manifest (`Parsing _)
		| `Manifest (`Read_error _) ->
			to_code Manifest_parsing
		| `Lockfile `Not_set_up
		| `Lockfile `Version_not_found
		| `Lockfile (`Parsing _) ->
			to_code Lockfile_parsing
		| `Lockfile (`Serializing _) ->
			to_code Lockfile_serializing
		| `Lockfile (`Upgrade _) -> to_code Upgrade_failed
		| `Meld _ -> to_code Input_kind_mismatch
		| `Version_mismatch _ -> to_code Version_mismatch
		| `Version_not_latest -> to_code Version_not_latest
		| `Foreman e -> foreman_exit_code e
		| `Silo _ -> to_code Silo_error
		| `Upgrade _ -> to_code Upgrade_failed
		| `BLAKE3_not_supported _ -> to_code BLAKE3_not_supported
		| `Darcs_context _ -> to_code Darcs_context
		| `Vendored_patch _ -> to_code Vendored_patch
		| `Blueprint _ -> to_code Blueprint_error

let prompt_yes_no ~env (message : string) () : bool =
	Unix.isatty Unix.stdin
	&& (
		(* Write without a trailing newline so the answer lands on the same line *)
		Format.eprintf "%s @?" message;
		try
			let stdin = Eio.Stdenv.stdin env in
			let buf = Eio.Buf_read.of_flow stdin ~max_size: 80 in
			let line = Eio.Buf_read.take_while (fun c -> c <> '\n') buf in
			(try ignore (Eio.Buf_read.skip 1 buf) with End_of_file -> ());
			let line = String.trim line in
			line = "" || line = "y" || line = "Y" || String.lowercase_ascii line = "yes"
		with
			| _ -> false
	)

let prompt_upgrade ~env () =
	prompt_yes_no ~env "Manifest ∨ lockfile needs upgrading. Upgrade now? [Y/n]" ()

let prompt_regenerate_lockfile ~env () =
	prompt_yes_no ~env "Lockfile is missing. Rebuild it now? This will run `nixtamal lock` [Y/n]" ()

let ensure_latest_version ~env () : (unit, Nixtamal.Error.t) result =
	match Nixtamal.check_version_only () with
	| Ok() -> Ok ()
	| Error (`Version_not_latest) ->
		if prompt_upgrade ~env () then
			Nixtamal.upgrade ()
		else
			Error `Version_not_latest
	| Error e -> Error e

let list_names ~fs section ~token =
	let open Cmdliner.Arg.Completion in
	match Manifest.list_names ~fs section with
	| Ok names when token = "" ->
		Ok (List.map string names)
	| Ok names ->
		let starts_with_token name =
			if String.starts_with ~prefix: token name then
				Some (string name)
			else
				None
		in
		Ok (List.filter_map starts_with_token names)
	| Error err ->
		Ok [message Fmt.(str "%a" Nixtamal.Error.pp err)]

let name_conv ~env ~docv section =
	let fs = Nixtamal.Working_directory.resolve ~env () in
	let open Cmdliner.Arg in
	Conv.make
		~docv
		~parser: (fun s -> Ok s)
		~pp: Format.pp_print_string
		~completion: (
			Completion.make (fun _ctx ~token ->
				list_names ~fs section ~token
			)
		)
		()

let input_name_conv ~env = name_conv ~env ~docv: "INPUT_NAME" `Inputs

let patch_name_conv ~env = name_conv ~env ~docv: "PATCH_NAME" `Patches

module Global = struct
	type t = {
		style_renderer: Fmt.style_renderer option;
		level: Logs.level option;
		dir: string option;
		jobs: int;
	}

	let directory_arg =
		let open Cmdliner in
		Arg.(
			value
			& opt (some string) None
			& info
				["directory"]
				~env: (prefixed_env_info ~doc: "Directory for Nixtamal" "DIRECTORY")
				~doc: "Working directory for Nixtamal-related files (default: \\$PWD/nix/tamal)"
				~docv: "PATH"
		)

	let jobs_arg =
		let open Cmdliner in
		let domain_count : int = Stdlib.Domain.recommended_domain_count () in
		Arg.(
			value
			& opt int domain_count
			& info
				["j"; "jobs"]
				~env: (prefixed_env_info "JOBS")
				~doc: "Nixtamal’s executor pool domain size."
				~docv: "INT"
		)

	let args =
		let open Cmdliner in
		let open Term in
		ret
			(
				const (fun style_renderer level dir jobs ->
					`Ok {style_renderer; level; dir; jobs}
				)
				$ Fmt_cli.style_renderer ~env: (prefixed_env_info "OUTPUT_COLOR") ()
				$ Logs_cli.level ~env: (prefixed_env_info "LOG_LEVEL") ()
				$ directory_arg
				$ jobs_arg
			)

	let run ~env {style_renderer; level; dir; jobs} =
		Fmt_tty.setup_std_outputs ?style_renderer ();
		Logs.set_level (
			match level with
			| None -> Some Logs.Info
			| Some lvl -> Some lvl
		);
		Logs.set_reporter (Logs_fmt.reporter ());
		Kdl.indent := 1;
		begin
			match dir with
			| None ->
				Nixtamal.Working_directory.set_default ~env ()
			| Some d ->
				Nixtamal.Working_directory.set
					~directory: (Nixtamal.Working_directory.of_path_string ~env d)
		end;
		fun f -> f ~env ~domain_count: jobs
end

module Set_up = struct
	let nixpkgs_mismatch = "Both --nixpkgs-channel & --nixpkgs-ref cannot be used at the same time"

	let info =
		Cmdliner.Cmd.info
			"set-up"
			~doc: "Set up working directory for Nixtamal. By default, also adds Nixpkgs from upstream to the project's inputs."
			~exits: Error.exits
			~man: common_man

	let run
			~env
			~domain_count: _
			default_hash_algorithm
			default_fetch_time
			vcs_ignore_mode
			(blueprint, nixpkgs)
		=
		Nixtamal.set_up
			~env
			?default_hash_algorithm
			?default_fetch_time
			~vcs_ignore_mode
			?nixpkgs
			?blueprint
			()
		|> exit_of_result

	let term ~env =
		let eio_env = env in
		let open Cmdliner in
		let hash_algorithm =
			let hash_algo_enum =
				let list = ref [] in
				let open Nixtamal.Input.Hash in
				for idx =
				min_algorithm to max_algorithm do
					match algorithm_of_enum idx with
					| Some algo ->
						let str = algorithm_to_string algo
						and tkn = algorithm_to_hash_token algo
						in
						list := (str, algo) :: (tkn, algo) :: !list
					| None -> ()
				done;
				Arg.enum !list
			in
			Arg.(
				value
				& opt (some hash_algo_enum) None
				& info
					["hash-algorithm"]
					~doc: "The default algorithm to be used for inputs & patches."
					~docv: "HASH_ALGORITHM"
			)

		and fetch_time =
			let fetch_time_enum =
				let open Nixtamal.Input.Fetch_time in
				let list = ref [] in
				for idx =
				min to max do
					match of_enum idx with
					| Some algo ->
						let str = to_string algo in
						list := (str, algo) :: !list
					| None -> ()
				done;
				Arg.enum !list
			in
			Arg.(
				value
				& opt (some fetch_time_enum) None
				& info
					["fetch-time"]
					~doc: "The default fetch time to be used for inputs."
					~docv: "FETCH_TIME"
			)

		and no_nixpkgs_arg =
			Arg.(
				value
				& flag
				& info
					["no-nixpkgs"]
					~env: (prefixed_env_info "NO_NIXPKGS")
					~doc: "Do not add Nixpkgs to the pinned inputs list by default."
			)
		and use_channels =
			Arg.(
				value
				& flag
				& info
					["c"; "use-channels"]
					~env: (prefixed_env_info "USE_NIXPKGS_CHANNELS")
					~doc: "Channels are more efficient being compressed with Zstandard instead of gzip (older stable releases still use LZMA), doesn’t rely on US megacorpations, & is recommend — despite this recommendation however, the syntax for this setup is less intuitive to users while also not providing a clear template for Git archives to be remixed for other inputs making it not the default for newcomers."
			)
		and use_mirrors_arg =
			Arg.(
				value
				& flag
				& info
					["use-mirrors"]
					~env: (prefixed_env_info "USE_MIRRORS")
					~doc: "For resilience, use the versioned TUNA mirror as a fallback. Channel inputs use the release archive with the TUNA mirror; non-channel refs use the known TUNA Git mirror. Off by default as Git updating is slightly slower & some users might object to TUNA’s hosting origin state."
			)
		and nixpkgs_channel_arg =
			Arg.(
				value
				& opt (some string) None
				& info
					["nixpkgs-channel"]
					~env: (prefixed_env_info "NIXPKGS_CHANNEL")
					~doc: (Fmt.str "Nixpkgs channel for Nixtamal setup (such as “nixpkgs-unstable”). %s." nixpkgs_mismatch)
					~docv: "CHANNEL_NAME"
			)
		and nixpkgs_ref_arg =
			Arg.(
				value
				& opt (some string) None
				& info
					["nixpkgs-ref"]
					~env: (prefixed_env_info "NIXPKGS_REF")
					~doc: (Fmt.str "Nixpkgs Git ref for Nixtamal setup (such as “refs/heads/nixos-25.11”; default: %s). %s." Nixtamal.Input.Nixpkgs.Reference.default_ref nixpkgs_mismatch)
					~docv: "REF"
			)
		in
		let nixpkgs_reference_arg =
			let open Term in
			let mk_reference nixpkgs_channel nixpkgs_ref =
				match nixpkgs_channel, nixpkgs_ref with
				| None, None -> `Ok None
				| Some channel, None -> `Ok (Some (`Channel channel))
				| None, Some ref -> `Ok (Some (`Ref ref))
				| Some _, Some _ -> `Error (true, nixpkgs_mismatch)
			in
			ret
				(
					const mk_reference
					$ nixpkgs_channel_arg
					$ nixpkgs_ref_arg
				)
		and nixpkgs_revision_arg =
			Arg.(
				value
				& opt (some string) None
				& info
					["nixpkgs-revision"]
					~env: (prefixed_env_info "NIXPKGS_REVISION")
					~doc: ("Nixpkgs Git revision for Nixtamal setup. The value will be used as the latest revision/change.")
					~docv: "REVISION"
			)

		and vcs_arg =
			let vcs_enum =
				let open Nixtamal.VCS in
				[
					("auto", Auto);
					("darcs", Forced Darcs);
					("git", Forced Git);
					("hg", Forced Mercurial);
					("pijul", Forced Pijul);
					("jj", Forced Jujutsu);
					("fossil", Forced Fossil);
					("none", None_);
				]
			in
			Arg.(
				value
				& opt (enum vcs_enum) Nixtamal.VCS.Auto
				& info
					["vcs"]
					~doc: "VCS for ignore file setup."
					~docv: "VCS"
			)
		in
		let nixpkgs_arg =
			let open Term in
			let open Nixtamal.Input in
			let mk_arg no_nixpkgs use_channels use_mirrors (reference : Nixpkgs.Reference.t option) nixpkgs_revision =
				let make_channel_input channel =
					let mirrors =
						if use_mirrors then [Nixpkgs.Channel.mirror_TUNA_URL channel] else []
					in
					Nixpkgs.make_channels ~channel ~mirrors ()
				in
				let make_channel_input_from_string channel =
					match Nixpkgs.Channel.of_string channel with
					| Some channel ->
						`Ok (Some (make_channel_input channel))
					| None ->
						`Error (true, Fmt.str "Invalid Nixpkgs channel “%s”" channel)
				in
				if no_nixpkgs then
					`Ok None
				else if use_channels then
					begin
						match reference with
						| None ->
							`Ok (Some (make_channel_input Nixpkgs.Channel.default))
						| Some (`Channel channel) ->
							make_channel_input_from_string channel
						| Some (`Ref _) ->
							`Error (true, "--use-channels is not compatible with --nixpkgs-ref; use --nixpkgs-channel")
					end
				else if use_mirrors then
					begin
						match reference with
						| None ->
							`Ok (Some (make_channel_input Nixpkgs.Channel.default))
						| Some (`Channel channel) ->
							make_channel_input_from_string channel
						| Some (`Ref _) ->
							let latest_revision = nixpkgs_revision in
							let input = Nixpkgs.make_git_with_known_mirrors ?reference ?latest_revision () in
							`Ok (Some input)
					end
				else
					let fresh_value = nixpkgs_revision in
					let input = Nixpkgs.make_git_archive ?reference ?fresh_value () in
					`Ok (Some input)
			in
			ret
				(
					const mk_arg
					$ no_nixpkgs_arg
					$ use_channels
					$ use_mirrors_arg
					$ nixpkgs_reference_arg
					$ nixpkgs_revision_arg
				)
		in
		let blueprint_arg =
			let blueprint_enum =
				let list = ref [] in
				let open Nixtamal.Blueprint in
				for idx =
				min to max do
					match of_enum idx with
					| Some bp ->
						list := (to_string bp, bp) :: !list
					| None -> ()
				done;
				Arg.enum !list
			in
			Arg.(
				value
				& opt (some blueprint_enum) None
				& info
					["b"; "blueprint"]
					~doc: "Make a complete project structure based on a blueprint to give a starting off point to build on."
					~docv: "BLUEPRINT"
			)
		in
		let blueprint_and_nixpkgs_arg =
			let open Term in
			ret
				(
					const (fun bp np ->
						match bp, np with
						| Some _, None ->
							`Error (true, "--blueprint requires nixpkgs; omit --no-nixpkgs or pass a channel/ref.")
						| _ -> `Ok (bp, np)
					)
					$ blueprint_arg
					$ nixpkgs_arg
				)
		in
		Term.(
			const (fun glb -> Global.run ~env: eio_env glb @@ run)
			$ Global.args
			$ hash_algorithm
			$ fetch_time
			$ vcs_arg
			$ blueprint_and_nixpkgs_arg
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end

module Check_soundness = struct
	let info =
		Cmdliner.Cmd.info
			"check-soundness"
			~doc: "Checks that the manifest × lockfile is sound."
			~exits: Error.exits
			~man: common_man

	let run ~env ~domain_count: _ =
		let (let*) = Result.bind in
		exit_of_result @@ begin
			let* () = ensure_latest_version ~env () in
			Nixtamal.check_soundness ~env ()
		end

	let term ~env =
		let eio_env = env in
		let open Cmdliner in
		Term.(
			const (fun glb -> Global.run ~env: eio_env glb @@ run)
			$ Global.args
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end

module Tweak = struct
	let info =
		Cmdliner.Cmd.info
			"tweak"
			~doc: "Tweak the manifest file with \\$VISUAL, \\$EDITOR, or vi"
			~exits: Error.exits
			~man: common_man

	let run ~env ~domain_count: _ =
		let (let*) = Result.bind in
		exit_of_result @@ begin
			let* () = ensure_latest_version ~env () in
			Nixtamal.tweak ~env ()
		end

	let term ~env =
		let eio_env = env in
		let open Cmdliner in
		Term.(
			const (fun glb -> Global.run ~env: eio_env glb @@ run)
			$ Global.args
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end

module Show = struct
	let info =
		Cmdliner.Cmd.info
			"show"
			~doc: "Shows current inputs as understood by Nixtamal for earthlings."
			~exits: Error.exits
			~man: common_man

	let run ~env ~domain_count: _ sections =
		exit_of_result @@ begin
			match sections with
			| None ->
				Nixtamal.show ~env ()
			| Some section ->
				let working_dir = Nixtamal.Working_directory.get () in
				Manifest.list_names ~fs: working_dir section
				|> Result.map (List.iter (Fmt.pr "%s\n"))
		end

	let term ~env =
		let eio_env = env in
		let open Cmdliner in
		let input_names_arg =
			Arg.(
				value
				& flag
				& info ["input-names"] ~doc: "List all input names from manifest."
			)
		and patch_names_arg =
			Arg.(
				value
				& flag
				& info ["patch-names"] ~doc: "List all patch names from manifest."
			)
		in
		let section_names_arg =
			let mk_section a b =
				match a, b with
				| false, false -> `Ok None
				| true, false -> `Ok (Some `Inputs)
				| false, true -> `Ok (Some `Patches)
				| true, true -> `Error (true, "--input-names & --patch-names are mutually exclusive")
			in
			Term.(ret (const mk_section $ input_names_arg $ patch_names_arg))
		in
		Term.(
			const (fun glb -> Global.run ~env: eio_env glb @@ run)
			$ Global.args
			$ section_names_arg
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end

module Lock = struct
	let info =
		Cmdliner.Cmd.info
			"lock"
			~doc: "Lock all not-yet-locked inputs."
			~exits: Error.exits
			~man: common_man

	let run ~env ~domain_count force names patches skip_patch_check no_tui =
		let (let*) = Result.bind in
		let names =
			if List.is_empty names then None
			else Some (List.map Nixtamal.Name.Input.make names)
		and patches =
			if List.is_empty patches then None
			else Some (List.map Nixtamal.Name.Patch.make patches)
		in
		exit_of_result @@ begin
			let* () = ensure_latest_version ~env () in
			Nixtamal.lock ~env ~domain_count ~force ?names ?patches ~skip_patch_check ~use_tui: (not no_tui) ()
		end

	let term ~env =
		let eio_env = env in
		let open Cmdliner in
		let force_arg =
			Arg.(
				value
				& flag
				& info ["f"; "force"] ~doc: "Force input to lock (useful if changing the manifest in a manner that otherwise wouldn’t trigger a lock such as changing a URL or hash algorithm)."
			)
		and relock_arg =
			Arg.(
				value
				& flag
				& info ["relock"] ~doc: "Alias for --force"
			)
		and names_arg =
			Arg.(
				value
				& pos_all (input_name_conv ~env) []
				& info [] ~docv: "INPUT_NAME" ~doc: "Input names to lock (if already locked, will skip)."
			)
		and patches_arg =
			Arg.(
				value
				& opt_all (patch_name_conv ~env) []
				& info
					["p"; "patch"]
					~doc: "Patch name(s) to lock (if already locked, will skip)."
					~docv: "PATCH_NAME"
			)
		and no_tui_arg =
			Arg.(
				value
				& flag
				& info ["no-tui"] ~doc: "Disable the TUI & use Logs.app for status output."
			)
		and skip_patch_check_arg =
			Arg.(
				value
				& flag
				& info
					["skip-patch-check"]
					~env: (prefixed_env_info "SKIP_PATCH_CHECK")
					~doc: "Skip checking that patches apply. Faster & works offline, but broken patches lock silently & fail at build instead."
			)
		in
		let force_lock_arg =
			let open Term in
			ret (
				const (fun force relock -> `Ok (force || relock))
				$ force_arg
				$ relock_arg
			)
		in
		Term.(
			const (fun glb -> Global.run ~env: eio_env glb @@ run)
			$ Global.args
			$ force_lock_arg
			$ names_arg
			$ patches_arg
			$ skip_patch_check_arg
			$ no_tui_arg
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end

module List_stale = struct
	let info =
		Cmdliner.Cmd.info
			"list-stale"
			~doc: "List stale inputs with fresh-cmd, without refreshing"
			~exits: Error.exits
			~man: common_man

	let run ~env ~domain_count skip_patches =
		let (let*) = Result.bind in
		exit_of_result @@ begin
			let* () = ensure_latest_version ~env () in
			Nixtamal.list_stale ~env ~domain_count ~skip_patches ()
		end

	let term ~env =
		let eio_env = env in
		let open Cmdliner in
		let skip_patches_arg =
			Arg.(
				value
				& flag
				& info ["skip-patches"] ~doc: "Skip processing all patches."
			)
		in
		Term.(
			const (fun glb -> Global.run ~env: eio_env glb @@ run)
			$ Global.args
			$ skip_patches_arg
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end

module Refresh = struct
	let info =
		Cmdliner.Cmd.info
			"refresh"
			~doc: "Refreshes all non-frozen inputs using the fresh-cmd — or the default fresh-cmd for certain kinds with a reasonable default (Git, some files/archives) — & then locks stale inputs."
			~exits: Error.exits
			~man: common_man

	let run ~env ~domain_count names patches skip_patches skip_patch_check no_tui =
		let (let*) = Result.bind in
		let names =
			if List.is_empty names then None
			else Some (List.map Nixtamal.Name.Input.make names)
		and patches =
			if List.is_empty patches then None
			else Some (List.map Nixtamal.Name.Patch.make patches)
		in
		exit_of_result @@ begin
			let* () = ensure_latest_version ~env () in
			Nixtamal.refresh ~env ~domain_count ?names ?patches ~skip_patches ~skip_patch_check ~use_tui: (not no_tui) ()
		end

	let term ~env =
		let eio_env = env in
		let open Cmdliner in
		let names_arg =
			Arg.(
				value
				& pos_all (input_name_conv ~env) []
				& info [] ~docv: "INPUT_NAME" ~doc: "Input names to refresh."
			)
		and patches_arg =
			Arg.(
				value
				& opt_all (patch_name_conv ~env) []
				& info
					["p"; "patch"]
					~doc: "Patch name(s) to refresh."
					~docv: "PATCH_NAME"
			)
		and skip_patches_arg =
			Arg.(
				value
				& flag
				& info ["skip-patches"] ~doc: "Skip processing all patches."
			)
		and skip_patch_check_arg =
			Arg.(
				value
				& flag
				& info
					["skip-patch-check"]
					~env: (prefixed_env_info "SKIP_PATCH_CHECK")
					~doc: "Skip checking that patches apply. Faster & works offline, but broken patches lock silently & fail at build instead."
			)
		and no_tui_arg =
			Arg.(
				value
				& flag
				& info ["no-tui"] ~doc: "Disable the TUI and use Logs.app for status output."
			)
		in
		Term.(
			const (fun glb -> Global.run ~env: eio_env glb @@ run)
			$ Global.args
			$ names_arg
			$ patches_arg
			$ skip_patches_arg
			$ skip_patch_check_arg
			$ no_tui_arg
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end

module GC = struct
	let info =
		Cmdliner.Cmd.info
			"gc"
			~doc: "Garbage collect old generations (experimental). Requires NIXTAMAL_EXPERIMENTAL_GC to be set & is probably still broken. Keeping the freshest (configurable) number of generations. We suggest at least 2 if favoring rebase-style workflows so swapping back & forth is less trouble."
			~exits: Error.exits
			~man: common_man

	let run ~env: _ ~domain_count: _ (gc_gen_cap : Nixtamal.Silo.Generation.t) =
		if Nixtamal.Setting.gc_enabled () then
			begin
				Logs.warn (fun m -> m "GC is experimental & behavior can actively change. Use with caution.");
				Nixtamal.garbage_collect ~gc_gen_cap ()
				|> exit_of_result
			end
		else
			begin
				Logs.err (fun m -> m "GC is experimental. Set NIXTAMAL_EXPERIMENTAL_GC to enable it.");
				1
			end

	let term ~env =
		let eio_env = env in
		let open Cmdliner in
		let generations_arg =
			let env = prefixed_env_info "GC_GENERATIONS" in
			Arg.(
				value
				& opt
					(
						Arg.Conv.make
							~docv: "INT"
							~parser: (fun s ->
								match int_of_string_opt s with
								| Some i -> Ok (Nixtamal.Silo.Generation.make i)
								| None -> Error "invalid int"
							)
							~pp: Nixtamal.Silo.Generation.pp
							()
					)
					(Nixtamal.Silo.Generation.make 2)
				& info
					["generations"]
					~env
					~doc: "Number of generations to keep (default: 2). Set to 0 to keep all, 1 to keep only current. Can also be set via NIXTAMAL_GC_GENERATIONS."
			)
		in
		Term.(
			const (fun glb -> Global.run ~env: eio_env glb @@ run)
			$ Global.args
			$ generations_arg
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end

module Upgrade = struct
	let info =
		Cmdliner.Cmd.info
			"upgrade"
			~doc: "Upgrade lockfile & manifest to current schema version."
			~exits: Error.exits
			~man: common_man

	let run ~env ~domain_count from to_ dry_run : int =
		let open Nixtamal in
		let lockfile_was_missing = not (Lockfile.exists ()) in
		let latest = Lazy.(force Schema.Version.latest) in
		let upgrading_to_latest = Option.value to_ ~default: latest = latest in
		let regen_result =
			match upgrade ?from ?to_ ~dry_run () with
			| Error err -> Error err
			| Ok() ->
				if lockfile_was_missing && upgrading_to_latest && not dry_run then
					begin
						if prompt_regenerate_lockfile ~env () then
							lock ~env ~domain_count ~use_tui: false ()
						else
							begin
								Logs.app (fun m -> m "Lockfile is missing. Run `nixtamal lock` to regenerate it.");
								Ok ()
							end
					end
				else
					Ok ()
		in
		exit_of_result regen_result

	let term ~env =
		let eio_env = env in
		let open Cmdliner in
		let version_conv : Nixtamal.Schema.Version.t Arg.conv =
			Arg.Conv.make
				~docv: "VERSION"
				~parser: (fun s ->
					match Nixtamal.Schema.Version.of_string s with
					| Some v -> Ok v
					| None ->
						Error (Fmt.str "Invalid version %s. Valid versions: %a" s Fmt.(array ~sep: (any ", ") Nixtamal.Schema.Version.pp) Lazy.(force Nixtamal.Schema.Version.versions))
				)
				~pp: Nixtamal.Schema.Version.pp
				()
		in
		let from_arg =
			let doc = "Upgrade from a specific version. Must match the lockfile version." in
			Arg.(
				value
				& opt (some version_conv) None
				& info ["from"] ~doc ~docv: "VERSION"
			)
		and to_arg =
			let doc = "Upgrade to a specific version. Must be newer than --from." in
			Arg.(
				value
				& opt (some version_conv) None
				& info ["to"] ~doc ~docv: "VERSION"
			)
		and dry_run_arg =
			Arg.(
				value
				& flag
				& info ["dry-run"] ~doc: "Show what would be upgraded without making changes (basic)."
			)
		in
		Term.(
			const (fun glb -> Global.run ~env: eio_env glb @@ run)
			$ Global.args
			$ from_arg
			$ to_arg
			$ dry_run_arg
		)

	let cmd ~env = Cmdliner.Cmd.v info (term ~env)
end
