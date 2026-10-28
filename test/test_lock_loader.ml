(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
(* Render the lock loader with every feature enabled & prove the result is
	parseable Nix. If `nix-instantiate` is unavailable we warn & skip instead
   of failing, so the suite stays green outside a Nix shell. *)
open Alcotest
open Nixtamal

(* The `local-patches` template branch reads the global Foreman table, so seed
   a uniquely-named local patch. Errors are ignored: the patch may already be
   present & any pre-existing state only widens coverage. *)
let seed_local_patch () =
	let name = Name.Patch.make "foo-bar"
	and source = Input.Patch.Source.Local "./patches/foo-bar-fix.patch"
	in
	let patch = Input.Patch.make ~name ~source in
	ignore @@ Foreman.add_patch name patch

let nix_instantiate_found ~proc_mgr ~proc_env =
	match Eio.Process.run
		proc_mgr
		~env: proc_env
		~stdout: Eio.Flow.null
		~stderr: Eio.Flow.null
		["nix-instantiate"; "--version"] with
	| () -> true
	| exception Eio.Io (Eio.Process.E (Eio.Process.Executable_not_found _), _) -> false

let check_parse ~env rendered =
	let proc_mgr = Eio.Stdenv.process_mgr env in
	let dir = Eio.Path.(Test_util.tmp_dir ~env / "lock_loader_full") in
	Eio.Path.mkdirs dir ~exists_ok: true ~perm: 0o755;
	Fun.protect
		~finally: (fun () -> Eio.Path.rmtree dir)
		(fun () ->
			(* `nix-instantiate` wants a writable state dir (profiles, temproots…)
			   & defaults to `/nix/var/nix`, which is read-only inside a Nix
			   sandbox; redirect it too under our tmpdir. *)
			let proc_env =
				Eio.Process.Env.override
					["NIX_STATE_DIR", Some (Eio.Path.native_exn Eio.Path.(dir / "nix-state"))]
					(Eio.Process.environment proc_mgr)
			in
			if not (nix_instantiate_found ~proc_mgr ~proc_env) then
				(
					prerr_endline "WARNING: “nix-instantiate” not found on PATH; skipping Nix parse check";
					Alcotest.skip ()
				)
			else
				(
					let version = Lazy.(force Schema.Version.latest) in
					let empty_lockfile = Lockfile.make ~version () in
					let lock_json = Fmt.str "%a" Lockfile.pp empty_lockfile in
					Eio.Path.save ~create: (`Or_truncate 0o644) Eio.Path.(dir / "default.nix") rendered;
					Eio.Path.save ~create: (`Or_truncate 0o644) Eio.Path.(dir / Lockfile.filename) lock_json;
					let stderr_buf = Buffer.create 1024 in
					let stderr_sink = Eio.Flow.buffer_sink stderr_buf in
					let file = Eio.Path.native_exn Eio.Path.(dir / "default.nix") in
					begin
						try
							Eio.Process.run
								proc_mgr
								~env: proc_env
								~stdout: Eio.Flow.null
								~stderr: stderr_sink
								["nix-instantiate"; "--parse"; file];
						with
							| Eio.Io (Eio.Process.E (Eio.Process.Child_error _), _) ->
								Alcotest.failf
									"“nix-instantiate --parse” failed: %s"
									(String.trim (Buffer.contents stderr_buf))
							| exn ->
								Alcotest.failf
									"“nix-instantiate --parse” errored: %s"
									(Printexc.to_string exn)
					end
				)
		)

let test_full_loader_parses =
	test_case "full loader parses" `Slow (fun () ->
		Eio_main.run @@ fun env ->
		seed_local_patch ();
		let version = Lazy.(force Schema.Version.latest)
		and features = Features.all_features
		in
		let rendered = Lock_loader.render_jingoo_template ~version ~features () in
		check_parse ~env rendered
	)

let suite = [
	test_full_loader_parses;
]
