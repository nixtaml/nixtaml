(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
open Alcotest
open Nixtamal

let latest_version_string =
	Schema.Version.to_string Lazy.(force Schema.Version.latest)

(* These global refs are shared across the whole test process; save & restore. *)
let with_working_dir ~env test_name f =
	let dir = Eio.Path.(Test_util.tmp_dir ~env / test_name) in
	Eio.Path.mkdirs dir ~exists_ok: true ~perm: 0o755;
	let saved_working_directory = !Working_directory.working_directory
	and saved_lockfile = !Lockfile.lockfile
	and saved_manifest = !Manifest.manifest
	in
	Fun.protect
		~finally: (fun () ->
			Eio.Path.rmtree dir;
			Working_directory.working_directory := saved_working_directory;
			Lockfile.lockfile := saved_lockfile;
			Manifest.manifest := saved_manifest
		)
		(fun () ->
			Working_directory.set ~directory: dir;
			Lockfile.lockfile := None;
			Manifest.manifest := None;
			f dir
		)

let test_check_version_with_missing_lockfile =
	test_case "check-version passes when lockfile is missing" `Quick (fun () ->
		Eio_main.run @@ fun env ->
		with_working_dir ~env "version_check_missing_lockfile" @@ fun dir ->
		let manifest_path = Eio.Path.(dir / Manifest.filename) in
		Eio.Path.save
			~create: (`Or_truncate 0o644)
			manifest_path
			(Fmt.str {|version "%s"|} latest_version_string);
		match Nixtamal.check_version_only () with
		| Ok() -> ()
		| Error err -> Alcotest.failf "expected Ok, got %a" Error.pp err
	)

let test_upgrade_with_missing_lockfile =
	test_case "upgrade works when lockfile is missing" `Quick (fun () ->
		Eio_main.run @@ fun env ->
		with_working_dir ~env "upgrade_missing_lockfile" @@ fun dir ->
		let manifest_path = Eio.Path.(dir / Manifest.filename)
		and lockfile_path = Eio.Path.(dir / Lockfile.filename)
		in
		Eio.Path.save
			~create: (`Or_truncate 0o644)
			manifest_path
			{|version "0.5.0"
inputs {}|};
		match Nixtamal.upgrade () with
		| Ok() ->
			let content = Eio.Path.load manifest_path in
			check
				string
				"manifest upgraded to latest"
				(
					Fmt.str
						{|version "%s"
inputs {}|}
						latest_version_string
				)
				content;
			check bool "lockfile not created" false (Eio.Path.is_file lockfile_path)
		| Error err -> Alcotest.failf "expected Ok, got %a" Error.pp err
	)

let suite = [
	test_check_version_with_missing_lockfile;
	test_upgrade_with_missing_lockfile;
]
