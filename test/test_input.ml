(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
open Alcotest
open Nixtamal

let store_name_suite =
	let derived_store_name = Name.Store.make "toast.al-webmanifest-src"
	and manifest_store_name = Name.Store.make "twm-mnfst"
	and lockfile_store_name = Name.Store.make "twm-lock"
	in
	let name = Name.Input.make "toast.al-webmanifest"
	and url = "https://toast.al/manifest.webmanifest"
	and algorithm = Input.Hash.SHA_256
	in
	let manifest_basis : Manifest.Input'.t =
		let file = Manifest.File.make ~url: (Manifest.Template.make url) ()
		and hash = Manifest.Hash.make ~algorithm ()
		in
		Manifest.Input'.make
			~name
			~kind: (`File file)
			~hash
			~fresh_cmd: None
			()
	and lockfile_basis : Lockfile.Input'.t =
		let file = Lockfile.File.make ~url: (Result.get_ok (Locator.parse url)) ~fetch_time: Lockfile.Fetch_time.Eval ()
		and hash =
			Lockfile.Hash.make
				~algorithm
				~value: "sha256-kbfC9lDuXE5wadPgcKg7LuYDwUdvzDvOI421rAkLFFw="
				()
		in
		Lockfile.Input'.make
			~kind: (`File file)
			~hash
			~fresh_value: "275cc6852ea7ed739f404ac1aa6a3566840bbae9"
			()
	in
	[
		test_case "Store name: derive when no lock" `Quick (fun () ->
			let input : Input.t =
				Manifest.Input'.of_manifest manifest_basis
			in
			let input = {input with store_name = Input.derive_store_name input.name} in
			check
				(Alcotest.option (testable Name.Store.pp Name.Store.equal))
				"store_name derived"
				(Some derived_store_name)
				input.store_name
		);
		test_case "Store name: derive when lock sn null" `Quick (fun () ->
			let input : Input.t =
				Nixtamal.meld_input_with_lock
					(Manifest.Input'.of_manifest manifest_basis)
					lockfile_basis
				|> Result.get_ok
			in
			check
				(Alcotest.option (testable Name.Store.pp Name.Store.equal))
				"store_name derived"
				(Some derived_store_name)
				input.store_name
		);
		test_case "Store name: use lock store when exists & missing from manifest" `Quick (fun () ->
			let input : Input.t =
				Nixtamal.meld_input_with_lock
					(Manifest.Input'.of_manifest manifest_basis)
					{lockfile_basis with store_name = Some lockfile_store_name}
				|> Result.get_ok
			in
			check
				(Alcotest.option (testable Name.Store.pp Name.Store.equal))
				"store_name of lockfile"
				(Some lockfile_store_name)
				input.store_name
		);
		test_case "Store name: override with manifest" `Quick (fun () ->
			let input : Input.t =
				Nixtamal.meld_input_with_lock
					(Manifest.Input'.of_manifest {manifest_basis with store_name = Some manifest_store_name})
					{lockfile_basis with store_name = Some lockfile_store_name}
				|> Result.get_ok
			in
			check
				(Alcotest.option (testable Name.Store.pp Name.Store.equal))
				"store_name derived"
				(Some manifest_store_name)
				input.store_name
		);
	]

let parse_channel channel =
	match Input.Nixpkgs.Channel.of_string channel with
	| Some channel ->
		channel
	| None ->
		Alcotest.fail (Fmt.str "expected a valid Nixpkgs channel, got %S" channel)

let nixpkgs_suite =
	let module Channel = Input.Nixpkgs.Channel in
	let check_parse channel expected =
		let parsed = parse_channel channel in
		check
			string
			(Fmt.str "Channel.to_string %S" channel)
			expected
			(Channel.to_string parsed)
	in
	let check_release (channel, extension, url, mirror) =
		let parsed = parse_channel channel in
		check
			string
			(Fmt.str "Channel.extension %S" channel)
			extension
			(Channel.extension parsed);
		check
			string
			(Fmt.str "Channel.release_URL %S" channel)
			url
			(Channel.release_URL parsed |> Input.Template.take);
		check
			string
			(Fmt.str "Channel.mirror_TUNA_URL %S" channel)
			mirror
			(Channel.mirror_TUNA_URL parsed |> Input.Template.take)
	in
	[
		test_case "Nixpkgs channels" `Quick (fun () ->
			List.iter
				(fun (channel, expected) -> check_parse channel expected)
				[
					("nixos-unstable", "nixos-unstable");
					("nixos-unstable-small", "nixos-unstable-small");
					("nixos-25.11", "nixos-25.11");
					("nixos-25.11-small", "nixos-25.11-small");
					("nixos-26.05", "nixos-26.05");
					("nixos-26.5", "nixos-26.05");
					("nixos-026.05", "nixos-26.05");
					("nixos-26.005", "nixos-26.05");
					("nixos-5.10", "nixos-5.10");
					("nixpkgs-unstable", "nixpkgs-unstable");
					("nixpkgs-25.11-darwin", "nixpkgs-25.11-darwin");
					("nixpkgs-26.05-darwin", "nixpkgs-26.05-darwin");
					("nixpkgs-26.5-darwin", "nixpkgs-26.05-darwin");
				];
			List.iter
				(fun channel ->
					check
						bool
						(Fmt.str "Channel.of_string %S" channel)
						false
						(Option.is_some (Channel.of_string channel))
				)
				[
					"nixos-";
					"nixpkgs-";
					"nixos-foo";
					"nixpkgs-25.11";
					"nixpkgs-unstable-darwin";
					"nixos-25.11-darwin";
					"nixpkgs-25.11-small";
					"nixos-25.11.1";
					"nixos--small";
					"refs/heads/nixos-25.11";
					"nixos-25.";
					"nixos-.05";
				]
		);
		test_case "Nixpkgs channel releases" `Quick (fun () ->
			List.iter
				check_release
				[
					(
						"nixos-unstable",
						"tar.zst",
						"https://releases.nixos.org/nixos/unstable/{{fresh_value}}/nixexprs.tar.zst",
						"https://mirrors.tuna.tsinghua.edu.cn/nix-channels/releases/nixos-unstable%40{{fresh_value}}/nixexprs.tar.zst"
					);
					(
						"nixos-unstable-small",
						"tar.zst",
						"https://releases.nixos.org/nixos/unstable-small/{{fresh_value}}/nixexprs.tar.zst",
						"https://mirrors.tuna.tsinghua.edu.cn/nix-channels/releases/nixos-unstable-small%40{{fresh_value}}/nixexprs.tar.zst"
					);
					(
						"nixos-25.11",
						"tar.xz",
						"https://releases.nixos.org/nixos/25.11/{{fresh_value}}/nixexprs.tar.xz",
						"https://mirrors.tuna.tsinghua.edu.cn/nix-channels/releases/nixos-25.11%40{{fresh_value}}/nixexprs.tar.xz"
					);
					(
						"nixos-25.11-small",
						"tar.xz",
						"https://releases.nixos.org/nixos/25.11-small/{{fresh_value}}/nixexprs.tar.xz",
						"https://mirrors.tuna.tsinghua.edu.cn/nix-channels/releases/nixos-25.11-small%40{{fresh_value}}/nixexprs.tar.xz"
					);
					(
						"nixos-26.5",
						"tar.zst",
						"https://releases.nixos.org/nixos/26.05/{{fresh_value}}/nixexprs.tar.zst",
						"https://mirrors.tuna.tsinghua.edu.cn/nix-channels/releases/nixos-26.05%40{{fresh_value}}/nixexprs.tar.zst"
					);
					(
						"nixpkgs-unstable",
						"tar.zst",
						"https://releases.nixos.org/nixpkgs/{{fresh_value}}/nixexprs.tar.zst",
						"https://mirrors.tuna.tsinghua.edu.cn/nix-channels/releases/nixpkgs-unstable%40{{fresh_value}}/nixexprs.tar.zst"
					);
					(
						"nixpkgs-25.11-darwin",
						"tar.xz",
						"https://releases.nixos.org/nixpkgs/25.11-darwin/{{fresh_value}}/nixexprs.tar.xz",
						"https://mirrors.tuna.tsinghua.edu.cn/nix-channels/releases/nixpkgs-25.11-darwin%40{{fresh_value}}/nixexprs.tar.xz"
					);
					(
						"nixpkgs-26.5-darwin",
						"tar.zst",
						"https://releases.nixos.org/nixpkgs/26.05-darwin/{{fresh_value}}/nixexprs.tar.zst",
						"https://mirrors.tuna.tsinghua.edu.cn/nix-channels/releases/nixpkgs-26.05-darwin%40{{fresh_value}}/nixexprs.tar.zst"
					);
				]
		);
		test_case "Nixpkgs channel fresh pipeline" `Quick (fun () ->
			let fresh = Channel.fresh (parse_channel "nixos-26.5") () in
			match fresh.cmd with
			| None ->
				Alcotest.fail "expected fresh command"
			| Some (curl, rest) ->
				let progs =
					curl :: rest
					|> List.map (fun (cmd : Input.Fresh.Cmd.cmd) ->
							Input.Template.take cmd.prog
						)
				in
				check (list string) "fresh pipeline" ["curl"; "awk"] progs;
				begin
					match List.rev curl.args with
					| last :: _ ->
						check
							string
							"fresh URL"
							"https://channels.nixos.org/nixos-26.05"
							(Input.Template.take last)
					| [] ->
						Alcotest.fail "expected curl arguments"
				end
		);
		test_case "Nixpkgs channels with mirror" `Quick (fun () ->
			let channel = parse_channel "nixos-26.5" in
			let input =
				Input.Nixpkgs.make_channels
					~channel
					~mirrors: [Channel.mirror_TUNA_URL channel]
					()
			in
			match input.kind with
			| `Archive archive ->
				check
					string
					"release URL"
					"https://releases.nixos.org/nixos/26.05/{{fresh_value}}/nixexprs.tar.zst"
					(Input.Template.take archive.url);
				check (option string) "extension" (Some "tar.zst") archive.extension;
				check
					(list string)
					"mirrors"
					["https://mirrors.tuna.tsinghua.edu.cn/nix-channels/releases/nixos-26.05%40{{fresh_value}}/nixexprs.tar.zst"]
					(List.map Input.Template.take archive.mirrors)
			| _ ->
				Alcotest.fail "expected archive input"
		);
	]

let suite =
	[test_case "Manifest fresh-cmd to KDL" `Quick (fun () ->
		let kdl = testable KDL.pp KDL.equal in
		let open Nixtamal.Input.Fresh.Cmd in
		let t = Input.Template.make in
		let in_kdl =
			~${prog = t "curl"; args = [t "https://toast.al"]}
			|: {prog = t "head"; args = [t "-n1"]}
			|> Option.some
			|> Manifest.Fresh_cmd.codec.to_kdl
		in
		let out_kdl =
			{|
				fresh-cmd {
					$ curl "https://toast.al"
					| head -n1
				}
			|}
			|> KDL.of_string
			|> Result.get_ok
		in
		check kdl "KDL fresh-cmd with pipe" out_kdl in_kdl
	);
	test_case "Manifest fresh-cmd of KDL" `Quick (fun () ->
		let fresh_cmd = testable Manifest.Fresh_cmd.pp Manifest.Fresh_cmd.equal in
		let open Nixtamal.Input.Fresh.Cmd in
		let t = Input.Template.make in
		let in_fresh_cmd =
			let kdl =
				{|
					fresh-cmd {
						$ curl "https://toast.al"
						| head -n1
					}
				|}
				|> KDL.of_string
				|> Result.get_ok
			in
			match Manifest.Fresh_cmd.codec.of_kdl kdl with
			| Ok lc -> lc
			| Error err -> failwith Fmt.(str "%a from %a" (list ~sep: semi KDL.Valid.pp_err) err KDL.pp kdl)
		in
		let out_fresh_cmd =
			~${prog = t "curl"; args = [t "https://toast.al"]}
			|: {prog = t "head"; args = [t "-n1"]}
			|> Option.some
		in
		check fresh_cmd "fresh-cmd with pipe from KDL" out_fresh_cmd in_fresh_cmd
	);
	test_case "Manifest named frozen Pijul to KDL" `Quick (fun () ->
		let kdl = testable KDL.pp KDL.equal in
		let t = Input.Template.make in
		let store_name = Name.Store.make "pijul-src" in
		let in_kdl =
			let name = Name.Input.make "pijul"
			and kind = `Pijul (Manifest.Pijul.make ~remote: (t "https://nest.pijul.com/pijul/pijul") ~reference: (`Channel "main") ())
			and hash = Manifest.Hash.make ()
			in
			Manifest.Input'.make ~name ~store_name ~kind ~hash ~fresh_cmd: None ~frozen: true ()
			|> Manifest.Input'.codec.to_node
		in
		let out_kdl =
			{|
				pijul name=pijul-src frozen=#true {
					pijul {
						remote "https://nest.pijul.com/pijul/pijul"
						channel main
					}
				}
			|}
			|> KDL.of_string
			|> Result.get_ok
		in
		check kdl "KDL named frozen Pijul" out_kdl [in_kdl]
	);
	test_case "Manifest named frozen Pijul of KDL" `Quick (fun () ->
		let input = testable Manifest.Input'.pp Manifest.Input'.equal in
		let open KDL.L in
		let open KDL.Valid in
		let t = Input.Template.make in
		let in_input =
			let kdl =
				{|
					pijul name=pijul-src frozen=#true {
						pijul {
							remote "https://nest.pijul.com/pijul/pijul"
							channel main
						}
					}
				|}
				|> KDL.of_string
				|> Result.get_ok
			in
			let node = ll @@ kdl.@(node "pijul" ~nth: 0) in
			match Result.bind node Manifest.Input'.codec.of_node with
			| Ok lc -> lc
			| Error err -> failwith Fmt.(str "%a from %a" (list ~sep: semi KDL.Valid.pp_err) err KDL.pp kdl)
		in
		let out_input =
			let name = Name.Input.make "pijul"
			and store_name = Name.Store.make "pijul-src"
			and kind = `Pijul (Manifest.Pijul.make ~remote: (t "https://nest.pijul.com/pijul/pijul") ~reference: (`Channel "main") ())
			and hash = Manifest.Hash.make ()
			in
			Manifest.Input'.make ~name ~store_name ~kind ~hash ~fresh_cmd: None ~frozen: true ()
		in
		check input "named frozen Pijul from KDL" out_input in_input
	);
	test_case "Manifest frozen Pijul sameshape" `Quick (fun () ->
		let input = testable Manifest.Input'.pp Manifest.Input'.equal in
		let t = Input.Template.make in
		let in_input =
			let name = Name.Input.make "pijul"
			and kind = `Pijul (Manifest.Pijul.make ~remote: (t "https://nest.pijul.com/pijul/pijul") ~reference: (`Channel "main") ())
			and hash = Manifest.Hash.make ()
			in
			Manifest.Input'.make ~name ~kind ~hash ~frozen: true ~fresh_cmd: None ()
		in
		let out_input =
			in_input
			|> Manifest.Input'.codec.to_node
			|> Manifest.Input'.codec.of_node
			|> Result.get_ok
		in
		check input "Frozen Pijul KDL roundrip" out_input in_input
	);
	test_case "Manifest named patch sameshape" `Quick (fun () ->
		let patch = testable Manifest.Patch.pp Manifest.Patch.equal in
		let t = Input.Template.make in
		let in_patch =
			let name = Name.Patch.make "h2o"
			and remote : Manifest.Patch.Source.remote = {
				url = t "https://example.com/h2o.patch";
				fresh_cmd = None;
				hash = Manifest.Hash.make ();
				frozen = false;
				gc = None;
				store_name = Some (Name.Store.make "h2o.patch");
				vendored = false;
			}
			in
			Manifest.Patch.make ~name ~source: (Manifest.Patch.Source.Remote remote)
		in
		let out_patch =
			in_patch
			|> Manifest.Patch.codec.to_node
			|> Manifest.Patch.codec.of_node
			|> Result.get_ok
		in
		check patch "Named patch KDL roundtrip" out_patch in_patch
	);
	test_case "Manifest vendored patch sameshape" `Quick (fun () ->
		let patch = testable Manifest.Patch.pp Manifest.Patch.equal in
		let t = Input.Template.make in
		let in_patch =
			let name = Name.Patch.make "h2o"
			and remote : Manifest.Patch.Source.remote = {
				url = t "https://example.com/h2o.patch";
				fresh_cmd = None;
				hash = Manifest.Hash.make ();
				frozen = false;
				gc = None;
				store_name = None;
				vendored = true;
			}
			in
			Manifest.Patch.make ~name ~source: (Manifest.Patch.Source.Remote remote)
		in
		let out_patch =
			in_patch
			|> Manifest.Patch.codec.to_node
			|> Manifest.Patch.codec.of_node
			|> Result.get_ok
		in
		check patch "Vendored patch KDL roundtrip" out_patch in_patch
	);
	test_case "Manifest default-vendor-patch" `Quick (fun () ->
		let t = Input.Template.make in
		let node =
			let name = Name.Patch.make "h2o"
			and remote : Manifest.Patch.Source.remote = {
				url = t "https://example.com/h2o.patch";
				fresh_cmd = None;
				hash = Manifest.Hash.make ();
				frozen = false;
				gc = None;
				store_name = None;
				vendored = false;
			}
			in
			Manifest.Patch.make ~name ~source: (Manifest.Patch.Source.Remote remote)
			|> Manifest.Patch.codec.to_node
		in
		let saved = !Manifest.Patch.Source.default_vendored in
		Fun.protect
			~finally: (fun () -> Manifest.Patch.Source.default_vendored := saved)
			(fun () ->
				Manifest.Patch.Source.default_vendored := true;
				match Manifest.Patch.codec.of_node node with
				| Ok patch ->
					begin
						match patch.source with
						| Manifest.Patch.Source.Remote remote ->
							check bool "default vendors the patch" true remote.vendored
						| Manifest.Patch.Source.Local _ ->
							Alcotest.fail "expected a remote patch"
					end
				| Error errs ->
					Alcotest.failf "expected Ok, got %a" (Fmt.list KDL.Valid.pp_err) errs
			)
	);
	test_case "Patch is_local_path detection" `Quick (fun () ->
		let check_local path expected =
			let result = Input.Patch.Source.is_local_path path in
			check Alcotest.bool (Fmt.str "is_local_path %S" path) expected result
		in
		(* Local paths *)
		check_local "./foo.patch" true;
		check_local "../foo.patch" true;
		check_local "file:./foo.patch" true;
		check_local "file:../foo.patch" true;
		(* Remote paths *)
		check_local "https://example.com/patch.diff" false;
		check_local "http://example.com/patch.diff" false;
		check_local "file:///absolute/path.patch" false;
		check_local "file:/relative.patch" false;
		(* You probably messed up & therefore not my problem *)
		check_local "file:./" true;
		check_local "file:../" true;
	);
	test_case "Patch strip_file_prefix" `Quick (fun () ->
		let check_strip input expected =
			let result = Input.Patch.strip_file_prefix input in
			check Alcotest.string (Fmt.str "strip_file_prefix %S" input) expected result
		in
		check_strip "./foo.patch" "./foo.patch";
		check_strip "../foo.patch" "../foo.patch";
		check_strip "file:./foo.patch" "./foo.patch";
		check_strip "file:../foo.patch" "../foo.patch";
		check_strip "https://example.com/patch.diff" "https://example.com/patch.diff";
	);
	] @
	nixpkgs_suite @
	store_name_suite @
	List.map QCheck_alcotest.to_alcotest [
		QCheck.Test.make
			~name: "Manifest input codec sameshape"
			(QCheck.make ~print: (Fmt.str "%a" Manifest.Input'.pp) Manifest.Input'.gen)
			(fun input ->
				let back_and_forth =
					input
					|> Manifest.Input'.codec.to_node
					|> Manifest.Input'.codec.of_node
				in
				match back_and_forth with
				| Ok input' when input' = input ->
					true
				| Error [`Eval_fetch_time_at_odds_with `Patches] when not (List.is_empty input.patches) ->
					begin
						match input.kind with
						| `File {fetch_time; _} | `Archive {fetch_time; _} | `Git {fetch_time; _} when fetch_time = Some Input.Fetch_time.Eval ->
							true
						| _ ->
							QCheck.Test.fail_reportf
								"Should fail with at odds with patch:@,%a@."
								Manifest.Input'.pp
								input
					end
				| Ok input' ->
					QCheck.Test.fail_reportf
						"Aimed for:@,%a@.@.But got:@,%a@."
						Manifest.Input'.pp
						input
						Manifest.Input'.pp
						input'
				| Error err ->
					QCheck.Test.fail_reportf "%a" Fmt.(list ~sep: semi KDL.Valid.pp_err) err;
			);
		QCheck.Test.make
			~name: "Patch source detection"
			(QCheck.make ~print: (Fmt.str "%a" Input.Patch.pp) Input.Patch.gen)
			(fun patch ->
				match patch.source with
				| Input.Patch.Source.Local path ->
					if not (Input.Patch.Source.is_local_path path) then
						QCheck.Test.fail_reportf "Local path %s should be detected as local" path
					else
						true
				| Input.Patch.Source.Remote {url; _} ->
					let models = Input.Patch.jg_models2 patch in
					let url_str = Input.Template.fill ~models url in
					if Input.Patch.Source.is_local_path url_str then
						QCheck.Test.fail_reportf "Remote URI %a should NOT be detected as local" Input.Template.pp url
					else
						true
			);
	]
