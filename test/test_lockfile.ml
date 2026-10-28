(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
open Alcotest
open Nixtamal

let (let*) = Result.bind

let test_empty_lockfile =
	test_case "0 inputs & 0 patches" `Quick (fun () ->
		let lockfile : Lockfile.t = {
			version = `v0_5_0;
			inputs = Name.InputMap.empty;
			patches = Name.PatchMap.empty
		}
		in
		let result = Fmt.str "%a" Lockfile.pp lockfile in
		let expected = "{\"v\":\"0.5.0\"\n,\"i\":{}\n,\"p\":{}\n}" in
		check string "empty lockfile" expected result
	)

let test_full_lockfile =
	test_case "2 inputs & 2 patches" `Quick (fun () ->
		let open Lockfile in
		let nixpkgs : Input'.t = {
			store_name = None;
			kind =
			`Archive {
				fetch_time = Fetch_time.Eval;
				url = Result.get_ok (Locator.parse "https://github.com/NixOS/nixpkgs/archive/3fbab70.tar.gz");
				mirrors = [];
				extension = None;
			};
			hash = {algorithm = Input.Hash.SHA_256; value = Some "sha256-FOO"};
			fresh_value = Some "3fbab70c6e69c87ea2b6e48aa6629da2aa6a23b0";
			patches = [Name.Patch.make "h2o"]
		}
		and test_tarball : Input'.t = {
			store_name = None;
			kind =
			`File {
				fetch_time = Fetch_time.Build;
				url = Result.get_ok (Locator.parse "https://example.com/file.tar.gz");
				mirrors = [];
			};
			hash = {algorithm = Input.Hash.SHA_256; value = Some "sha256-BAR"};
			fresh_value = Some "9b9a89c5de55a0f60cbd13b29fded76e34448e68";
			patches = []
		}
		and h2o : Patch.t = {
			url = Result.get_ok (Locator.parse "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/477349.patch");
			hash = {algorithm = Input.Hash.SHA_512; value = Some "sha512-BAZ"};
			fresh_value = None;
			store_name = None;
		}
		and qux : Patch.t = {
			url = Result.get_ok (Locator.parse "https://example.com/patch2.patch");
			hash = {algorithm = Input.Hash.BLAKE3; value = Some "blake3-QUX"};
			fresh_value = None;
			store_name = None;
		}
		in
		let inputs = Name.InputMap.(empty |> add (Name.Input.make "nixpkgs") nixpkgs |> add (Name.Input.make "dash-nix") test_tarball)
		and patches = Name.PatchMap.(empty |> add (Name.Patch.make "h2o") h2o |> add (Name.Patch.make "qux") qux)
		in
		let lockfile : Lockfile.t = {version = `v0_5_0; inputs; patches} in
		let result = Fmt.str "%a" Lockfile.pp lockfile in
		let expected =
			"{\"v\":\"0.5.0\"\n,\"i\":{\n\"nixpkgs\":{\"kd\":[1,{\"ft\":1,\"ur\":\"https://github.com/NixOS/nixpkgs/archive/3fbab70.tar.gz\",\"ms\":[]}],\"ha\":{\"al\":0,\"vl\":\"sha256-FOO\"},\"fv\":\"3fbab70c6e69c87ea2b6e48aa6629da2aa6a23b0\",\"ps\":[\"h2o\"]}\n,\"dash-nix\":{\"kd\":[0,{\"ft\":0,\"ur\":\"https://example.com/file.tar.gz\",\"ms\":[]}],\"ha\":{\"al\":0,\"vl\":\"sha256-BAR\"},\"fv\":\"9b9a89c5de55a0f60cbd13b29fded76e34448e68\",\"ps\":[]}\n}\n,\"p\":{\n\"h2o\":{\"ur\":\"https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/477349.patch\",\"ha\":{\"al\":1,\"vl\":\"sha512-BAZ\"}}\n,\"qux\":{\"ur\":\"https://example.com/patch2.patch\",\"ha\":{\"al\":2,\"vl\":\"blake3-QUX\"}}\n}\n}"
		in
		check string "full lockfile" expected result
	)

let test_local_patches_in_default_nix =
	test_case "Local patches in default.nix" `Quick (fun () ->
		let kdl_doc =
			{|
				version "0.5.0"
				inputs {
					nixpkgs {
						archive {
							url "https://github.com/NixOS/nixpkgs/archive/3fbab70.tar.gz"
						}
						hash algorithm=SHA-256
						patches my-fix other-fix
					}
				}
				patches {
					my-fix "./patches/fix1.patch"
					other-fix "../shared/fix2.patch"
				}
			|}
			|> KDL.of_string
			|> Result.get_ok
		and version = `v0_5_0
		and features =
			Features.(empty |> add Flag.patches |> add Flag.archive)
		in
		let manifest = Manifest.document_to_t kdl_doc |> Result.get_ok in
		let dummy_lockfile : Lockfile.t = {
			version;
			inputs = Name.InputMap.empty;
			patches = Name.PatchMap.empty
		}
		in
		List.iter
			(fun manifest_patch ->
				let name = manifest_patch.Manifest.Patch.name in
				let input_patch = Manifest.Patch.of_manifest manifest_patch in
				match Foreman.add_patch name input_patch with
				| Error _ -> failwith (Fmt.str "Failed to add patch %a" Name.Patch.pp name)
				| Ok() -> ()
			)
			manifest.Manifest.patches;
		let result = Lock_loader.render_jingoo_template ~version ~features () in
		let contains_sub ~haystack ~needle =
			let rec loop i =
				if i + String.length needle > String.length haystack then
					false
				else if String.sub haystack i (String.length needle) = needle then
					true
				else
					loop (i + 1)
			in
			loop 0
		in
		check bool "contains my-fix" true (contains_sub ~haystack: result ~needle: "my-fix");
		check bool "contains other-fix" true (contains_sub ~haystack: result ~needle: "other-fix");
		check bool "contains ./patches/fix1.patch" true (contains_sub ~haystack: result ~needle: "./patches/fix1.patch");
		check bool "contains ../shared/fix2.patch" true (contains_sub ~haystack: result ~needle: "../shared/fix2.patch");
		check bool "input patches ps in output" true (contains_sub ~haystack: result ~needle: "input.ps");
		check int "lockfile empty patches" 0 (Name.PatchMap.cardinal dummy_lockfile.Lockfile.patches)
	)

let test_lock_subset_narrows =
	test_case "lock variant narrows to input" `Quick (fun () ->
		let open Lockfile in
		let nixpkgs : Input'.t = {
			store_name = None;
			kind =
			`Archive {
				fetch_time = Fetch_time.Eval;
				url = Result.get_ok (Locator.parse "https://github.com/NixOS/nixpkgs/archive/3fbab70.tar.gz");
				mirrors = [];
				extension = None;
			};
			hash = {algorithm = Input.Hash.SHA_256; value = Some "sha256-FOO"};
			fresh_value = Some "3fbab70c6e69c87ea2b6e48aa6629da2aa6a23b0";
			patches = [Name.Patch.make "h2o"; Name.Patch.make "qux"];
		}
		and dash : Input'.t = {
			store_name = None;
			kind =
			`File {
				fetch_time = Fetch_time.Build;
				url = Result.get_ok (Locator.parse "https://example.com/file.tar.gz");
				mirrors = [];
			};
			hash = {algorithm = Input.Hash.SHA_256; value = Some "sha256-BAR"};
			fresh_value = None;
			patches = [];
		}
		and h2o : Patch.t = {
			url = Result.get_ok (Locator.parse "https://example.com/h2o.patch");
			hash = {algorithm = Input.Hash.SHA_256; value = Some "sha256-H2O"};
			fresh_value = None;
			store_name = None;
		}
		and qux : Patch.t = {
			url = Result.get_ok (Locator.parse "https://example.com/qux.patch");
			hash = {algorithm = Input.Hash.SHA_256; value = Some "sha256-QUX"};
			fresh_value = None;
			store_name = None;
		}
		in
		let lock : Lockfile.t = {
			version = `v1_5_0;
			inputs = Name.InputMap.(empty |> add (Name.Input.make "nixpkgs") nixpkgs |> add (Name.Input.make "dash") dash);
			patches = Name.PatchMap.(empty |> add (Name.Patch.make "h2o") h2o |> add (Name.Patch.make "qux") qux);
		}
		in
		match Lockfile.lock_subset lock (Name.Input.make "nixpkgs") [Name.Patch.make "h2o"] with
		| None -> fail "variant missing nixpkgs input"
		| Some variant ->
			check
				(list string)
				"only target input kept"
				["nixpkgs"]
				(List.map (fun (n, _) -> Name.Input.take n) (Name.InputMap.bindings variant.Lockfile.inputs));
			check
				(list string)
				"only prefix patches kept"
				["h2o"]
				(List.map (fun (n, _) -> Name.Patch.take n) (Name.PatchMap.bindings variant.Lockfile.patches));
			let entry = Name.InputMap.find (Name.Input.make "nixpkgs") variant.Lockfile.inputs in
			check
				(list string)
				"ps truncated to prefix"
				["h2o"]
				(List.map Name.Patch.take entry.Lockfile.Input'.patches)
	)

let suite = [
	test_empty_lockfile;
	test_full_lockfile;
	test_local_patches_in_default_nix;
	test_lock_subset_narrows;
]
