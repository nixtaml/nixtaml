(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
open Alcotest
open Nixtamal

let (let*) = Result.bind

let test_version_validation_valid =
	test_case "valid version strings" `Quick (fun () ->
		check
			(option string)
			"0.5.0 parses"
			(Some "0.5.0")
			(Schema.Version.(Option.map to_string (of_string "0.5.0")));
		check
			(option string)
			"1.0.0 parses"
			(Some "1.0.0")
			(Schema.Version.(Option.map to_string (of_string "1.0.0")));
		check
			(option string)
			"0.4.0 is invalid"
			None
			(Schema.Version.(Option.map to_string (of_string "0.4.0")));
	)

let test_lockfile_upgrade_v0_5_0_to_v1_0_0 =
	test_case "upgrade lockfile 0.5.0 to 1.0.0" `Quick (fun () ->
		let lockfile : Lockfile.t = {
			version = `v0_5_0;
			inputs = Name.InputMap.empty;
			patches = Name.PatchMap.empty
		}
		in
		match Lockfile.upgrade `v1_0_0 lockfile with
		| Error _ -> Alcotest.fail "Upgrade should not fail"
		| Ok upgraded ->
			check string "version upgraded" "1.0.0" (Schema.Version.to_string upgraded.Lockfile.version)
	)

let test_lockfile_upgrade_v1_0_0_to_v1_1_0 =
	test_case "upgrade lockfile V1.0.0 to V1.1.0 with store_name derivation" `Quick (fun () ->
		let nixpkgs : Lockfile.Input'.t = {
			store_name = None;
			kind =
			`Archive {
				fetch_time = Lockfile.Fetch_time.Eval;
				url = Result.get_ok (Locator.parse "https://example.com/nixpkgs.tar.gz");
				mirrors = [];
				extension = None;
			};
			hash = {algorithm = Input.Hash.SHA_256; value = Some "sha256-abc123"};
			fresh_value = Some "abc123";
			patches = [Name.Patch.make "h2o"]
		}
		in
		let h2o : Lockfile.Patch.t = {
			url = Result.get_ok (Locator.parse "https://example.com/h2o.patch");
			hash = {algorithm = Input.Hash.SHA_512; value = Some "sha512-xyz"};
			fresh_value = None;
			store_name = None;
		}
		in
		let lockfile : Lockfile.t = {
			version = `v1_0_0;
			inputs = Name.InputMap.add (Name.Input.make "nixpkgs") nixpkgs Name.InputMap.empty;
			patches = Name.PatchMap.add (Name.Patch.make "h2o") h2o Name.PatchMap.empty
		}
		in
		match Lockfile.upgrade `v1_1_0 lockfile with
		| Error _ -> Alcotest.fail "V1_0_0 → V1_1_0 migration failed"
		| Ok upgraded ->
			let input = Name.InputMap.find (Name.Input.make "nixpkgs") upgraded.inputs in
			check (option string) "nixpkgs store_name derived" (Some "nixpkgs-src") (Option.map Name.Store.take input.store_name)
	)

let test_lockfile_upgrade_v1_3_0_to_v1_4_0_derives_patch_store_name =
	test_case "upgrade lockfile V1.3.0 to V1.4.0 derives patch store_name" `Quick (fun () ->
		let h2o : Lockfile.Patch.t = {
			url = Result.get_ok (Locator.parse "https://example.com/h2o.patch");
			hash = {algorithm = Input.Hash.SHA_512; value = Some "sha512-xyz"};
			fresh_value = None;
			store_name = None;
		}
		in
		let lockfile : Lockfile.t = {
			version = `v1_3_0;
			inputs = Name.InputMap.empty;
			patches = Name.PatchMap.add (Name.Patch.make "h2o") h2o Name.PatchMap.empty
		}
		in
		match Lockfile.upgrade `v1_4_0 lockfile with
		| Error _ -> Alcotest.fail "V1_3_0 → V1_4_0 migration failed"
		| Ok upgraded ->
			let patch = Name.PatchMap.find (Name.Patch.make "h2o") upgraded.patches in
			check (option string) "h2o store_name derived" (Some "h2o-patch") (Option.map Name.Store.take patch.store_name);
			check (option string) "h2o fresh_value none" None patch.fresh_value
	)

let test_lockfile_upgrade_v0_5_0_to_current =
	test_case "upgrade lockfile 0.5.0 to current" `Quick (fun () ->
		let lockfile : Lockfile.t = {
			version = `v0_5_0;
			inputs = Name.InputMap.empty;
			patches = Name.PatchMap.empty
		}
		in
		match Lockfile.upgrade Lazy.(force Schema.Version.latest) lockfile with
		| Error _ -> Alcotest.fail "Upgrade should not fail"
		| Ok upgraded ->
			check string "version upgraded" (Schema.Version.to_string Lazy.(force Schema.Version.latest)) (Schema.Version.to_string upgraded.Lockfile.version)
	)

let test_manifest_upgrade =
	test_case "upgrade manifest content" `Quick (fun () ->
		let content =
			{|version "0.5.0"
inputs {
	nixpkgs archive "https://github.com/NixOS/nixpkgs/archive/3fbab70.tar.gz"
}
|}
		in
		match Manifest.upgrade content with
		| Error _ -> Alcotest.fail "Manifest upgrade failed"
		| Ok upgraded ->
			let contains_version = String.contains upgraded 'v' in
			check bool "manifest contains version" true contains_version;
			check bool "manifest not empty" true (String.length upgraded > 0)
	)

let test_manifest_version_parsing =
	test_case "manifest version detection" `Quick (fun () ->
		let content =
			{|// comment
version "1.0.0"
inputs {}
|}
		in
		let all_lines = String.split_on_char '\n' content in
		let rec find_version acc = function
			| [] -> None
			| line :: rest ->
				let trimmed = String.trim line in
				if String.starts_with ~prefix: "version" trimmed then
					Some (line, List.length (List.rev acc))
				else
					find_version (line :: acc) rest
		in
		match find_version [] all_lines with
		| None -> Alcotest.fail "Version line not found"
		| Some (version_line, line_num) ->
			check string "version line correct" "version \"1.0.0\"" version_line;
			check int "version on line 2" 2 (line_num + 1)
	)

let test_manifest_version_upgrade_needed =
	test_case "manifest upgrade detection" `Quick (fun () ->
		let manifest_content_0_5_0 =
			{|version "0.5.0"
inputs {}
|}
		in
		let manifest_content_1_0_0 =
			{|version "1.0.0"
inputs {}
|}
		in
		let detect_version content =
			let all_lines = String.split_on_char '\n' content in
			let rec find_version = function
				| [] -> None
				| line :: rest ->
					let trimmed = String.trim line in
					if String.starts_with ~prefix: "version" trimmed then
						(
							match String.index_opt trimmed '"' with
							| Some start ->
								let after_start = start + 1 in
								(
									match String.index_from_opt trimmed after_start '"' with
									| Some end_ -> Some (String.sub trimmed after_start (end_ - after_start))
									| None -> None
								)
							| None -> None
						)
					else find_version rest
			in
			find_version all_lines
		in
		let detected_0_5_0 = detect_version manifest_content_0_5_0 in
		let detected_1_0_0 = detect_version manifest_content_1_0_0 in
		check (option string) "0.5.0 detected" (Some "0.5.0") detected_0_5_0;
		check (option string) "1.0.0 detected" (Some "1.0.0") detected_1_0_0
	)

let suite = [
	test_version_validation_valid;
	test_lockfile_upgrade_v0_5_0_to_v1_0_0;
	test_lockfile_upgrade_v1_0_0_to_v1_1_0;
	test_lockfile_upgrade_v1_3_0_to_v1_4_0_derives_patch_store_name;
	test_lockfile_upgrade_v0_5_0_to_current;
	test_manifest_upgrade;
	test_manifest_version_parsing;
	test_manifest_version_upgrade_needed;
]
