(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
open Alcotest
open Nixtamal

let suite = [
	test_case "vendored filename keeps patch extension" `Quick (fun () ->
		let name = Name.Patch.make "fix" in
		check
			string
			".patch kept"
			"fix.patch"
			(Working_directory.vendored_filename name "https://example.com/fix.patch")
	);
	test_case "vendored filename keeps diff extension" `Quick (fun () ->
		let name = Name.Patch.make "fix" in
		check
			string
			".diff kept"
			"fix.diff"
			(Working_directory.vendored_filename name "https://example.com/fix.diff")
	);
	test_case "vendored filename defaults without extension" `Quick (fun () ->
		let name = Name.Patch.make "fix" in
		check
			string
			".patch fallback"
			"fix.patch"
			(Working_directory.vendored_filename name "https://example.com/fix")
	);
	test_case "vendored filename strips query strings" `Quick (fun () ->
		let name = Name.Patch.make "fix" in
		check
			string
			"query stripped"
			"fix.diff"
			(Working_directory.vendored_filename name "https://example.com/fix.diff?raw=true")
	);
	test_case "vendored relpath" `Quick (fun () ->
		let name = Name.Patch.make "fix" in
		check
			string
			"tamal-relative path"
			"./vendored_patches/fix.patch"
			(Working_directory.vendored_relpath name "https://example.com/fix.patch")
	);
]
