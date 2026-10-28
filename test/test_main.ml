(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
let () =
	Alcotest.run "Nixtamal" [
		"Input", Test_input.suite;
		"Silo", Test_silo.suite;
		"Lockfile", Test_lockfile.suite;
		"Lock loader", Test_lock_loader.suite;
		"Upgrade", Test_upgrade.suite;
		"VCS ignore", Test_VCS.tests;
		"Blueprint", Test_blueprint.tests;
		"Version", Test_version.suite;
		"Working directory", Test_working_directory.suite;
	]
