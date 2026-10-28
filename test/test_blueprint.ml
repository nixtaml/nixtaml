(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
open Nixtamal

let sameshape =
	QCheck_alcotest.to_alcotest @@
		QCheck.Test.make
			~name: "sameshape"
			QCheck.(make ~print: (Fmt.str "%a" Blueprint.pp) Blueprint.gen)
			(fun t ->
				let s = Blueprint.to_string t in
				match Blueprint.of_string s with
				| Ok t' when t' = t -> true
				| Ok t' -> QCheck.Test.fail_reportf "Aimed for %a, got %a" Blueprint.pp t Blueprint.pp t'
				| Error e -> QCheck.Test.fail_reportf "of_string returned Error for %s: %s" s e
			)

let tests = [
	sameshape;
]
