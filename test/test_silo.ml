(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
open Nixtamal
open Silo

let gen_test =
	QCheck_alcotest.to_alcotest @@
		QCheck.Test.make ~name: "Generation sameshape" Generation.gen (fun gen ->
			let s = Generation.to_string gen in
			match Generation.of_string s with
			| Some gen' when gen' = gen -> true
			| Some gen' -> QCheck.Test.fail_reportf "Aimed for %a, got %a" Generation.pp gen Generation.pp gen'
			| None -> QCheck.Test.fail_reportf "of_string returned None for %s" s
		)

let gc_tests =
	let test_cutoff ~gc_gen_cap ~gen_now =
		if gc_gen_cap > gen_now then Generation.min else Generation.(gen_now - gc_gen_cap)
	in
	[
		Alcotest.test_case "GC Cutoff Math/cap=2, gen=5" `Quick (fun () ->
			let c = test_cutoff ~gc_gen_cap: (Generation.make 2) ~gen_now: (Generation.make 5) in
			Alcotest.(check string) "" "3" Generation.(to_string c)
		);
		Alcotest.test_case "GC Cutoff Math/cap=10, gen=2 (underflow)" `Quick (fun () ->
			let c = test_cutoff ~gc_gen_cap: (Generation.make 10) ~gen_now: (Generation.make 2) in
			Alcotest.(check string) "" "0" Generation.(to_string c)
		);
		Alcotest.test_case "GC Cutoff Math/cap=0 (keep all)" `Quick (fun () ->
			let c = test_cutoff ~gc_gen_cap: Generation.min ~gen_now: (Generation.make 5) in
			Alcotest.(check string) "" "5" Generation.(to_string c)
		);
		Alcotest.test_case "GC Cutoff Math/cap=2, gen=14 (e hex)" `Quick (fun () ->
			let c = test_cutoff ~gc_gen_cap: (Generation.make 2) ~gen_now: (Generation.make 14) in
			Alcotest.(check string) "" "c" Generation.(to_string c)
		);
		Alcotest.test_case "GC Cutoff Math/cap=1, gen=1" `Quick (fun () ->
			let c = test_cutoff ~gc_gen_cap: (Generation.make 1) ~gen_now: (Generation.make 1) in
			Alcotest.(check string) "" "0" Generation.(to_string c)
		);
	]

let suite = gen_test :: gc_tests
