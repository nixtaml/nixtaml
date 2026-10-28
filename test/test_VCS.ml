(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
let test_append_to_new () =
	let lines_to_add = ["foo"; "bar"] in
	let result = Nixtamal.VCS.append_to_ignore_content ~lines_to_add String.empty
	and expected =
		{|# Nixtamal
foo
bar
|}
	in
	Alcotest.(check (option string)) "new file" (Some expected) result

let test_append_to_existing () =
	let content =
		{|existing_content
|}
	in
	let lines_to_add = ["foo"; "bar"] in
	let result = Nixtamal.VCS.append_to_ignore_content ~lines_to_add content
	and expected =
		{|existing_content

# Nixtamal
foo
bar
|}
	in
	Alcotest.(check (option string)) "existing file" (Some expected) result

let test_append_middle () =
	let content =
		{|start

# Nixtamal
foo

end
|}
	and lines_to_add = ["bar"; "baz"]
	in
	let result = Nixtamal.VCS.append_to_ignore_content ~lines_to_add content
	and expected =
		{|start

# Nixtamal
foo
bar
baz

end
|}
	in
	Alcotest.(check (option string)) "middle append" (Some expected) result

let test_duplicate_lines_not_added () =
	let content =
		{|# Nixtamal
foo
|}
	and lines_to_add = ["foo"; "bar"]
	in
	let result = Nixtamal.VCS.append_to_ignore_content ~lines_to_add content
	and expected =
		{|# Nixtamal
foo
bar
|}
	in
	Alcotest.(check (option string)) "no duplicate" (Some expected) result

let test_full_cycle_with_file () =
	Eio_main.run @@ fun env ->
	let test_dir = Eio.Path.(Test_util.tmp_dir ~env / "VCS_ignore_full_cycle") in
	Eio.Path.mkdirs test_dir ~exists_ok: true ~perm: 0o755;
	Fun.protect
		~finally: (fun () -> Eio.Path.rmtree test_dir)
		(fun () ->
			let ignore_file = Eio.Path.(test_dir / ".ignore")
			and initial_content =
				{|start
# Nixtamal
foo

end|}
			in
			Eio.Path.save ~create: (`Or_truncate 0o644) ignore_file initial_content;
			let lines_to_add = ["foo"; "bar"; "baz"] in
			Nixtamal.VCS.append_unique_line_with_comment ~file: ignore_file ~lines_to_add;
			let result = Eio.Path.load ignore_file
			and expected =
				{|start
# Nixtamal
foo
bar
baz

end|}
			in
			Alcotest.(check string) "full cycle with file" expected result
		)

let tests = [
	Alcotest.test_case "append to new string" `Quick test_append_to_new;
	Alcotest.test_case "append to existing string" `Quick test_append_to_existing;
	Alcotest.test_case "append in middle of string" `Quick test_append_middle;
	Alcotest.test_case "duplicate lines not added" `Quick test_duplicate_lines_not_added;
	Alcotest.test_case "full cycle with file" `Quick test_full_cycle_with_file;
]
