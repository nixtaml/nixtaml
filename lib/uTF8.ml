(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
(* Extending Camomile *)
include Camomile.UTF8

let equal a b = compare a b = 0

let pp ppf str = Fmt.pf ppf "%s" str

(* Display width rules for Unicode characters:
	• Combining marks and control characters: 0
	• CJK characters, Hangul, fullwidth forms: 2
	• Everything else: 1 *)
let uchar_width uch =
	let code = Camomile.UChar.code uch in
	if code = 0 then 0
	else if code <= 0x1F then 0 (* C0 control *)
	else if code = 0x7F then 0 (* DEL *)
	else if code >= 0x80 && code <= 0x9F then 0 (* C1 control *)
	else if code >= 0x300 && code <= 0x36F then 0 (* Combining diacritical marks *)
	else if code >= 0x200B && code <= 0x200F then 0 (* Zero-width formatters *)
	else if code >= 0x1100 && code <= 0x115F then 2 (* Hangul Jamo *)
	else if code >= 0x2E80 && code <= 0x9FFF then 2 (* CJK *)
	else if code >= 0xAC00 && code <= 0xD7AF then 2 (* Hangul Syllables *)
	else if code >= 0xF900 && code <= 0xFAFF then 2 (* CJK Compatibility *)
	else if code >= 0xFF01 && code <= 0xFF60 then 2 (* Fullwidth ASCII variants *)
	else if code >= 0xFFE0 && code <= 0xFFE6 then 2 (* Fullwidth symbol variants *)
	else 1

(* Compute display width of a UTF-8 string in terminal cells.
	NOTE: This is an approximation but handles common cases correctly. *)
let width (str : string) : int =
	let acc = ref 0 in
	iter (fun uch -> acc := !acc + uchar_width uch) str;
	!acc

(* Truncate a UTF-8 string to fit within max_width display cells. Returns the
	truncated string with "…" if truncation occurred. If the string already fits,
	returns it unchanged. *)
let truncate_to_width str max_width =
	if width str <= max_width then
		str
	else
		(* Need to truncate; find byte position where display width <= max *)
		let rec find_truncate_pos s target_width pos current_width =
			if pos >= String.length s then
				String.length s
			else
				try
					let uch = look s pos in
					let char_width = uchar_width uch in
					let next_width = current_width + char_width in
					if next_width > target_width then
						pos
					else
						find_truncate_pos s target_width (next s pos) next_width
				with
					| _ -> pos
		in
		let truncate_pos = find_truncate_pos str max_width 0 0 in
		if truncate_pos <= 1 then
			String.sub str 0 1 ^ "…"
		else
			String.sub str 0 (truncate_pos - 1) ^ "…"

let gen_uchar : Camomile.UChar.t QCheck.Gen.t =
	let open QCheck.Gen in
	let* i =
		oneof [
			int_range 0 0xD7FF;
			int_range 0xE000 0x10FFFF;
		];
	in
	return (Camomile.UChar.of_int i)

let gen : t QCheck.Gen.t =
	let open QCheck.Gen in
	let* size = int_bound 40 in
	let* chars = list_size (return size) gen_uchar in
	let buf = Buf.create size in
	List.iter (fun u -> Buf.add_char buf u) chars;
	return (Buf.contents buf)
