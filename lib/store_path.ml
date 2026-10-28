(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
type t =
	Store_path of UTF8.t
[@@unboxed]
[@@deriving show, eq, qcheck]

(* WARNING: this is ‘unsafe’, better to use `parse` when dealing with user data *)
let [@inline]make sp = Store_path sp

let [@inline]take (Store_path sp) = sp

let parse sp =
	let len = String.length sp in
	if len = 0 then
		Error (`Invalid_store_name (sp, 0, None))
	else
		let rec loop i =
			if i >= len then
				Ok (make sp)
			else
				match sp.[i] with
				| '/'
				| '-'
				| '.'
				| '+'
				| '_'
				| '?'
				| '='
				| 'a'..'z'
				| 'A'..'Z'
				| '0'..'9' ->
					loop (i + 1)
				| c -> Error (`Invalid_store_name (sp, i, Some c))
		in
		loop 0
