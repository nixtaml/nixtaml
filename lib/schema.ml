(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
module Version = struct
	(* These types help us get compiler errors while introducing new errors *)
	type latest = [`v1_5_0]
	type legacy = [`v0_5_0 | `v1_0_0 | `v1_1_0 | `v1_2_0 | `v1_3_0 | `v1_4_0]
	type t = [latest | legacy]

	let to_string : t -> string = function
		| `v1_5_0 -> "1.5.0"
		| `v1_4_0 -> "1.4.0"
		| `v1_3_0 -> "1.3.0"
		| `v1_2_0 -> "1.2.0"
		| `v1_1_0 -> "1.1.0"
		| `v1_0_0 -> "1.0.0"
		| `v0_5_0 -> "0.5.0"

	let pp fmt v = Format.pp_print_string fmt (to_string v)

	let to_enum : t -> int = function
		| `v1_5_0 -> 0x6
		| `v1_4_0 -> 0x5
		| `v1_3_0 -> 0x4
		| `v1_2_0 -> 0x3
		| `v1_1_0 -> 0x2
		| `v1_0_0 -> 0x1
		| `v0_5_0 -> 0x0

	let of_enum (idx : int) : t option =
		match idx with
		| 0x6 -> Some `v1_5_0
		| 0x5 -> Some `v1_4_0
		| 0x4 -> Some `v1_3_0
		| 0x3 -> Some `v1_2_0
		| 0x2 -> Some `v1_1_0
		| 0x1 -> Some `v1_0_0
		| 0x0 -> Some `v0_5_0
		| _ -> None

	let [@inline]compare (a : t) (b : t) =
		Int.compare (to_enum a) (to_enum b)

	let [@inline]equal (a : t) (b : t) =
		Int.equal (to_enum a) (to_enum b)

	let latest : t Lazy.t =
		lazy(
			let ver = ref None
			and idx = ref 0x0
			in
			while begin
				match of_enum !idx with
				| Some v -> ver := Some v; incr idx; true
				| None -> false
			end do () done;
			Option.get !ver
		)

	let versions : t array Lazy.t =
		lazy(
			let vs = Dynarray.create ()
			and idx = ref 0x0
			in
			while begin
				match of_enum !idx with
				| Some v -> Dynarray.add_last vs v; incr idx; true
				| None -> false
			end do () done;
			Dynarray.to_array vs
		)

	let of_string (s : string) : t option =
		match s with
		| "1.5.0" -> Some `v1_5_0
		| "1.4.0" -> Some `v1_4_0
		| "1.3.0" -> Some `v1_3_0
		| "1.2.0" -> Some `v1_2_0
		| "1.1.0" -> Some `v1_1_0
		| "1.0.0" -> Some `v1_0_0
		| "0.5.0" -> Some `v0_5_0
		| _ -> None

	let to_string : t -> string = function
		| `v1_5_0 -> "1.5.0"
		| `v1_4_0 -> "1.4.0"
		| `v1_3_0 -> "1.3.0"
		| `v1_2_0 -> "1.2.0"
		| `v1_1_0 -> "1.1.0"
		| `v1_0_0 -> "1.0.0"
		| `v0_5_0 -> "0.5.0"

	let jsont : t Jsont.t =
		let open Jsont in
		let enc v = to_string v
		and dec s =
			match of_string s with
			| Some v -> v
			| None -> invalid_arg ("Invalid version: " ^ s)
		in
		map ~kind: "Version" ~dec ~enc string

	let is_legacy : t -> bool = function
		| `v1_5_0 ->
			false
		| `v1_4_0
		| `v1_3_0
		| `v1_2_0
		| `v1_1_0
		| `v1_0_0
		| `v0_5_0 ->
			true
end
