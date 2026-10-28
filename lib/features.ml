(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
type t = int

let empty = 0x0

let max = 0xc

let pp fmt feat =
	for idx =
	max downto 0 do
		Format.pp_print_char
			fmt
			(if ((feat lsr idx) land 1) = 1 then '1' else '0')
	done

let show = Fmt.str "%a" pp

let [@inline]equal a b = a = b

let [@inline]compare a b = compare a b

let gen : t QCheck.Gen.t =
	let open QCheck.Gen in
	let* range = int_range 0 max in
	return (1 lsl range)

module Flag = struct
	type t =
		Flag of int
	[@@unboxed]

	let [@inline]make u = Flag u
	let [@inline]take (Flag u) = u

	let pp = pp
	let show = show
	let equal = equal
	let compare = compare
	let gen = gen

	(* only build features needed *)
	let patches : t = Flag (1 lsl 0x0)
	let file : t = Flag (1 lsl 0x8)
	let archive : t = Flag (1 lsl 0x9)
	let git : t = Flag (1 lsl 0xa)
	let darcs : t = Flag (1 lsl 0xb)
	let pijul : t = Flag (1 lsl 0xc)
	let fossil : t = Flag (1 lsl 0xd)
end

let [@inline]has (Flag.Flag f) (v : t) : bool = (v land f) <> 0
let [@inline]add (Flag.Flag f) (v : t) : t = v lor f

let add_kind (input : Input.t) : t -> t =
	match input.kind with
	| `File _ -> add Flag.file
	| `Archive _ -> add Flag.archive
	| `Git _ -> add Flag.git
	| `Darcs _ -> add Flag.darcs
	| `Pijul _ -> add Flag.pijul
	| `Fossil _ -> add Flag.fossil

let add_patches (input : Input.t) (v : t) : t =
	if List.is_empty input.patches then v
	else add Flag.patches v

let add_input (input : Input.t) (v : t) : t =
	v
	|> add_kind input
	|> add_patches input

(* used for testing *)
let all_features =
	empty
	|> add Flag.patches
	|> add Flag.file
	|> add Flag.archive
	|> add Flag.git
	|> add Flag.darcs
	|> add Flag.pijul
	|> add Flag.fossil

(*
let jsont : t Jsont.t =
	let dec s =
		try
			let (features, pos) = of_substring s ~pos: 0 in
			if pos = String.length s then
				features
			else
				Jsont.Error.msgf Jsont.Meta.none "Not a valid feature flag (uint32)"
		with
			| _ -> Jsont.Error.msgf Jsont.Meta.none "Not a valid feature flag (uint32)"
	and enc = to_string_hex
	in
	Jsont.map ~kind: "Feature" ~dec ~enc Jsont.string
	*)
