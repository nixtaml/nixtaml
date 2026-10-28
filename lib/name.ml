(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
module Input = struct
	type t =
		Input of UTF8.t
	[@@unboxed]
	[@@deriving eq, qcheck]

	let [@inline]make n = Input n

	let [@inline]take (Input n) = n

	let pp fmt name =
		(* it’s okay to have fun *)
		Fmt.pf fmt "「%s」" (take name)

	let show = Fmt.str "%a" pp

	let compare (Input a) (Input b) =
		let prio x =
			if x = "nixpkgs" then
				0
			else if String.starts_with ~prefix: "nixpkgs" x then
				1
			else
				2
		in
		match Stdlib.compare (prio a) (prio b) with
		| 0 -> String.compare a b
		| d -> d

	let jsont : t Jsont.t =
		Jsont.string
		|> Jsont.map ~kind: "Name_input" ~dec: make ~enc: take
end

module Patch = struct
	type t =
		Patch of UTF8.t
	[@@unboxed]
	[@@deriving eq, ord, qcheck]

	let [@inline]make n = Patch n
	let [@inline]take (Patch n) = n

	let pp fmt name =
		(* it’s okay to have fun *)
		Fmt.pf fmt "「%s」" (take name)

	let show = Fmt.str "%a" pp

	let jsont : t Jsont.t =
		Jsont.string |> Jsont.map ~kind: "Name_patch" ~dec: make ~enc: take
end

module Store = struct
	type t =
		Store of UTF8.t
	[@@unboxed]
	[@@deriving eq, ord]

	(* the Nix store unfortunately doesn’t support much in the way of Unicode
		despite any reasonable file system supporting it

		See: https://nixos.wiki/wiki/Valid_Nix_store_path_names *)
	let parse name =
		let len = UTF8.length name in
		if len = 0 then
			Error (`Invalid_store_name (name, 0, None))
		else
			let first = UTF8.get name 0 in
			let first_uchar = Camomile.UChar.char_of first in
			if first_uchar = '-' || first_uchar = '.' then
				Error (`Invalid_store_name (name, 0, Some first_uchar))
			else
				let rec loop i =
					if i >= len then
						Ok (Store name)
					else
						let c = UTF8.get name i in
						let uch = Camomile.UChar.char_of c in
						if uch = '-'
							|| uch = '.'
							|| uch = '+'
							|| uch = '_'
							|| uch = '?'
							|| uch = '='
							|| (uch >= 'a' && uch <= 'z')
							|| (uch >= 'A' && uch <= 'Z')
							|| (uch >= '0' && uch <= '9') then
							loop (i + 1)
						else
							Error (`Invalid_store_name (name, i, Some uch))
				in
				loop 1

	let is_valid str = Result.is_ok (parse str)

	(* WARNING: this is ‘unsafe’, better to use `parse` when dealing with user data *)
	let [@inline]make s = Store s
	let [@inline]take (Store n) = n

	let pp fmt name =
		(* it’s okay to have fun *)
		Fmt.pf fmt "「%s」" (take name)

	let show = Fmt.str "%a" pp

	let gen =
		let open QCheck.Gen in
		let valid_first =
			oneof
				[
					map (fun c -> Char.chr (c + Char.code 'a')) (int_range 0 25);
					map (fun c -> Char.chr (c + Char.code 'A')) (int_range 0 25);
					map (fun c -> Char.chr (c + Char.code '0')) (int_range 0 9)
				]
		in
		let valid_rest =
			oneof
				[
					map (fun c -> Char.chr (c + Char.code 'a')) (int_range 0 25);
					map (fun c -> Char.chr (c + Char.code 'A')) (int_range 0 25);
					map (fun c -> Char.chr (c + Char.code '0')) (int_range 0 9);
					return '-';
					return '.';
					return '+';
					return '_';
					return '?';
					return '='
				]
		in
		let+ first = valid_first
		and+ rest = list_size (int_range 0 20) valid_rest
		in
		Store (String.make 1 first ^ String.concat "" (List.map (String.make 1) rest))

	let jsont : t Jsont.t =
		Jsont.string
		|> Jsont.map ~kind: "Name_store" ~dec: (fun s -> Store s) ~enc: take
end

module InputSet = struct
	module Impl = Set.Make(struct
		type t = Input.t
		let compare = Input.compare
	end)
	include Impl

	let pp fmt set =
		Fmt.pf fmt "{%a}" (Fmt.list ~sep: Fmt.sp Input.pp) (elements set)
end

module PatchSet = struct
	module Impl = Set.Make(struct
		type t = Patch.t
		let compare = Patch.compare
	end)
	include Impl

	let pp fmt set =
		Fmt.pf fmt "{%a}" (Fmt.list ~sep: Fmt.sp Patch.pp) (elements set)
end

module InputMap = struct
	module Impl = Map.Make(struct
		type t = Input.t
		let compare = Input.compare
	end)
	include Impl

	type 'a t = 'a Impl.t

	let pp pp_value fmt map =
		Fmt.list
			~sep: (Fmt.any "; ")
			(fun ppf (key, value) -> Fmt.pf ppf "%s ↦ %a" (Input.take key) pp_value value)
			fmt
			(bindings map)

	let gen gen_val =
		let open QCheck.Gen in
		let* n = int_bound 32 in
		let* vals = list_size (return n) (pair Input.gen gen_val) in
		return (List.fold_left (fun m (k, v) -> Impl.add k v m) Impl.empty vals)

	let jsont ?kind ?doc (type' : 'a Jsont.t) : 'a t Jsont.t =
		let input_map =
			let dec_empty () = empty
			and dec_add _meta key value mems = add (Input.make key) value mems
			and dec_finish _meta mems = mems
			and enc f mems acc =
				fold (fun n v acc -> f Jsont.Meta.none (Input.take n) v acc) mems acc
			in
			Jsont.Object.Mems.map type' ~dec_empty ~dec_add ~dec_finish ~enc: {enc}
		in
		Jsont.Object.map ?kind ?doc Fun.id
		|> Jsont.Object.keep_unknown input_map ~enc: Fun.id
		|> Jsont.Object.finish
end

module PatchMap = struct
	module Impl = Map.Make(struct
		type t = Patch.t
		let compare = Patch.compare
	end)
	include Impl

	type 'a t = 'a Impl.t

	let pp pp_value fmt map =
		Fmt.list
			~sep: (Fmt.any ", ")
			(fun ppf (key, value) -> Fmt.pf ppf "%s ↦ %a" (Patch.take key) pp_value value)
			fmt
			(bindings map)

	let gen gen_val =
		let open QCheck.Gen in
		let* n = int_bound 32 in
		let* vals = list_size (return n) (pair Patch.gen gen_val) in
		return (List.fold_left (fun m (k, v) -> Impl.add k v m) Impl.empty vals)

	let jsont ?kind ?doc (type' : 'a Jsont.t) : 'a t Jsont.t =
		let patch_map =
			let dec_empty () = empty
			and dec_add _meta key value mems = add (Patch.make key) value mems
			and dec_finish _meta mems = mems
			and enc f mems acc =
				fold (fun n v acc -> f Jsont.Meta.none (Patch.take n) v acc) mems acc
			in
			Jsont.Object.Mems.map type' ~dec_empty ~dec_add ~dec_finish ~enc: {enc}
		in
		Jsont.Object.map ?kind ?doc Fun.id
		|> Jsont.Object.keep_unknown patch_map ~enc: Fun.id
		|> Jsont.Object.finish
end
