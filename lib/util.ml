(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
module Path = struct
	let rel ~from ~to_ =
		let segs s = List.filter (fun p -> p <> "" && p <> ".") (String.split_on_char '/' s) in
		let rec drop_prefix from_components to_components =
			match from_components, to_components with
			| h1 :: t1, h2 :: t2 when h1 = h2 -> drop_prefix t1 t2
			| _ -> from_components, to_components
		in
		let from_segs = segs from
		and to_segs = segs to_
		in
		let remaining_from, remaining_to = drop_prefix from_segs to_segs in
		String.concat "/" (List.init (List.length remaining_from) (fun _ -> "..") @ remaining_to)
end

module Formatter = struct
	let to_flow pp flow =
		let buffer = Buffer.create 4096 in
		let fmt = Format.formatter_of_buffer buffer in
		pp fmt;
		Format.pp_print_flush fmt ();
		Eio.Flow.copy_string (Buffer.contents buffer) flow
end

module Jsont = struct
	include Jsont

	let encode_tag tag_code encoder v = [|
		(
			match Json.encode uint8 tag_code with
			| Ok r -> r
			| Error e -> failwith ("Json.encode uint8 tag failed: " ^ e)
		);
		(
			match Json.encode encoder v with
			| Ok r -> r
			| Error e -> failwith ("Json.encode value failed: " ^ e)
		);
	|]

	let pp_lockfile fmt json =
		let rec pp_value depth fmtr json =
			match json with
			| Jsont.Null _ -> Fmt.pf fmtr "%a" Jsont.pp_null ()
			| Jsont.Bool (b, _) -> Fmt.pf fmtr "%a" Jsont.pp_bool b
			| Jsont.Number (f, _) -> Fmt.pf fmtr "%a" Jsont.pp_number f
			| Jsont.String (s, _) -> Fmt.pf fmtr "%a" Jsont.pp_string s
			| Jsont.Array (arr, _) ->
				Fmt.pf fmtr "[%a]" (Fmt.list ~sep: (Fmt.any ",") (pp_value (depth + 1))) arr
			| Jsont.Object (obj, _) ->
				let pp_mem fmtr ((k, _), v) =
					Fmt.pf fmtr "%a:%a" Jsont.pp_string k (pp_value (depth + 1)) v
				in
				match List.length obj, depth with
				| 0, _ -> Fmt.pf fmtr "{}"
				| _, 0 -> Fmt.pf fmtr "{%a\n}" (Fmt.list ~sep: (Fmt.any "\n,") pp_mem) obj
				| _, 1 -> Fmt.pf fmtr "{\n%a\n}" (Fmt.list ~sep: (Fmt.any "\n,") pp_mem) obj
				| _ -> Fmt.pf fmtr "{%a}" (Fmt.list ~sep: (Fmt.any ",") pp_mem) obj
		in
		pp_value 0 fmt json

	let of_flow codec flow =
		try
			let buf = Eio.Buf_read.of_flow flow ~max_size: max_int in
			let str = Eio.Buf_read.take_all buf in
			Jsont_bytesrw.decode_string codec str
		with
			| exn -> Error (Printexc.to_string exn)

	let to_flow codec value flow =
		match Jsont_bytesrw.encode_string codec value with
		| Ok str ->
			Eio.Flow.copy_string str flow;
			Ok ()
		| Error err -> Error err

	let to_flow_lockfile codec value flow =
		match Jsont.Json.encode codec value with
		| Ok json ->
			let buffer = Buffer.create 4096 in
			let fmt = Format.formatter_of_buffer buffer in
			(* Try our damnedest to get one input per line *)
			Format.pp_set_margin fmt max_int;
			let base_fmt = Format.pp_get_formatter_out_functions fmt () in
			let no_indent_fmt = {base_fmt with
				out_indent = (fun _ -> ())
			}
			in
			Format.pp_set_formatter_out_functions fmt no_indent_fmt;
			pp_lockfile fmt json;
			Format.pp_print_newline fmt ();
			Format.pp_print_flush fmt ();
			Eio.Flow.copy_string (Buffer.contents buffer) flow;
			Ok ()
		| Error err -> Error err
end

module These = struct
	type ('a, 'b) t =
		| This of 'a
		| That of 'b
		| These of 'a * 'b
	[@@deriving show]

	let map_both f g = function
		| This a -> This (f a)
		| That b -> That (g b)
		| These (a, b) -> These (f a, g b)

	let map_this f =
		map_both f Fun.id

	let map_that g =
		map_both Fun.id g

	let these f g h = function
		| This a -> f a
		| That b -> g b
		| These (a, b) -> h a b

	let merge h =
		these Fun.id Fun.id h

	let merge_with h f g =
		these f g (fun a b -> h (f a) (g b))
end

module Non_empty_list = struct
	type 'a t =
			('a * 'a list)
	[@@deriving show, eq, qcheck]

	let to_list (x, xs) = x :: xs

	let of_list = function
		| [] -> None
		| x :: xs -> Some (x, xs)

	let map f (x, xs) = (f x, List.map f xs)
	let fold_left f acc (x, xs) = List.fold_left f acc (x :: xs)
	let fold_right f acc (x, xs) = List.fold_right f acc (x :: xs)
end

module String = struct
	include Stdlib.String

	let escape_nix s =
		let buf = Buffer.create (String.length s * 2) in
		let esc = function
			| '\\' -> Buffer.add_string buf "\\\\"
			| '"' -> Buffer.add_string buf "\\\""
			| '$' -> Buffer.add_string buf "\\$"
			| c -> Buffer.add_char buf c
		in
		iter esc s;
		Buffer.contents buf
end
