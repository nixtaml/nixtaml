(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
(* extend & fix casing for ocaml-kdl *)
include Kdl

let of_flow flow =
	try
		Eio.Buf_read.parse_exn
			(fun buf -> Eio.Buf_read.take_all buf |> Kdl.of_string)
			~max_size: max_int
			flow
	with
		| exn -> failwith (Printexc.to_string exn)

let to_flow flow doc =
	Eio.Buf_write.with_flow flow @@ fun buf ->
	let out_string s off len =
		String.sub s off len |> Eio.Buf_write.string buf
	in
	let flush () = () in
	let ppf = Format.make_formatter out_string flush in

	(* replace spaces with tabs for indentation *)
	let base_fmt = Format.pp_get_formatter_out_functions ppf () in
	let tabbed_fmt = {base_fmt with
		out_indent = (fun n -> base_fmt.out_string (String.make n '\t') 0 n)
	}
	in
	Format.pp_set_formatter_out_functions ppf tabbed_fmt;

	(* enable utf-8 and pretty-print *)
	Fmt.set_utf_8 ppf true;
	pp ppf doc;
	Format.pp_print_flush ppf ()

module L = KDL_lens_result

module Valid = struct
	type err = [
		| L.lerr
		| `Parse_error of Kdl.error
		| `One_required of string list
		| `Invalid_kind of [`File | `Archive | `Git | `Darcs | `Pijul | `Fossil] * err list
		| `Only_one_of of string list
		| `Invalid_fresh_cmd of string
		| `Invalid_hash_algorithm of string
		| `Invalid_fetch_time of string
		| `Invalid_GC of string
		| `Eval_fetch_time_at_odds_with of [`Patches]
		| `Invalid_version of string
	]
	[@@deriving show]
	type 'a t = ('a, err list) result
	let pp ~(ok : 'a Fmt.t) : 'a t Fmt.t =
		Fmt.result ~ok ~error: (Fmt.list pp_err)
	let map = Result.map
	let map1 (f : 'a -> 'b) (vx : ('a, err) result) : 'b t =
		match vx with
		| Ok x -> Ok (f x)
		| Error e -> Error [e]
	let map_error = Result.map_error
	let pure x = Ok x
	let and_map (vx : 'a t) (vf : ('a -> 'b) t) =
		match vx, vf with
		| Ok x, Ok f -> Ok (f x)
		| Error e, Ok _ -> Error e
		| Ok _, Error e -> Error e
		| Error e1, Error e2 -> Error (e2 @ e1)
	let product (vx : 'a t) (vy : 'b t) =
		match vx, vy with
		| Ok x, Ok y -> Ok (x, y)
		| Error e, Ok _ -> Error e
		| Ok _, Error e -> Error e
		| Error e1, Error e2 -> Error (e2 @ e1)
	let and_map1 (vx : ('a, err) result) (vf : ('a -> 'b) t) =
		match vx, vf with
		| Ok x, Ok f -> Ok (f x)
		| Error e, Ok _ -> Error [e]
		| Ok _, Error e -> Error e
		| Error e1, Error e2 -> Error (e2 @ [e1])
	let bind = Result.bind
	(* An empty error list counts as absent: there is no evidence anything was
		present. *)
	let is_missing_err : err -> bool = function
		| `Not_found _ | `Missing_prop _ -> true
		| _ -> false
	(* Missing node/prop falls back to [default]; present-but-malformed still
		fails. Mirrors [L.is_missing]. *)
	let optional ~(default : 'a) : 'a t -> 'a t = function
		| Ok _ as ok -> ok
		| Error errs when List.for_all is_missing_err errs -> Ok default
		| Error _ as e -> e
	(* Validated lookup: [None] becomes the given single error. *)
	let required ~(none : err) : 'a option -> 'a t = function
		| Some x -> Ok x
		| None -> Error [none]
	(* Missing node/prop becomes [None]; present-but-malformed still fails. *)
	let find : 'a t -> 'a option t = function
		| Ok x -> Ok (Some x)
		| Error errs when List.for_all is_missing_err errs -> Ok None
		| Error _ as e -> e
	(* [bind], but missing becomes [default] instead of failing. *)
	let optional_bind ~(default : 'b) (valid : 'a -> 'b t) : 'a t -> 'b t = function
		| Ok x -> valid x
		| Error errs when List.for_all is_missing_err errs -> Ok default
		| Error _ as e -> e
	(* Sequence, concatenating errors in order (unlike [and_map], which reverses).
		*)
	let rec all : 'a t list -> 'a list t = function
		| [] -> Ok []
		| vx :: vxs ->
			match vx, all vxs with
			| Ok y, Ok ys -> Ok (y :: ys)
			| Error e1, Ok _ -> Error e1
			| Ok _, Error e2 -> Error e2
			| Error e1, Error e2 -> Error (e1 @ e2)
	(* Which named branches parsed: none ([All_absent]), exactly one ([One],
		pairing the ok value with any broken siblings, which are ignored), several
		([Clash], likewise ignoring sibling failures), or none with failures
		([Broken]). Whether a failed branch counts as absent is the {i caller}'s call:
		[absent] takes the branch name & its errors — as an example, input kind counts
		as absent only on its own [Not_found], while a Git reference treats any
		failure as absent. Branches failing with no errors count as absent. *)
	type 'a presence =
		| All_absent
		| One of 'a * (string * err list) list
		| Clash of (string * 'a) list
		| Broken of (string * err list) list
	[@@deriving show]
	let present ~(absent : string -> err list -> bool) ~(named : (string * 'a t) list) : 'a presence =
		let rec go oks invalids = function
			| [] -> oks, invalids
			| (name, vx) :: rest ->
				match vx with
				| Ok x -> go ((name, x) :: oks) invalids rest
				| Error errs when absent name errs -> go oks invalids rest
				| Error errs -> go oks ((name, errs) :: invalids) rest
		in
		match go [] [] named with
		| [], [] -> All_absent
		| [(_, x)], invalids -> One (x, List.rev invalids)
		| [], invalids -> Broken (List.rev invalids)
		| oks, _ -> Clash (List.rev oks)
	let lift_lens (r : ('a, L.lerr) result) : ('a, err list) result =
		Result.map_error (fun e -> ([e] :> err list)) r
	let ll = lift_lens
	let (let+) vx f = map f vx
	let (and+) = product
	let (let*) = bind
end

type 'a codec = {
	to_kdl: 'a -> t;
	of_kdl: t -> 'a Valid.t
}

type 'a node_codec = {
	to_node: 'a -> node;
	of_node: node -> 'a Valid.t
}
