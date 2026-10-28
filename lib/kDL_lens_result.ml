(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: eilveli                                              │
│ SPDX-FileContributor: toastal <toastal@posteo.net>                           │
│ SPDX-License-Identifier: MPL-2.0                                             │
│                                                                              │
│ Upstream: https://github.com/eilvelia/ocaml-kdl                              │
│ Purpose: address “note: we can possibly replace option with result for more  │
│ detailed errors”                                                             │
│                                                                              │
│ Short-comings: no ‘trail’ for the errors & not extensible with user-defined  │
│ errors with ('a, 'b, 'err) proving difficult to work with                    │
└─────────────────────────────────────────────────────────────────────────────*)

type lerr = [
	| `Cannot_each
	| `Cannot_replace
	| `Missing_annot
	| `Missing_index of int
	| `Missing_prop of string
	| `Missing_top
	| `Not_found of string * string option
	| `Mismatched_type
	| `Wrong_type_bool
	| `Wrong_type_float
	| `Wrong_type_Int
	| `Wrong_type_Int32
	| `Wrong_type_Int64
	| `Wrong_type_native_int
	| `Wrong_type_null
	| `Wrong_type_number
	| `Wrong_type_string
	| `Wrong_type_stringNumber
]
[@@deriving show]

let pp_lerr fmt = function
	| `Cannot_each -> Fmt.pf fmt "Cannot each"
	| `Cannot_replace -> Fmt.pf fmt "Cannot replace"
	| `Missing_annot -> Fmt.pf fmt "Missing annotation"
	| `Missing_prop prop -> Fmt.pf fmt "Missing property “%s”" prop
	| `Missing_index idx -> Fmt.pf fmt "Missing index “%d”" idx
	| `Missing_top -> Fmt.pf fmt "Missing top-level node"
	| `Not_found (name, annot) ->
		begin
			match annot with
			| None -> Fmt.pf fmt "Not found “%s”" name
			| Some a -> Fmt.pf fmt "Not found “%s” with annotation (%s)" name a
		end
	| `Mismatched_type -> Fmt.pf fmt "Mismatched type"
	| `Wrong_type_bool -> Fmt.pf fmt "Wrong type, expected a boolean"
	| `Wrong_type_float -> Fmt.pf fmt "Wrong type, expected a float"
	| `Wrong_type_Int -> Fmt.pf fmt "Wrong type, expected an int"
	| `Wrong_type_Int32 -> Fmt.pf fmt "Wrong type, expected an int32"
	| `Wrong_type_Int64 -> Fmt.pf fmt "Wrong type, expected an int64"
	| `Wrong_type_native_int -> Fmt.pf fmt "Wrong type, expected a native int"
	| `Wrong_type_null -> Fmt.pf fmt "Wrong type, expected a null"
	| `Wrong_type_number -> Fmt.pf fmt "Wrong type, expected a number"
	| `Wrong_type_string -> Fmt.pf fmt "Wrong type, expected a string"
	| `Wrong_type_stringNumber -> Fmt.pf fmt "Wrong type, expected a string number"

(* Absent rather than malformed: the node/prop simply isn’t there.
	Deliberately excludes [Missing_index] & [Missing_top] — e.g. a bare
	[submodules] node (arg missing) means [true], while no node means
	[false]; swallowing the index error would invert that. *)
let is_missing : lerr -> bool = function
	| `Not_found _ | `Missing_prop _ -> true
	| _ -> false

open Kdl

(* note: we can possibly replace option with result for more detailed errors *)

type ('s, 'a) lens = {
	get: 's -> ('a, lerr) result;
	set: 'a -> 's -> ('s, lerr) result;
}

let get a lens = lens.get a

let set a v lens = lens.set v a

let get_exn a lens =
	match lens.get a with
	| Ok v -> v
	(*| Error e -> failwith (String.concat "; " (List.map lerr_to_string e))*)
	| Error e -> failwith (show_lerr e)

let set_exn a v lens =
	match lens.set v a with
	| Ok v -> v
	(*| Error e -> failwith (String.concat "; " (List.map lerr_to_string e))*)
	| Error e -> failwith (show_lerr e)

(* note: update can possibly be added to the definition of [lens] to increase
	performance with more specialized implementations *)

let update f a lens =
	match lens.get a with
	| Error e -> Error e
	| Ok value ->
		match f value with
		| Ok value' -> lens.set value' a
		| Error e -> Error e

let compose l1 l2 = {
	get = (fun x ->
		match l2.get x with
		| Ok x' -> l1.get x'
		| Error e -> Error e
	);
	set = (fun v a -> update (l1.set v) a l2)
}

let ( // ) l1 l2 = compose l2 l1

let (|--) = ( // )

let (.@()) = get
let (.@() <-) a l v = set a v l

let (.@!()) = get_exn
let (.@!() <-) a l v = set_exn a v l

let node_name = {
	get = (fun node -> Ok node.name);
	set = (fun name node -> Ok {node with name});
}

let node_annot = {
	get = (fun node -> Option.to_result ~none: `Missing_annot node.annot);
	set = (fun annot node -> Ok {node with annot = Some annot});
}

(* Unset the annotation by passing None *)
let node_annot_opt = {
	get = (fun node -> Ok node.annot);
	set = (fun annot node -> Ok {node with annot});
}

let args = {
	get = (fun node -> Ok node.args);
	set = (fun args node -> Ok {node with args});
}

let props = {
	get = (fun node -> Ok node.props);
	set = (fun props node -> Ok {node with props});
}

let children = {
	get = (fun node -> Ok node.children);
	set = (fun children node -> Ok {node with children});
}

let top = {
	get = (function node :: _ -> Ok node | [] -> Error `Missing_top);
	set = (fun node -> function _ :: xs -> Ok (node :: xs) | [] -> Error `Missing_top);
}

open struct
	let nth_and_replace n x' list =
		let found = ref false in
		(* Note: Unlike List.mapi, this stops iterating when we've found the element *)
		let [@tail_mod_cons] rec go i = function
			| [] -> []
			| _ :: xs when i = n -> found := true; x' :: xs
			| x :: xs -> x :: go (i + 1) xs
		in
		let result = go 0 list in
		if !found then Ok result else Error (`Missing_index n)

	let filter_and_replace f replace_list list =
		let found = ref false in
		let f (replace, result) x =
			if f x then
				begin
					found := true;
					match replace with
					| x' :: xs -> xs, x' :: result
					| [] -> [], x :: result
				end
			else
				replace, x :: result
		in
		let _, list = List.fold_left f (replace_list, []) list in
		if !found then Ok (List.rev list) else Error `Cannot_replace

	let [@inline]matches_node ?annot name node =
		String.equal node.name name
		&& (
			match annot with
			| Some a ->
				(
					match node.annot with
					| Some a' -> String.equal a a'
					| None -> false
				)
			| None -> true
		)

	let rec find_node n annot name = function
		| [] -> Error (`Not_found (name, annot))
		| x :: xs when matches_node ?annot name x ->
			if n <= 0 then Ok x else find_node (n - 1) annot name xs
		| _ :: xs -> find_node n annot name xs

	let find_and_replace_node nth annot name x' list =
		let found = ref false in
		let [@tail_mod_cons] rec go n = function
			| [] -> []
			| x :: xs when matches_node ?annot name x ->
				if n <= 0 then (found := true; x' :: xs) else x :: go (n - 1) xs
			| x :: xs -> x :: go n xs
		in
		let result = go nth list in
		if !found then Ok result else Error (`Not_found (name, annot))
end

let nth n = {
	get = (fun list ->
		List.nth_opt list n
		|> Option.to_result ~none: (`Missing_index n)
	);
	set = (fun x' list -> nth_and_replace n x' list)
}

(* these operations are O(n), and update is quite inefficient *)
let arg n = {
	(* Inlined [nth] instead of [args // nth n] *)
	get = (fun node ->
		List.nth_opt node.args n
		|> Option.to_result ~none: (`Missing_index n)
	);
	set = (fun arg' node ->
		nth_and_replace n arg' node.args
		|> Result.map (fun args -> {node with args})
	)
}

let first_arg = arg 0

let prop key = {
	get = (fun node ->
		List.assoc_opt key node.props
		|> Option.to_result ~none: (`Missing_prop key)
	);
	set = (fun v' node ->
		let found = ref false in
		let f (k, v) = if k = key then (found := true; k, v') else k, v in
		let props = List.map f node.props in
		if !found then Ok {node with props} else Error (`Missing_prop key)
	)
}

let node ?(nth = 0) ?annot (name : string) = {
	get = (fun nodes -> find_node nth annot name nodes);
	set = (fun node' nodes -> find_and_replace_node nth annot name node' nodes)
}

let node_many ?annot (name : string) =
	let matches = matches_node ?annot name in
	{
		get = (fun nodes ->
			match List.filter matches nodes with
			| [] -> Error (`Not_found (name, annot))
			| xs -> Ok xs
		);
		set = (fun nodes' nodes -> filter_and_replace matches nodes' nodes)
	}

let node_nth : int -> (node list, node) lens = nth

(* TODO: get node by annot only? *)

let child ?nth ?annot name = children // node ?nth ?annot name
let child_many ?annot name = children // node_many ?annot name
let child_nth n = children // node_nth n

let value : (annot_value, value) lens = {
	get = (fun (_, v) -> Ok v);
	set = (fun v' (a, _) -> Ok (a, v'));
}

let annot : (annot_value, string) lens = {
	get = (fun (a, _) -> Option.to_result ~none: `Missing_annot a);
	set = (fun a' (_, v) -> Ok (Some a', v));
}

let annot_opt : (annot_value, string option) lens = {
	get = (fun (a, _) -> Ok a);
	set = (fun a' (_, v) -> Ok (a', v));
}

let string = {
	get = (function `String str -> Ok str | _ -> Error `Wrong_type_string);
	set = (fun value' _value -> Ok (`String value'));
}

(* Ast.Num.of_string not exposed *)
let number : (value, number) lens = {
	get = (fun n -> L.number.get n |> Option.to_result ~none: `Wrong_type_number);
	set = (fun num n -> L.number.set num n |> Option.to_result ~none: `Wrong_type_number);
}

let string_number : (value, string) lens = {
	get = (fun n -> L.string_number.get n |> Option.to_result ~none: `Wrong_type_stringNumber);
	set = (fun x n -> L.string_number.set x n |> Option.to_result ~none: `Wrong_type_stringNumber);
}

let float_number : (value, float) lens = {
	get = (fun n -> L.float_number.get n |> Option.to_result ~none: `Wrong_type_float);
	set = (fun x n -> L.float_number.set x n |> Option.to_result ~none: `Wrong_type_float);
}

let int_number : (value, int) lens = {
	get = (fun n -> L.int_number.get n |> Option.to_result ~none: `Wrong_type_Int);
	set = (fun x n -> L.int_number.set x n |> Option.to_result ~none: `Wrong_type_Int);
}

let int32_number : (value, int32) lens = {
	get = (fun n -> L.int32_number.get n |> Option.to_result ~none: `Wrong_type_Int32);
	set = (fun x n -> L.int32_number.set x n |> Option.to_result ~none: `Wrong_type_Int32);
}

let int64_number : (value, int64) lens = {
	get = (fun n -> L.int64_number.get n |> Option.to_result ~none: `Wrong_type_Int64);
	set = (fun x n -> L.int64_number.set x n |> Option.to_result ~none: `Wrong_type_Int64);
}

let nativeint_number : (value, nativeint) lens = {
	get = (fun n -> L.nativeint_number.get n |> Option.to_result ~none: `Wrong_type_native_int);
	set = (fun x n -> L.nativeint_number.set x n |> Option.to_result ~none: `Wrong_type_native_int);
}

let bool = {
	get = (function `Bool b -> Ok b | _ -> Error `Wrong_type_bool);
	set = (fun value' _value -> Ok (`Bool value'))
}

let null = {
	get = (function `Null -> Ok () | _ -> Error `Wrong_type_null);
	set = (fun _ _ -> Ok `Null)
}

let string_value : (annot_value, string) lens = value // string
let number_value : (annot_value, number) lens = value // number
let string_number_value : (annot_value, string) lens = value // string_number
let float_number_value : (annot_value, float) lens = value // float_number
let int_number_value : (annot_value, int) lens = value // int_number
let int32_number_value : (annot_value, int32) lens = value // int32_number
let int64_number_value : (annot_value, int64) lens = value // int64_number
let nativeint_number_value : (annot_value, nativeint) lens =
	value // nativeint_number
let bool_value : (annot_value, bool) lens = value // bool
let null_value : (annot_value, unit) lens = value // null

let filter f = {
	get = (fun list -> Ok (List.filter f list));
	set = (fun replace list -> filter_and_replace f replace list)
}

open struct
	exception Short_circuit

	let mapm_option f list =
		let g a =
			match f a with
			| Ok x -> x
			| Error _ -> raise_notrace Short_circuit
		in
		try
			Ok (List.map g list)
		with
			| Short_circuit -> Error `Cannot_each
end

let each l = {
	get = (fun list -> mapm_option l.get list);
	set = (fun replace_list list ->
		let f (replace, result) v =
			match replace with
			| v' :: replace_rest ->
				(
					match l.set v' v with
					| Ok x -> replace_rest, x :: result
					| Error _ -> raise_notrace Short_circuit
				)
			| [] -> [], v :: result
		in
		try
			let _, list = List.fold_left f (replace_list, []) list in
			Ok (List.rev list)
		with
			| Short_circuit -> Error `Cannot_each
	)
}
