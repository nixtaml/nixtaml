(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
module Store : sig
	type t
	val make : UTF8.t -> t
	val take : t -> UTF8.t
	val is_valid : UTF8.t -> bool
	val parse : UTF8.t -> (t, [`Invalid_store_name of UTF8.t * int * char option]) result
	val pp : t Fmt.t
	val show : t -> string
	val compare : t -> t -> int
	val equal : t -> t -> bool
	val jsont : t Jsont.t
	val gen : t QCheck.Gen.t
end

module Input : sig
	type t
	val make : UTF8.t -> t
	val take : t -> UTF8.t
	val pp : t Fmt.t
	val show : t -> string
	val compare : t -> t -> int
	val equal : t -> t -> bool
	val jsont : t Jsont.t
	val gen : t QCheck.Gen.t
end

module Patch : sig
	type t
	val make : UTF8.t -> t
	val take : t -> UTF8.t
	val pp : t Fmt.t
	val show : t -> string
	val compare : t -> t -> int
	val equal : t -> t -> bool
	val jsont : t Jsont.t
	val gen : t QCheck.Gen.t
end

module InputSet : sig
	type t
	val empty : t
	val is_empty : t -> bool
	val add : Input.t -> t -> t
	val remove : Input.t -> t -> t
	val mem : Input.t -> t -> bool
	val elements : t -> Input.t list
	val cardinal : t -> int
	val of_list : Input.t list -> t
	val diff : t -> t -> t
	val iter : (Input.t -> unit) -> t -> unit
	val fold : (Input.t -> 'a -> 'a) -> t -> 'a -> 'a
	val filter : (Input.t -> bool) -> t -> t
	val partition : (Input.t -> bool) -> t -> t * t
	val equal : t -> t -> bool
	val compare : t -> t -> int
	val pp : t Fmt.t
end

module PatchSet : sig
	type t
	val empty : t
	val is_empty : t -> bool
	val add : Patch.t -> t -> t
	val remove : Patch.t -> t -> t
	val mem : Patch.t -> t -> bool
	val elements : t -> Patch.t list
	val cardinal : t -> int
	val of_list : Patch.t list -> t
	val diff : t -> t -> t
	val iter : (Patch.t -> unit) -> t -> unit
	val fold : (Patch.t -> 'a -> 'a) -> t -> 'a -> 'a
	val filter : (Patch.t -> bool) -> t -> t
	val partition : (Patch.t -> bool) -> t -> t * t
	val equal : t -> t -> bool
	val compare : t -> t -> int
	val pp : t Fmt.t
end

module InputMap : sig
	type 'a t
	val empty : 'a t
	val is_empty : 'a t -> bool
	val add : Input.t -> 'a -> 'a t -> 'a t
	val singleton : Input.t -> 'a -> 'a t
	val remove : Input.t -> 'a t -> 'a t
	val merge : (Input.t -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
	val cardinal : 'a t -> int
	val bindings : 'a t -> (Input.t * 'a) list
	val find : Input.t -> 'a t -> 'a
	val find_opt : Input.t -> 'a t -> 'a option
	val mem : Input.t -> 'a t -> bool
	val iter : (Input.t -> 'a -> unit) -> 'a t -> unit
	val fold : (Input.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val map : ('a -> 'b) -> 'a t -> 'b t
	val mapi : (Input.t -> 'a -> 'b) -> 'a t -> 'b t
	val filter : (Input.t -> 'a -> bool) -> 'a t -> 'a t
	val filter_map : (Input.t -> 'a -> 'b option) -> 'a t -> 'b t
	val partition : (Input.t -> 'a -> bool) -> 'a t -> 'a t * 'a t
	val split : Input.t -> 'a t -> 'a t * 'a option * 'a t
	val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
	val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
	val for_all : (Input.t -> 'a -> bool) -> 'a t -> bool
	val exists : (Input.t -> 'a -> bool) -> 'a t -> bool
	val of_list : (Input.t * 'a) list -> 'a t
	val of_seq : (Input.t * 'a) Seq.t -> 'a t
	val add_seq : (Input.t * 'a) Seq.t -> 'a t -> 'a t
	val pp : 'a Fmt.t -> 'a t Fmt.t
	val gen : 'a QCheck.Gen.t -> 'a t QCheck.Gen.t
	val jsont : ?kind: string -> ?doc: string -> 'a Jsont.t -> 'a t Jsont.t
end

module PatchMap : sig
	type 'a t
	val empty : 'a t
	val is_empty : 'a t -> bool
	val add : Patch.t -> 'a -> 'a t -> 'a t
	val singleton : Patch.t -> 'a -> 'a t
	val remove : Patch.t -> 'a t -> 'a t
	val merge : (Patch.t -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
	val cardinal : 'a t -> int
	val bindings : 'a t -> (Patch.t * 'a) list
	val find : Patch.t -> 'a t -> 'a
	val find_opt : Patch.t -> 'a t -> 'a option
	val mem : Patch.t -> 'a t -> bool
	val iter : (Patch.t -> 'a -> unit) -> 'a t -> unit
	val fold : (Patch.t -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val map : ('a -> 'b) -> 'a t -> 'b t
	val mapi : (Patch.t -> 'a -> 'b) -> 'a t -> 'b t
	val filter : (Patch.t -> 'a -> bool) -> 'a t -> 'a t
	val filter_map : (Patch.t -> 'a -> 'b option) -> 'a t -> 'b t
	val partition : (Patch.t -> 'a -> bool) -> 'a t -> 'a t * 'a t
	val split : Patch.t -> 'a t -> 'a t * 'a option * 'a t
	val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
	val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
	val for_all : (Patch.t -> 'a -> bool) -> 'a t -> bool
	val exists : (Patch.t -> 'a -> bool) -> 'a t -> bool
	val of_list : (Patch.t * 'a) list -> 'a t
	val of_seq : (Patch.t * 'a) Seq.t -> 'a t
	val add_seq : (Patch.t * 'a) Seq.t -> 'a t -> 'a t
	val pp : 'a Fmt.t -> 'a t Fmt.t
	val gen : 'a QCheck.Gen.t -> 'a t QCheck.Gen.t
	val jsont : ?kind: string -> ?doc: string -> 'a Jsont.t -> 'a t Jsont.t
end
