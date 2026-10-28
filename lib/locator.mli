(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
(*
	MERGE NOTE (community fork): Security hardening from uRI.ml has been ported
	to locator.ml — scheme whitelist & path traversal detection. The error type
	has been extended with `Invalid_scheme & `Path_traversal.
*)

type t

val make : string -> t
val take : t -> string
val equal : t -> t -> bool
val compare : t -> t -> int
val pp : t Fmt.t
val show : t -> string

type error = [
	| `Empty_locator
	| `Invalid_scheme of string
	| `Path_traversal of string
]

val pp_error : error Fmt.t

val parse : string -> (t, error) result
val is_valid : string -> bool

val jsont : t Jsont.t

val gen : t QCheck.Gen.t
