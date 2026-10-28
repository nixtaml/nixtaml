(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
(* Whether something was fetched this run (and where it landed), or left
   untouched. Own module (rather than nested in [Store_path]) since the
   status *contains* a store path, not the other way around. *)
type t =
	| Unfetched
	| Fetched of Store_path.t
[@@deriving show, eq, qcheck]

let default = Unfetched
