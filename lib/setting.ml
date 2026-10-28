(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
let gc_enabled : unit -> bool =
	let enabled =
		lazy(
			begin
				match Sys.getenv_opt "NIXTAMAL_EXPERIMENTAL_GC" with
				| None -> false
				| Some "" | Some "0" | Some "false" | Some "no" -> false
				| Some _ -> true
			end
		)
	in
	fun () -> Lazy.force enabled
