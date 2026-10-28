(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
let needs_shell (str : string) : bool =
	(* command contains whitespace such as

		VISUAL="kate --new-window"
		EDITOR="SHELL=/run/current-system/sw/bin/fish nvim"
	*)
	String.exists
		(function
			| ' ' | '\t' | '\n' | '\r' -> true
			| _ -> false
		)
		str

let find_env : string Lazy.t =
	lazy(
		match Sys.getenv_opt "VISUAL" with
		| Some v -> v
		| None ->
			match Sys.getenv_opt "EDITOR" with
			| Some e -> e
			| None -> "vi"
	)

let run_on file : unit =
	let ed = Lazy.force find_env in
	if needs_shell ed then
		Unix.execvp "/bin/sh" [|"/bin/sh"; "-c"; ed ^ " " ^ Filename.quote file|]
	else
		Unix.execvp ed [|ed; file|]
