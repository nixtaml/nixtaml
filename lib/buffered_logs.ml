(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
(*
	The primary purpose for Buffered_logs at present is to handle that we need to
	hold off logging until *after* the TUI displays its content (instead of
	Logs.app). The TUI is meant to be a more visual, digestible experience for
	the lock/refresh commands — but if the logs are printed in its readline-like
	output, then the cursor position is now going to be off. The better
	experience is wait until the TUI UX is over & *then* show Logs.
*)
let buffer = Dynarray.create ()
let buffer_mutex = Eio.Mutex.create ()
let saved_level = ref None

let reporter = {
	Logs.report = (fun _src level ~over k msgf ->
		match Logs.level () with
		| Some min_level when level > min_level ->
			over ();
			k ()
		| _ ->
			let header_str = Fmt.str "%a" Logs_fmt.pp_header (level, None) in
			msgf (fun ?header: _ ?tags: _ fmt ->
				Fmt.kstr
					(fun msg_str ->
						let full_msg = Fmt.str "%s@ %s" header_str msg_str in
						Eio.Mutex.lock buffer_mutex;
						Dynarray.add_last buffer full_msg;
						Eio.Mutex.unlock buffer_mutex;
						over ();
						k ()
					)
					fmt
			)
	);
}

let flush () : unit =
	Eio.Mutex.lock buffer_mutex;
	Dynarray.iter (fun s -> Fmt.pr "%s@." s) buffer;
	Dynarray.clear buffer;
	Eio.Mutex.unlock buffer_mutex

let enable () : unit =
	Dynarray.clear buffer;
	saved_level := Logs.level ();
	Logs.set_reporter reporter

let disable () : unit =
	flush ();
	Logs.set_reporter (Logs_fmt.reporter ());
	(
		match !saved_level with
		| Some lvl -> Logs.set_level (Some lvl)
		| None -> ()
	);
	saved_level := None
