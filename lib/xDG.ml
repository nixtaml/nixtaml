(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
(* fix naming & extend XDG module *)
include Xdg

let base = create ~env: Sys.getenv_opt ()

let nixtamal_cache_dir env =
	let proc_mgr = Eio.Stdenv.process_mgr env in
	let cache_root =
		match Eio.Process.getenv_opt proc_mgr "XDG_CACHE_HOME" with
		| Some d -> d
		| None ->
			let home = home_dir base in
			Filename.concat home ".cache"
	in
	Eio.Path.(Eio.Stdenv.fs env / cache_root / "nixtamal")

let check_passed_dir env =
	Eio.Path.(nixtamal_cache_dir env / "check_passed")
