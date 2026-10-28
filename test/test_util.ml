(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
let tmp_dir ~env =
	let fs = Eio.Stdenv.fs env in
	let cwd = Eio.Stdenv.cwd env in
	match Sys.getenv_opt "TMPDIR" with
	| Some d -> Eio.Path.(fs / d)
	| None ->
		match Sys.getenv_opt "XDG_RUNTIME_DIR" with
		| Some d -> Eio.Path.(fs / d / "nixtamal")
		| None ->
			try
				Eio.Path.(fs / "tmp" / "nixtamal")
			with
				| Eio.Io _ -> Eio.Path.(cwd / ".tmp")
