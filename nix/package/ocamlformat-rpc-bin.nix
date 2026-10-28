#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
# I don’t like ocamlformat as it doesn’t have tab support despite OCaml fully
# supporting tabs — so I use Topiary. However, the OCaml LSP requires
{
	lib,
	runCommand,
	ocamlformat,
}:

runCommand "extract-ocamlformat-rpc-bin" { } ''
	mkdir -p $out/bin
	cp ${lib.getBin ocamlformat}/bin/ocamlformat-rpc $out/bin/
''
