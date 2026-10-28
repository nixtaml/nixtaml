#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
	mkShell,
	editorconfig-checker,
	kdlfmt,
	topiary,
	ocamlPackages,
	nixtamal,
	nixfmt-tabs,
	ocamlformat-rpc-bin,
}:

mkShell {
	name = "nixtamal";
	inputsFrom = [
		nixtamal
	];
	packages = [
		editorconfig-checker
		kdlfmt
		nixfmt-tabs
		topiary

		ocamlPackages.alcotest
		ocamlPackages.qcheck
		ocamlPackages.qcheck-alcotest
		ocamlPackages.ocaml-lsp
		ocamlformat-rpc-bin # 💢 why does the LSP depend on ocamlformat‽
	];
}
