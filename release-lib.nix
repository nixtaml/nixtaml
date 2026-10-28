#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
	system ? builtins.currentSystem,
	bootstrap-nixpkgs ? null,
}:

let
	inputs = import ./nix/tamal { inherit system bootstrap-nixpkgs; };

	pkgs = import inputs.nixpkgs {
		overlays = [
			(import "${inputs.dash-nix}/nix/overlay")
			(import ./nix/overlay/default.nix)
			(import ./nix/overlay/development.nix)
			(import ./nix/overlay/check.nix)
		];
	};
in
{
	nixtamal = pkgs.nixtamal.default;

	default = pkgs.nixtamal.default;

	shell = pkgs.nixtamal.dev-shell;

	check = pkgs.nixtamal.check;
}
