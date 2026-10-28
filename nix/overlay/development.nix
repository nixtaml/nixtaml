#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
final: prev: {
	nixtamal = prev.nixtamal.overrideScope (
		final': prev': {
			dev-shell = final'.callPackage ../package/dev-shell.nix { };

			nixfmt-tabs = final'.callPackage ../package/nixfmt-tabs.nix { };

			topiary = final'.callPackage ../package/topiary-with-config.nix { inherit (final) topiary; };

			ocamlformat-rpc-bin = final'.callPackage ../package/ocamlformat-rpc-bin.nix { };

			ocamlPackages = (prev'.ocamlPackages or prev.ocamlPackages).overrideScope (
				ofinal: oprev:
				let
					unsafe-dune-downgrade-to-3_23 = /* bash */ ''
						substituteInPlace dune-project \
							--replace-fail "dune 3.24" "dune 3.23"
					'';
				in
				{
					jsonrpc = oprev.jsonrpc.overrideAttrs (old: {
						src = ofinal.ocaml-lsp.src;
						postPatch = (old.postPatch or "") + unsafe-dune-downgrade-to-3_23;
					});

					lsp = oprev.lsp.overrideAttrs (old: {
						src = ofinal.ocaml-lsp.src;
						postPatch = (old.postPatch or "") + unsafe-dune-downgrade-to-3_23;
					});

					ocaml-lsp = oprev.ocaml-lsp.overrideAttrs (old: {
						src = final.fetchFromGitHub {
							owner = "toastal";
							repo = "ocaml-lsp";
							rev = "20a78f1bd284de972e9069a4d1069f3d94b04ca9";
							hash = "sha256-Zf9eCBlBaYhYmjKPSLS5wMH4mB8HgPsiHQ7aJ6kuktA=";
						};
						postPatch = (old.postPatch or "") + unsafe-dune-downgrade-to-3_23;
					});
				}
			);
		}
	);
}
