#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
final: prev: {
	nixtamal = prev.nixtamal.overrideScope (
		final': prev': {
			check = final.lib.makeScope final'.newScope (self: {
				recurseForDerivations = true;

				format = {
					EditorConfig = self.callPackage ../check/editorconfig.nix { };

					nixfmt = self.callPackage ../check/nixfmt.nix { };
				};

				meta = {
					nickel = self.callPackage ../check/meta.nix { };
				};

				e2e = {
					assert-blueprint = self.callPackage ../check/assert-blueprint.nix { };
				};
			});
		}
	);
}
