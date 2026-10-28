#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
final: prev: {
	nixtamal = prev.nixtamal.overrideScope (
		final': prev': {
			check = (prev'.check or { }) // {
				e2e = {
					blueprint = final'.callPackage ../check/blueprint.nix { };
				};
			};
		}
	);
}
