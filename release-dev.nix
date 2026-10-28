#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
import ./release-lib.nix {
	system = builtins.currentSystem;
	bootstrap-nixpkgs = <nixpkgs>;
}
