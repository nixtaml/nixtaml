#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
final: prev: {
	nixtamal = final.lib.makeScope final.newScope (self: {
		nixtamal = self.callPackage ../package/nixtamal.nix { };

		default = final.symlinkJoin {
			name = "nixtamal";
			paths = with self.nixtamal; [
				bin
				data
				doc
				man
			];
		};
	});
}
