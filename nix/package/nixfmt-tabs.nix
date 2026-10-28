#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
	lib,
	makeBinaryWrapper,
	stdenvNoCC,
	nixfmt,
}:

let
	nixfmt-patched = nixfmt.overrideAttrs (old: {
		patches = (old.patches or [ ]) ++ [
			../patch/nixfmt-with-tabs.patch
		];
	});
in
stdenvNoCC.mkDerivation {
	name = "nixfmt";

	preferLocalBuild = true;

	nativeBuildInputs = [ makeBinaryWrapper ];

	dontUnpack = true;

	installPhase = ''
		mkdir -p "$out/bin"
		makeBinaryWrapper "${lib.getExe nixfmt-patched}" "$out/bin/nixfmt" \
			--add-flags "--tabs"
		cp -Tr "${nixfmt}/share" "$out/share"
	'';

	meta = {
		description = "nixfmt, but with tab indentation";
		longDescription = ''
			I don’t like *this* formatter, but it is *a* formatter. Tweaking it to
			expand to tab support, helps the maker actually read the code since 2
			spaces is not enough contrast & spaces can’t be configurable.
		'';
	};
}
