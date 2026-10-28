#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
	lib,
	makeBinaryWrapper,
	stdenvNoCC,
	topiary,
	writeText,
}:

let
	cfg = writeText "topairy-cfg.ncl" /* nickel */ ''
		{
			languages = {
				bash.indent | priority 1 = "\t",
				css.indent | priority 1 = "\t",
				json.indent | priority 1 = "\t",
				nickel.indent | priority 1 = "\t",
				ocaml.indent | priority 1 = "\t",
				ocaml_interface.indent | priority 1 = "\t",
				ocamllex.indent | priority 1 = "\t",
				openscad.indent | priority 1 = "\t",
				rust.indent | priority 1 = "\t",
				sdml.indent | priority 1 = "\t",
				toml.indent | priority 1 = "\t",
			},
		}
	'';
in
stdenvNoCC.mkDerivation {
	name = "topiary";

	preferLocalBuild = true;

	nativeBuildInputs = [ makeBinaryWrapper ];

	dontUnpack = true;

	installPhase = ''
		mkdir -p "$out/bin"
		makeBinaryWrapper "${lib.getExe topiary}" "$out/bin/topiary" \
			--add-flags "-C ${cfg}"
		cp -Tr "${topiary}/share" "$out/share"
	'';

	meta = {
		description = "Topiary with a tab-indented config";
	};
}
