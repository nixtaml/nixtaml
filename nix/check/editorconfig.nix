#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
	lib,
	runDashCommand,
	editorconfig-checker,
	nixtamal,
}:

runDashCommand
	{
		name = "check-EditorConfig";
		runtimeInputs = [
			editorconfig-checker
		];
		env.src =
			let
				fs = lib.fileset;
			in
			(fs.toSource {
				root = ../..;
				fileset = fs.unions [
					(fs.fileFilter (file: file.name == ".editorconfig") ../..)
					(fs.fromSource nixtamal.src)
					../../nix
				];
			});
	}
	/* sh */ ''
		set -eu
		if [ -z "$src" ]; then
			echo "Missing \$src" >&2
			exit 1
		fi
		cd "$src"
		editorconfig-checker >"$out"
	''
