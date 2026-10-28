#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
	lib,
	runDashCommand,
	parallel,
	nickel,
}:

runDashCommand
	{
		name = "check-meta-nickel";
		runtimeInputs = [
			parallel
			nickel
		];
		env.src =
			let
				fs = lib.fileset;
			in
			fs.toSource {
				root = ../..;
				fileset = fs.unions [
					(fs.fileFilter (file: file.hasExt "ncl") ../../meta)
					(fs.fileFilter (file: file.hasExt "ncl") ../../ncl)
				];
			};
	}
	/* sh */ ''
		set -eu
		if [ -z "$src" ]; then
			echo "Missing \$src" >&2
			exit 1
		fi
		cd "$src"
		find meta -type f -name '*.ncl' \
			| parallel --will-cite \
				--halt now,fail=1 \
				--jobs "''${NIX_BUILD_CORES:-1}" \
				nickel eval {} \
			| tee "$out"
	''
