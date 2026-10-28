#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
	lib,
	runDashCommand,
	parallel,
	nixfmt,
	keepLogOrder ? true,
}:

runDashCommand
	{
		name = "check-nixfmt";
		runtimeInputs = [
			parallel
			nixfmt
		];
		env.src =
			let
				fs = lib.fileset;
			in
			fs.toSource {
				root = ../..;
				fileset =
					fs.intersection (fs.fileFilter (file: file.hasExt "nix") ../..)

						(fs.difference (fs.fromSource (lib.cleanSource ../..)) ../tamal);
			};
	}
	/* sh */ ''
		set -eu
		if [ -z "$src" ]; then
			echo "Missing \$src" >&2
			exit 1
		fi
		find "$src" -type f \
			| parallel --will-cite \
				${lib.optionalString keepLogOrder "--keep-order"} \
				--jobs "''${NIX_BUILD_CORES:-1}" \
				nixfmt --check {} \
			| tee "$out"
	''
