{
	lib,
	writeDashScriptBin,
	coreutils,
	nix,
	nixtamal,
	findutils,
	git,
}:

let
	fs = lib.fileset;

	tamal = fs.toSource {
		root = ../.;
		fileset = fs.unions [
			../tamal/default.nix
			../tamal/lock.json
			../tamal/manifest.kdl
			(fs.maybeMissing ../tamal/darcs_context)
		];
	};

	PATH = lib.makeBinPath [
		coreutils
		findutils
		nix
		nixtamal
		git # required for git ls-remote
	];
in
writeDashScriptBin {
	name = "check-e2e-blueprint";
	text = /* sh */ ''
		set -eu
		export PATH="${PATH}"

		proj="$(mktemp -d)"
		trap 'rm -rf "$proj"' EXIT

		export NIXTAMAL_DIRECTORY="$proj/nix/tamal"
		mkdir -p "$NIXTAMAL_DIRECTORY"
		cp -Tr --dereference --no-preserve=all "${tamal}/tamal" "$NIXTAMAL_DIRECTORY"

		cd "$proj"

		nixtamal set-up --verbose --nixpkgs-channel nixos-unstable --blueprint release-split

		nix-build release-dev.nix --no-out-link
	'';
}
