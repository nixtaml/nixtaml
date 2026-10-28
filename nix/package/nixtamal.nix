#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
	lib,
	stdenv,
	python3Packages,
	ocamlPackages,
	darwin,
	makeBinaryWrapper,
	removeReferencesTo,
	installShellFiles,
	coreutils,
	curl,
	gawk,
	nix,
	nix-prefetch-darcs,
	nix-prefetch-git,
	nix-prefetch-fossil,
	nix-prefetch-pijul,
	testers,
	nixtamal,
}:

ocamlPackages.buildDunePackage {
	pname = "nixtamal";
	version = "2.3.0";
	release_year = 2026;

	src =
		let
			fs = lib.fileset;

			ocaml_project =
				file:
				lib.lists.elem file.name [
					"dune"
					"dune-project"
					"dune-workspace"
				]
				|| file.hasExt "opam";

			ocaml_src =
				file:
				lib.lists.any file.hasExt [
					"ml"
					"mld"
					"mli"
					"mly"
				];
		in
		fs.toSource {
			root = ../..;
			fileset = fs.difference (fs.unions [
				../../CHANGELOG.rst
				../../LICENSE.txt
				../../README.rst
				(fs.fileFilter (file: file.hasExt "txt") ../../license)
				(fs.fileFilter ocaml_project ../..)
				(fs.fileFilter ocaml_src ../../bin)
				(fs.fileFilter ocaml_src ../../lib)
				(fs.fileFilter ocaml_src ../../test)
				(fs.fileFilter (file: file.hasExt "jinja") ../../lib)
				../../doc
				../../meta
				../../ncl
				../../schema
				../../script
			]) (fs.maybeMissing ../../_build);
		};

	nativeBuildInputs = [
		makeBinaryWrapper
		removeReferencesTo
		installShellFiles
		# Compile-time preprocessing
		ocamlPackages.ppx_deriving
		# For manpages
		python3Packages.docutils
		# Completions
		ocamlPackages.cmdliner
		# NOTE: no KDL support
		python3Packages.pygments
	]
	++ lib.optional stdenv.hostPlatform.isDarwin darwin.sigtool;

	buildInputs = with ocamlPackages; [
		cmdliner
		fmt
	];

	propagatedBuildInputs = with ocamlPackages; [
		camomile
		eio
		eio_main
		eio_posix
		jingoo
		(jsont.override {
			withBrr = false;
			withBytesrw = true;
			withTypegist = false;
		})
		kdl
		logs
		ppx_blob
		saturn
		stdint
		xdg
	];

	nativeCheckInputs = [
		nix
	];

	checkInputs = with ocamlPackages; [
		alcotest
		ppx_deriving_qcheck
		qcheck
		qcheck-alcotest
		qcheck-core
	];

	outputs = [
		"bin"
		"data"
		"doc"
		"lib"
		"man"
		"out"
	];

	postPatch = ''
		substituteInPlace bin/main.ml --subst-var version
		substituteInPlace lib/lock_loader.ml --subst-var release_year
	'';

	doCheck = true;

	installPhase = ''
		runHook preInstall

		dune install \
			-j "$NIX_BUILD_CORES" \
			--cache="disabled" \
			--prefix="$out" \
			--bindir="$bin/bin" \
			--datadir="$data/share" \
			--docdir="$doc/share/doc" \
			--mandir="$man/share/man" \
			--libdir="$lib/lib/ocaml/${ocamlPackages.ocaml.version}/site-lib" \
			nixtamal

		cp -r "$src/meta" "$src/ncl" "$data/share"/*/

		for dep in "${ocamlPackages.ocaml}" "${ocamlPackages.camomile}"; do
			remove-references-to -t "$dep" "$bin/bin/nixtamal"
		done

		wrapProgram "$bin/bin/nixtamal" --prefix PATH : ${
			lib.makeBinPath [
				# coreutils required since the prefetcher scripts presently
				# don’t specify all of their inputs
				coreutils
				# Supported Nix prefetch scripts
				nix-prefetch-darcs
				nix-prefetch-fossil
				nix-prefetch-git
				nix-prefetch-pijul
				# cURL + gawk are used for the default_fresh_cmd for files &
				# archives
				curl
				gawk
			]
		}

		${lib.optionalString (stdenv.buildPlatform.canExecute stdenv.hostPlatform) /* sh */ ''
			mkdir -p "$TMPDIR"
			cmdliner tool-completion --standalone-completion bash nixtamal >"$TMPDIR/completion.bash"
			cmdliner tool-completion --standalone-completion zsh nixtamal >"$TMPDIR/completion.zsh"
			substituteInPlace "$TMPDIR/completion.zsh" --replace-fail "_nixtamal_cmdliner" "_nixtamal"

			installShellCompletion --bash --cmd nixtamal "$TMPDIR/completion.bash"
			installShellCompletion --fish --cmd nixtamal "script/completion.fish"
			installShellCompletion --zsh --cmd nixtamal "$TMPDIR/completion.zsh"
		''}

		runHook postInstall
	'';

	passthru.tests.version = testers.testVersion {
		package = nixtamal.bin;
		command = "${nixtamal.meta.mainProgram} --version";
	};

	meta = {
		license = with lib.licenses; [ gpl3Plus ];
		platforms = lib.platforms.unix;
		mainProgram = "nixtamal";
		outputsToInstall = [
			"bin"
			"data"
			"doc"
			"man"
		];
		homepage = "https://nixtamal.toast.al";
		description = "Fulfilling input pinning for Nix";
		longDescription = ''
			• Automate the manual work of input pinning for dependency management
			• Allow easy ways to lock & refresh those inputs
			• Declarative manifest file over imperative CLI flags
			• diff/grep-friendly lockfile
			• Declarative patch/diff management for inputs
			• Host-, forge-, VCS-agnostic
			• Choose eval time fetchers (builtins) or build time fetchers (Nixpkgs, default) — which opens up fetching now-supported Darcs, Pijul, & Fossil
			• Supports mirrors, failing over when a server is down
			• Override hash algorithm on a per-project & per-input basis — including BLAKE3 support
			• Custom freshness commands
			• No experimental Nix features required
		'';
	};
}
