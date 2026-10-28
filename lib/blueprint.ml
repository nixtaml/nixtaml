(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
type t =
	| Release
	| Release_split
[@@deriving show, eq, enum, qcheck]

let to_string = function
	| Release -> "release"
	| Release_split -> "release-split"

let of_string = function
	| "release" -> Ok Release
	| "release-split" -> Ok Release_split
	| s -> Error (Fmt.str "Unknown blueprint: %s" s)

let pp ppf t = Fmt.string ppf (to_string t)

let hr n =
	let uchar = Uchar.of_int 0x2500
	and buf = Buffer.create (n + 2)
	in
	Buffer.add_char buf '#';
	for _ = 1 to n do
		Buffer.add_utf_8_uchar buf uchar
	done;
	buf

let banner_SPDX ?identity: identity_opt ~year () =
	let identity : VCS.Identity.t =
		match identity_opt with
		| None | Some VCS.Identity.{name = None; contact = None} ->
			{name = Some "NAME"; contact = Some "CONTACT"}
		| Some id ->
			id
	and buf = Buffer.create 256
	in
	let pad content =
		let width = 77 in
		Buffer.add_string buf content;
		let fill = width - String.length content in
		if fill >= 0 then
			(
				Buffer.add_string buf (String.make fill ' ');
				Buffer.add_string buf "│"
			)
	in
	Buffer.add_buffer buf (hr 78);
	Buffer.add_string buf "┐\n# ";
	pad (
		Fmt.str
			"SPDX-FileCopyrightText: %d%a%a"
			year
			(fun ppf ->
				function
					| Some n -> Fmt.pf ppf " %s" n
					| None -> ()
			)
			identity.name
			(fun ppf ->
				function
					| Some c -> Fmt.pf ppf " <%s>" c
					| None -> ()
			)
			identity.contact
	);
	Buffer.add_string buf "\n# ";
	pad "SPDX-License-Identifier:";
	Buffer.add_string buf "\n";
	Buffer.add_buffer buf (hr 78);
	Buffer.add_string buf "┘\n";
	buf

let release_body =
	{|{
	inherit (pkgs.nixtamal-demo)
		demo
		;

	default = pkgs.nixtamal-demo.demo;
	shell = pkgs.nixtamal-demo.dev-shell;
}
|}

let release_nix ~banner =
	let buf = Buffer.create 512 in
	Buffer.add_buffer buf banner;
	Buffer.add_string
		buf
		{|let
	inputs = import ./nix/tamal { };

	pkgs = import inputs.nixpkgs {
		overlays = [
			(import ./nix/overlay)
			(import ./nix/overlay/development.nix)
		];
	};
in
|};
	Buffer.add_string buf (release_body);
	buf

let release_lib_nix ~banner =
	let buf = Buffer.create 512 in
	Buffer.add_buffer buf banner;
	Buffer.add_string
		buf
		{|{
	system ? builtins.currentSystem,
	bootstrap-nixpkgs ? null,
}:

let
	inputs = import ./nix/tamal { inherit system bootstrap-nixpkgs; };

	pkgs = import inputs.nixpkgs {
		overlays = [
			(import ./nix/overlay/default.nix)
			(import ./nix/overlay/development.nix)
		];
	};
in
|};
	Buffer.add_string buf release_body;
	buf

let release_nix_split ~banner =
	let buf = Buffer.create 512 in
	Buffer.add_buffer buf banner;
	Buffer.add_string buf "import ./release-lib.nix { }\n";
	buf

let release_dev_nix ~banner =
	let buf = Buffer.create 512 in
	Buffer.add_buffer buf banner;
	Buffer.add_string
		buf
		{|import ./release-lib.nix {
	system = builtins.currentSystem;
	bootstrap-nixpkgs = <nixpkgs>;
}
|};
	buf

let default_nix ~banner =
	let buf = Buffer.create 512 in
	Buffer.add_buffer buf banner;
	Buffer.add_string buf "(import ./release.nix).default\n";
	buf

let shell_nix ~banner =
	let buf = Buffer.create 512 in
	Buffer.add_buffer buf banner;
	Buffer.add_string buf "(import ./release.nix).shell\n";
	buf

let shell_nix_split ~banner =
	let buf = Buffer.create 512 in
	Buffer.add_buffer buf banner;
	Buffer.add_string buf "(import ./release-dev.nix).shell\n";
	buf

let overlay_default_nix ~banner =
	let buf = Buffer.create 512 in
	Buffer.add_buffer buf banner;
	Buffer.add_string
		buf
		{|final: prev: {
	nixtamal-demo = final.lib.makeScope final.newScope (self: {
		demo = self.callPackage ../package/demo.nix { };
	});
}
|};
	buf

let overlay_development_nix ~banner =
	let buf = Buffer.create 512 in
	Buffer.add_buffer buf banner;
	Buffer.add_string
		buf
		{|final: prev: {
	nixtamal-demo = prev.nixtamal-demo.overrideScope (
		final': prev': {
			dev-shell = final'.callPackage ../package/dev-shell.nix { };
		}
	);
}
|};
	buf

let demo_nix ~banner =
	let buf = Buffer.create 512 in
	Buffer.add_buffer buf banner;
	Buffer.add_string
		buf
		{|{ lib, stdenv, writeText }:

stdenv.mkDerivation (finalAttrs: {
	pname = "demo";
	version = "0-dev";

	# TODO: Replace with lib.fileset for precise source filtering:
	# src = lib.fileset.toSource {
	# 	root = ../..;
	# 	fileset = lib.fileset.unions [ ../../src ../../LICENSE.txt ];
	# };
	src = lib.cleanSource ../..;

	__structuredAttrs = true;
	strictDeps = true;

	buildPhase = ''
		runHook preBuild
		cc -o demo ${writeText "demo.c" /* c */ ''
			#include <stdio.h>
			int main(void) {
				printf("Hello from Nixtamal demo package v${finalAttrs.version}!\n");
				return 0;
			}
		''}
		runHook postBuild
	'';

	installPhase = ''
		runHook preInstall
		install -Dm755 demo $out/bin/demo
		runHook postInstall
	'';

	meta = {
		description = "Nixtamal Demo package";
		license = with lib.licenses; [ ];
		mainProgram = "demo";
	};
})
|};
	buf

let dev_shell_nix ~banner =
	let buf = Buffer.create 512 in
	Buffer.add_buffer buf banner;
	Buffer.add_string
		buf
		{|{ mkShell, demo, kdlfmt }:

mkShell {
	name = "nixtamal-demo";
	inputsFrom = [ demo ];
	packages = [
		# for Nixtamal’s manifest.kdl
		kdlfmt
	];
}
|};
	buf

let directories ~cwd =
	let nix_dir = Eio.Path.(cwd / "nix") in
	function
		| Release | Release_split ->
			[|
				nix_dir;
				Eio.Path.(nix_dir / "overlay");
				Eio.Path.(nix_dir / "package")
			|]

let files ~cwd ?identity blueprint : (_ Eio.Path.t * Buffer.t) array =
	let year : int = 1900 + (Unix.gmtime (Unix.gettimeofday ())).Unix.tm_year in
	let banner = banner_SPDX ?identity ~year ()
	and nix_dir = Eio.Path.(cwd / "nix")
	in
	match blueprint with
	| Release ->
		[|
			Eio.Path.(cwd / "release.nix"), release_nix ~banner;
			Eio.Path.(cwd / "default.nix"), default_nix ~banner;
			Eio.Path.(cwd / "shell.nix"), shell_nix ~banner;
			Eio.Path.(nix_dir / "overlay" / "default.nix"), overlay_default_nix ~banner;
			Eio.Path.(nix_dir / "overlay" / "development.nix"), overlay_development_nix ~banner;
			Eio.Path.(nix_dir / "package" / "demo.nix"), demo_nix ~banner;
			Eio.Path.(nix_dir / "package" / "dev-shell.nix"), dev_shell_nix ~banner;
		|]
	| Release_split ->
		[|
			Eio.Path.(cwd / "release-lib.nix"), release_lib_nix ~banner;
			Eio.Path.(cwd / "release.nix"), release_nix_split ~banner;
			Eio.Path.(cwd / "release-dev.nix"), release_dev_nix ~banner;
			Eio.Path.(cwd / "default.nix"), default_nix ~banner;
			Eio.Path.(cwd / "shell.nix"), shell_nix_split ~banner;
			Eio.Path.(nix_dir / "overlay" / "default.nix"), overlay_default_nix ~banner;
			Eio.Path.(nix_dir / "overlay" / "development.nix"), overlay_development_nix ~banner;
			Eio.Path.(nix_dir / "package" / "demo.nix"), demo_nix ~banner;
			Eio.Path.(nix_dir / "package" / "dev-shell.nix"), dev_shell_nix ~banner;
		|]

let set_up_and_write ~env (blueprint : t) : (unit, exn) result =
	try
		let cwd = Eio.Stdenv.cwd env in
		Logs.app (fun m -> m "Setting up “%a” blueprint" pp blueprint);
		Array.iter
			(fun path ->
				Logs.info (fun m -> m "Making directory %a …" Eio.Path.pp path);
				Eio.Path.mkdirs ~exists_ok: true ~perm: 0o755 path
			)
			(directories ~cwd blueprint);
		let identity = VCS.Identity.get ~env in
		Array.iter
			(fun (path, buf) ->
				match Eio.Path.kind ~follow: false path with
				(* clobbering would be bad UX :] *)
				| `Not_found ->
					Logs.info (fun m -> m "Writing to %a …" Eio.Path.pp path);
					Eio.Path.save ~follow: false ~create: (`Exclusive 0o664) path (Buffer.contents buf)
				| _ ->
					Logs.warn (fun m ->
						m "File %a cannot be written; already exists" Eio.Path.pp path
					)
			)
			(files ~cwd ?identity blueprint);
		Logs.app (fun m -> m "\nBuild the demo:");
		Logs.app (fun m -> m "\t$ nix-build release.nix -A demo ");
		Logs.app (fun m -> m "Then run the demo:");
		Logs.app (fun m -> m "\t$ result/bin/demo");
		Ok ()
	with
		| e -> Error e
