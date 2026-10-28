(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
type jg_models2 = string -> Jingoo.Jg_types.tvalue

module Template = struct
	type t =
		Template of UTF8.t
	[@@unboxed]
	[@@deriving show, eq, qcheck]

	let [@inline]make t = Template t
	let [@inline]take (Template t) = t
	let [@inline]fill ~(models : jg_models2) tpl =
		Jingoo.Jg_template2.from_string ~models (take tpl)
end

module Fresh = struct
	module Cmd = struct
		type cmd = {
			prog: Template.t;
			args: Template.t list;
		}
		[@@deriving show, eq, make, qcheck]

		type t = cmd Util.Non_empty_list.t
		[@@deriving show, eq, qcheck]

		let (~$) x = (x, [])
		let (|:) (x, xs) x' = (x, x' :: xs)
		let (@) (x, xs) (y, ys) = (x, xs @ y :: ys)
	end

	type t = {
		cmd: Cmd.t option;
		value: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]
end

let default_fresh_awk_script : Template.t =
	Template.make
		(* awk *)
		{|BEGIN {
	while ((getline) > 0) {
		if (/^[Ee][Tt]ag:/) {
			sub(/^[^:]+: */, "")
			gsub(/"/, "")
			print
			exit
		}
		if (/^[Ll]ink:.*rel="immutable"/) {
			sub(/^[^<]*</, "")
			sub(/>.*$/, "")
			print
			exit
		}
	}
	print "Neither Link rel=\"immutable\" nor ETag HTTP header was found for the resource & will require a custom fresh-cmd" > "/dev/stderr"
	exit 1
}|}

module Fetch_time = struct
	type t =
		| Build
		| Eval
	[@@deriving show, eq, enum, qcheck]
	(** Eval time will force the builtin fetchers. Build time will use those
		from Nixpkgs (& are required for certain features like patches. There are
		pros/cons to both, but we will allow it to stated instead of assumed. *)

	let to_string = function
		| Build -> "build"
		| Eval -> "eval"

	let of_string = function
		| "build" | "Build" -> Some Build
		| "eval" | "Eval" -> Some Eval
		| _ -> None

	let default : t ref = ref Build
	(* Build is probably the most ‘expected’ & generally performs better *)

	let value : t option -> t = function
		| None -> !default
		| Some ftg -> ftg
end

module GC = struct
	type t =
		| Track
		| Skip
	[@@deriving show, eq, enum, qcheck]

	let to_string = function
		| Track -> "track"
		| Skip -> "skip"

	let of_string = function
		| "track" | "Track" -> Some Track
		| "skip" | "Skip" -> Some Skip
		| _ -> None

	let default : t ref = ref Skip

	let value : t option -> t = function
		| None -> !default
		| Some gc -> gc
end

(* KINDS **********************************************************************)

module File = struct
	type t = {
		fetch_time: Fetch_time.t option;
		url: Template.t;
		mirrors: Template.t list;
	}
	[@@deriving show, eq, make, qcheck]

	let default_fresh_cmd (url : Template.t) : Fresh.Cmd.t =
		let open Fresh.Cmd in
		let t = Template.make in
		~${prog = t "curl"; args = [t "-LIs"; t "--http2"; url]}
		|: {prog = t "awk"; args = [default_fresh_awk_script]}
end

module Archive = struct
	type t = {
		fetch_time: Fetch_time.t option;
		url: Template.t;
		mirrors: Template.t list;
		extension: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]

	let default_fresh_cmd (url : Template.t) : Fresh.Cmd.t =
		let open Fresh.Cmd in
		let t = Template.make in
		~${prog = t "curl"; args = [t "-LIs"; t "--http2"; url]}
		|: {prog = t "awk"; args = [default_fresh_awk_script]}
end

module Git = struct
	module Reference = struct
		type t = [
			| `Branch of UTF8.t
			| `Ref of UTF8.t
			| `Tag of UTF8.t
		]
		[@@deriving show, eq, qcheck]
	end

	type t = {
		fetch_time: Fetch_time.t option;
		repository: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
		datetime: UTF8.t option; (* ISO 8601 RFC 3339 *)
		submodules: bool; [@default false]
		lfs: bool; [@default false]
		latest_revision: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]

	let default_fresh_cmd git : Fresh.Cmd.t =
		let open Fresh.Cmd in
		let t = Template.make in
		let git_ls_remote (ls_remote_args : Template.t list) : t =
			let args = t "ls-remote" :: git.repository :: ls_remote_args in
			~${prog = t "git"; args}
			|: {prog = t "cut"; args = [t "-f1"]}
		in
		match git.reference with
		| `Branch b -> git_ls_remote [t "--branches"; t b]
		| `Ref r -> git_ls_remote [t "--refs"; t r]
		| `Tag r -> git_ls_remote [t "--tags"; t r]
end

module Darcs = struct
	module Reference = struct
		type context_grounds = [
			| `Assumed of UTF8.t option
			| `Stated of UTF8.t
		]
		[@@deriving show, eq, qcheck]

		type t = [
			| `Context of context_grounds
			| `Tag of UTF8.t
		]
		[@@deriving show, eq, qcheck]
	end

	type t = {
		repository: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
		datetime: UTF8.t option; (* ISO 8601 RFC 3339 *)
		latest_weak_hash: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]
end

module Pijul = struct
	module Reference = struct
		type t = [
			| `Channel of UTF8.t
			| `State of UTF8.t
			| `Change of UTF8.t
		]
		[@@deriving show, eq, qcheck]
	end

	type t = {
		remote: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
		datetime: UTF8.t option; (* ISO 8601 RFC 3339 *)
		latest_state: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]
end

module Fossil = struct
	module Reference = struct
		type t = [
			| `Branch of UTF8.t
			| `Tag of UTF8.t
			| `Checkin of UTF8.t
		]
		[@@deriving show, eq, qcheck]
	end

	type t = {
		repository: Template.t;
		reference: Reference.t;
		date: UTF8.t option;
		latest_checkin: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]
end

(* HASH ***********************************************************************)

module Hash = struct
	type algorithm =
		| SHA_256 [@value 0]
		| SHA_512
		| BLAKE3
	[@@deriving enum, eq, ord, show, qcheck]

	let algorithm_to_string = function
		| SHA_256 -> "SHA-256"
		| SHA_512 -> "SHA-512"
		| BLAKE3 -> "BLAKE3"

	let algorithm_to_hash_token = function
		| SHA_256 -> "sha256"
		| SHA_512 -> "sha512"
		| BLAKE3 -> "blake3"

	(* handles correct name, hash token name, & misspellings *)
	let algorithm_of_string = function
		| "SHA-256" | "sha256" | "SHA256" -> Some SHA_256
		| "SHA-512" | "sha512" | "SHA512" -> Some SHA_512
		| "BLAKE3" | "blake3" -> Some BLAKE3
		| _ -> None

	(* implements pkgs.lib.fakeHash, but avoids pkgs.lib *)
	let fake_hash (algo : algorithm) =
		Fmt.str "%s-%s" (algorithm_to_hash_token algo) @@
			match algo with
			(* length 44 *)
			| SHA_256 | BLAKE3 -> "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
			(* length 88 *)
			| SHA_512 -> "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=="

	(* many of the builtin fetchers may only work with SHA-256 *)
	let default_algorithm : algorithm ref = ref SHA_256

	type t = {
		algorithm: algorithm;
			[@default !default_algorithm]
		(* None is for not yet calculated *)
		value: UTF8.t option;
		(* used to assert in fetching for manually-updated pins *)
		expected: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]
end

(* INPUT *******************************************************************)

module Kind = struct
	type t = [
		| `File of File.t
		| `Archive of Archive.t
		| `Git of Git.t
		| `Darcs of Darcs.t
		| `Pijul of Pijul.t
		| `Fossil of Fossil.t
	]
	[@@deriving show, eq, qcheck]

	let to_string : t -> string = function
		| `File _ -> "File"
		| `Archive _ -> "Archive"
		| `Git _ -> "Git"
		| `Darcs _ -> "Darcs"
		| `Pijul _ -> "Pijul"
		| `Fossil _ -> "Fossil"
end

let make_kind_file ~url ?mirrors () =
	`File (File.make ~url ?mirrors ())

let make_kind_archive ~url ?mirrors ?extension () =
	`Archive (Archive.make ~url ?mirrors ?extension ())

let make_kind_darcs ~repository ?mirrors ~reference ?latest_weak_hash () =
	`Darcs (Darcs.make ~repository ?mirrors ~reference ?latest_weak_hash ())

let make_kind_pijul ~remote ?mirrors ~reference ?latest_state () =
	`Pijul (Pijul.make ~remote ?mirrors ~reference ?latest_state ())

let make_kind_git ~repository ?mirrors ~reference ?latest_revision ?submodules ?lfs () =
	`Git (Git.make ~repository ?mirrors ~reference ?latest_revision ?submodules ?lfs ())

let make_kind_fossil ~repository ~reference ?latest_checkin () =
	`Fossil (Fossil.make ~repository ~reference ?latest_checkin ())

type t = {
	name: Name.Input.t;
	store_name: Name.Store.t option;
	fetch_status: Fetch_status.t; [@default Fetch_status.default]
	kind: Kind.t;
	fresh: Fresh.t; [@default Fresh.make ()]
	hash: Hash.t; [@default Hash.make ()]
	frozen: bool; [@default false]
	gc: GC.t option;
	patches: Name.Patch.t list; [@default []]
}
[@@deriving show, eq, make, qcheck]

let derive_store_name (name : Name.Input.t) : Name.Store.t option =
	let derived_name = Name.Input.take name ^ "-src" in
	match Name.Store.parse derived_name with
	| Ok store_name ->
		Some store_name
	| Error (`Invalid_store_name _) ->
		Logs.warn (fun m -> m "Derived store name “%s” from input is invalid for the Nix store ([-.+_?=0-9a-zA-Z]+)" derived_name);
		None

let derive_patch_store_name (name : Name.Patch.t) : Name.Store.t option =
	let derived_name = Name.Patch.take name ^ "-patch" in
	match Name.Store.parse derived_name with
	| Ok store_name ->
		Some store_name
	| Error (`Invalid_store_name _) ->
		Logs.warn (fun m -> m "Derived store name “%s” from patch is invalid for the Nix store ([-.+_?=0-9a-zA-Z]+)" derived_name);
		None

let fresh_cmd (input : t) : Fresh.Cmd.t option =
	match input.fresh.cmd with
	| Some cmd -> Some cmd
	| None ->
		(
			match input.kind with
			| `File f -> Some (File.default_fresh_cmd f.url)
			| `Archive a -> Some (Archive.default_fresh_cmd a.url)
			| `Git g -> Some (Git.default_fresh_cmd g)
			(* As far as we can tell, the other kinds do not have a reasonable,
				consistent way to gauge freshness
			*)
			| _ -> None
		)

(* Some features will require certain fetch times *)
let set_fetch_time (input : t) : t =
	if List.is_empty input.patches then
		input
	else
		match input.kind with
		| `File file ->
			Logs.debug (fun m ->
				m "Setting fetch time to “build” for %a" Name.Input.pp input.name
			);
			{input with
				kind = `File {file with fetch_time = Some Fetch_time.Build}}
		| `Archive archive ->
			Logs.debug (fun m ->
				m "Setting fetch time to “build” for %a" Name.Input.pp input.name
			);
			{input with
				kind = `Archive {archive with fetch_time = Some Fetch_time.Build}}
		| `Git git ->
			Logs.debug (fun m ->
				m "Setting fetch time to “build” for %a" Name.Input.pp input.name
			);
			{input with
				kind = `Git {git with fetch_time = Some Fetch_time.Build}}
		| _ -> input

(* PATCH **********************************************************************)

module Patch = struct
	module Source = struct
		type remote = {
			url: Template.t;
			fresh: Fresh.t; [@default Fresh.make ()]
			hash: Hash.t; [@default Hash.make ()]
			frozen: bool; [@default false]
			gc: GC.t option;
			store_name: Name.Store.t option;
			vendored: bool; [@default false]
			fetch_status: Fetch_status.t; [@default Fetch_status.default]
		}
		[@@deriving show, make, eq, qcheck]

		type t =
			| Local of string
			| Remote of remote
		[@@deriving show, eq]

		let is_local_path (path : string) : bool =
			String.starts_with ~prefix: "./" path
			|| String.starts_with ~prefix: "../" path
			|| String.starts_with ~prefix: "file:./" path
			|| String.starts_with ~prefix: "file:../" path

		let gen =
			let open QCheck.Gen in
			let a_to_z = char_range 'a' 'z' in
			let local path = Local ("./" ^ path)
			and remote rem = Remote rem
			in
			oneof_weighted [
				(1, map local (string_size ~gen: a_to_z (int_bound 20)));
				(3, map remote gen_remote)
			]
	end

	type t = {
		name: Name.Patch.t;
		source: Source.t;
	}
	[@@deriving show, make, qcheck]

	let strip_file_prefix (path : string) : string =
		if String.starts_with ~prefix: "file:" path then
			String.sub path 5 (String.length path - 5)
		else
			path

	let fresh_cmd (patch : t) : Fresh.Cmd.t option =
		match patch.source with
		| Source.Local _ -> None
		| Source.Remote remote -> remote.fresh.cmd

	let jg_models2 (patch : t) (needle : string) : Jingoo.Jg_types.tvalue =
		let open Jingoo.Jg_types in
		(* such a small model, just using match *)
		match needle with
		| "fresh_value" ->
			begin
				match patch.source with
				| Source.Local _ -> Tnull
				| Source.Remote remote ->
					match remote.fresh.value with
					| Some v -> Tstr v
					| None -> Tnull
			end
		| "name" -> Tstr (Name.Patch.take patch.name)
		| _ -> Tnull

	let of_source ~(source : Template.t) ~(models : jg_models2) : Source.t =
		let path = Template.fill ~models source in
		if Source.is_local_path path then
			Source.Local (strip_file_prefix path)
		else
			Source.Remote (Source.make_remote ~url: source ())
end

(* PHASE TRACKING *************************************************************)

let log_promotion (_ : t) : unit = ()

(* JINGOO MODELS **************************************************************)

let jg_models2 (input : t) (needle : string) : Jingoo.Jg_types.tvalue =
	let open Jingoo.Jg_types in
	let opt_count = Option.fold ~none: 0 ~some: (Fun.const 1) in
	(* presupplied with global values *)
	let make_hashtbl (further_size : int) : (string, tvalue) Hashtbl.t =
		let size = 1 + opt_count input.fresh.value in
		let htbl = Hashtbl.create (size + further_size) in
		Hashtbl.add htbl "name" (Tstr (Name.Input.take input.name));
		Option.iter (fun v -> Hashtbl.add htbl "fresh_value" (Tstr v)) input.fresh.value;
		htbl
	in
	let hashtbl =
		match input.kind with
		| `File _ ->
			make_hashtbl 0
		| `Archive _ ->
			make_hashtbl 0
		| `Git g ->
			begin
				let htbl = make_hashtbl 5 in
				(
					match g.reference with
					| `Branch b -> Hashtbl.add htbl "branch" (Tstr b)
					| `Tag t -> Hashtbl.add htbl "tags" (Tstr t)
					| `Ref r -> Hashtbl.add htbl "ref" (Tstr r)
				);
				Option.iter (fun d -> Hashtbl.add htbl "datetime" (Tstr d)) g.datetime;
				Hashtbl.add htbl "lfs" (Tbool g.lfs);
				Hashtbl.add htbl "submodules" (Tbool g.submodules);
				Option.iter
					(fun r ->
						List.iter (fun key -> Hashtbl.add htbl key (Tstr r)) ["rev"; "revision"]
					)
					g.latest_revision;
				htbl
			end
		| `Darcs d ->
			begin
				let htbl = make_hashtbl 3 in
				(
					match d.reference with
					| `Context (`Stated sc) ->
						Hashtbl.add htbl "context" (Tstr sc)
					| `Context (`Assumed ac) ->
						Option.iter (fun c -> Hashtbl.add htbl "context" (Tstr c)) ac
					| `Tag t ->
						Hashtbl.add htbl "tag" (Tstr t)
				);
				Option.iter (fun d -> Hashtbl.add htbl "datetime" (Tstr d)) d.datetime;
				Option.iter (fun w -> Hashtbl.add htbl "weak_hash" (Tstr w)) d.latest_weak_hash;
				htbl
			end
		| `Pijul p ->
			begin
				let htbl = make_hashtbl 3 in
				(
					match p.reference with
					| `Channel c -> Hashtbl.add htbl "channel" (Tstr c)
					| `State s -> Hashtbl.add htbl "state" (Tstr s)
					| `Change c -> Hashtbl.add htbl "change" (Tstr c)
				);
				Option.iter (fun d -> Hashtbl.add htbl "datetime" (Tstr d)) p.datetime;
				Option.iter (fun s -> Hashtbl.add htbl "state" (Tstr s)) p.latest_state;
				htbl
			end
		| `Fossil f ->
			begin
				let htbl = make_hashtbl 3 in
				(
					match f.reference with
					| `Branch b -> Hashtbl.add htbl "branch" (Tstr b)
					| `Tag t -> Hashtbl.add htbl "tag" (Tstr t)
					| `Checkin c -> Hashtbl.add htbl "checkin" (Tstr c)
				);
				Option.iter (fun d -> Hashtbl.add htbl "date" (Tstr d)) f.date;
				Option.iter (fun c -> Hashtbl.add htbl "checkin" (Tstr c)) f.latest_checkin;
				htbl
			end
	in
	match Hashtbl.find_opt hashtbl needle with
	| Some value -> value
	| None -> Tnull

(* NIXPKGS ********************************************************************)

(* Nixpkgs is so critical & valuable to the Nix ecosystem that it gets its own
	special treatment; it is also *required* to get access to many of the
	fetchers *)
module Nixpkgs = struct
	module Channel = struct
		type t =
			| NixOS_unstable of {small: bool}
			| NixOS_stable of {major: int; minor: int; small: bool}
			| Nixpkgs_unstable
			| Nixpkgs_darwin of {major: int; minor: int}
		[@@deriving show, eq, qcheck]

		let default = NixOS_unstable {small = false}

		let to_string = function
			| NixOS_unstable {small} ->
				Fmt.str "nixos-unstable%s" (if small then "-small" else "")
			| NixOS_stable {major; minor; small} ->
				Fmt.str
					"nixos-%d.%02d%s"
					major
					minor
					(if small then "-small" else "")
			| Nixpkgs_unstable ->
				"nixpkgs-unstable"
			| Nixpkgs_darwin {major; minor} ->
				Fmt.str "nixpkgs-%d.%02d-darwin" major minor

		let parse_stable_version (version : string) : (int * int) option =
			match List.map int_of_string_opt (String.split_on_char '.' version) with
			| [Some major; Some minor] when major >= 0 && minor >= 0 ->
				Some (major, minor)
			| _ ->
				None

		let of_string (channel : string) : t option =
			match String.split_on_char '-' channel with
			| ["nixos"; "unstable"] ->
				Some (NixOS_unstable {small = false})
			| ["nixos"; "unstable"; "small"] ->
				Some (NixOS_unstable {small = true})
			| ["nixos"; version] ->
				parse_stable_version version
				|> Option.map (fun (major, minor) -> NixOS_stable {major; minor; small = false})
			| ["nixos"; version; "small"] ->
				parse_stable_version version
				|> Option.map (fun (major, minor) -> NixOS_stable {major; minor; small = true})
			| ["nixpkgs"; "unstable"] ->
				Some Nixpkgs_unstable
			| ["nixpkgs"; version; "darwin"] ->
				parse_stable_version version
				|> Option.map (fun (major, minor) -> Nixpkgs_darwin {major; minor})
			| _ ->
				None

		(* NOTE: .xz is being deprecated *)
		let extension = function
			| NixOS_stable {major; _}
			| Nixpkgs_darwin {major; _} when major < 26 ->
				"tar.xz"
			| _ ->
				"tar.zst"

		let release_URL (channel : t) : Template.t =
			let extension = extension channel
			and base =
				match channel with
				| NixOS_unstable {small} ->
					Fmt.str
						"https://releases.nixos.org/nixos/unstable%s"
						(if small then "-small" else "")
				| NixOS_stable {major; minor; small} ->
					Fmt.str
						"https://releases.nixos.org/nixos/%d.%02d%s"
						major
						minor
						(if small then "-small" else "")
				| Nixpkgs_unstable ->
					"https://releases.nixos.org/nixpkgs"
				| Nixpkgs_darwin {major; minor} ->
					Fmt.str
						"https://releases.nixos.org/nixpkgs/%d.%02d-darwin"
						major
						minor
			in
			Template.make @@
				Fmt.str "%s/{{fresh_value}}/nixexprs.%s" base extension

		let mirror_TUNA_URL (channel : t) : Template.t =
			Template.make @@
				Fmt.str
					"https://mirrors.tuna.tsinghua.edu.cn/nix-channels/releases/%s%%40{{fresh_value}}/nixexprs.%s"
					(to_string channel)
					(extension channel)

		let fresh (channel : t) ?fresh_value () : Fresh.t =
			let open Fresh.Cmd in
			let tpl = Template.make in
			let curl_channel : Fresh.Cmd.t =
				~${
					prog = tpl "curl";
					args = [
						tpl "-Ls";
						tpl "--http2";
						tpl "--compressed";
						tpl "-o";
						tpl "/dev/null";
						tpl "-w";
						tpl "%{url_effective}";
						tpl ("https://channels.nixos.org/" ^ to_string channel)
					];
				}
				|: {prog = tpl "awk"; args = [tpl "-F/"; tpl "{print $NF}"]}
			in
			{
				cmd = Some curl_channel;
				value = fresh_value;
			}
	end

	module Reference = struct
		type t = [
			| `Channel of string
			| `Ref of string
		]

		(* NOTE: “nixpkgs-unstable” is probably good enough for your
			project, but defaulting to nixos-unstable since it is ‘safer’,
			requiring that all the NixOS tests pass *)
		let default_channel = Channel.to_string Channel.default
		let default_ref = "refs/heads/" ^ default_channel
	end

	let name = Name.Input.make "nixpkgs"
	let store_name = Name.Store.make "nixpkgs-src"

	let default_git_repository = Template.make "https://github.com/NixOS/nixpkgs.git"

	let default_hash = Hash.make ~algorithm: Hash.SHA_256 ()

	let known_git_mirrors : Template.t list =
		List.map Template.make [
			"https://mirrors.tuna.tsinghua.edu.cn/git/nixpkgs.git"
		]

	let make_fresh_git ~(reference : Reference.t) ?fresh_value () : Fresh.t =
		let open Fresh.Cmd in
		let t = Template.make in
		let git_ls_remote (ls_remote_args : Template.t list) : t =
			let args = t "ls-remote" :: default_git_repository :: ls_remote_args in
			~${prog = t "git"; args}
			|: {prog = t "cut"; args = [t "-f1"]}
		in
		{
			cmd = begin
				match reference with
				| `Ref r -> Some (git_ls_remote [t "--refs"; t r])
				| `Channel c -> Some (git_ls_remote [t "--branches"; t c])
			end;
			value = fresh_value;
		}

	let make_git_archive ?(reference = `Ref Reference.default_ref) ?fresh_value () =
		let fresh = make_fresh_git ~reference ?fresh_value () in
		let url =
			Template.make "https://github.com/NixOS/nixpkgs/archive/{{fresh_value}}.tar.gz"
		in
		let kind = make_kind_archive ~url () in
		make ~name ~kind ~fresh ~hash: default_hash ()

	(* The TUNA mirror is a Git mirror, so normalize on Git *)
	let make_git_with_known_mirrors
			?(extra_mirrors = [])
			?reference
			?latest_revision
			?submodules
			?lfs
			()
		=
		let reference =
			match reference with
			| None -> `Ref Reference.default_ref
			| Some (`Channel c) -> `Branch c
			| Some (`Ref r) -> `Ref r
		in
		let kind =
			make_kind_git
				~repository: default_git_repository
				~mirrors: (known_git_mirrors @ extra_mirrors)
				~reference
				?latest_revision
				?submodules
				?lfs
				()
		in
		make ~name ~kind ~hash: default_hash ()

	let make_channels
			?(channel = Channel.default)
			?(mirrors = [])
			()
		=
		let extension = Some (Channel.extension channel)
		and url = Channel.release_URL channel
		in
		let kind = make_kind_archive ~url ~mirrors ?extension ()
		and fresh = Channel.fresh channel ()
		in
		make ~name ~kind ~hash: default_hash ~fresh ()
end
