(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
module Non_empty_list = Util.Non_empty_list

type error = Error.lockfile_error

let filename = "lock.json"

let encode_tag = Util.Jsont.encode_tag

let rec fill_locators ~models acc = function
	| [] -> Ok (List.rev acc)
	| template :: rest ->
		match Locator.parse (Input.Template.fill ~models template) with
		| Ok loc -> fill_locators ~models (loc :: acc) rest
		| Error e -> Error (e, [])

let fill_locator ~models template =
	match Locator.parse (Input.Template.fill ~models template) with
	| Ok loc -> Ok loc
	| Error e -> Error (e, [])

module Fetch_time = struct
	(* NOTE: this is only available for kinds with builtin fetchers *)
	include Input.Fetch_time

	let jsont : t Jsont.t =
		let open Jsont in
		let dec uint =
			match of_enum uint with
			| Some ft -> ft
			| None -> Error.msgf Meta.none "Not a valid fetch time (uint8)"
		and enc = to_enum
		in
		map ~kind: "Fetch_time" uint8 ~dec ~enc
end

module File = struct
	type t = {
		fetch_time: Fetch_time.t;
		url: Locator.t;
		mirrors: Locator.t list;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_lock
			~(models : Input.jg_models2)
			({fetch_time; url; mirrors; _}: Input.File.t)
			: (t, Locator.error Non_empty_list.t) result
		=
		let (let*) = Result.bind in
		let* url = fill_locator ~models url in
		let* mirrors = fill_locators ~models [] mirrors in
		Ok {
			fetch_time = Fetch_time.value fetch_time;
			url;
			mirrors;
		}

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "File_lock"
			(fun fetch_time url mirrors -> {fetch_time; url; mirrors})
		|> Object.mem "ft" Fetch_time.jsont ~enc: (fun i -> i.fetch_time)
		|> Object.mem "ur" Locator.jsont ~enc: (fun i -> i.url)
		|> Object.mem "ms" (list Locator.jsont) ~enc: (fun i -> i.mirrors)
		|> Object.finish
end

module Archive = struct
	type t = {
		fetch_time: Fetch_time.t;
		url: Locator.t;
		mirrors: Locator.t list;
		extension: string option;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_lock
			~(models : Input.jg_models2)
			({fetch_time; url; mirrors; extension; _}: Input.Archive.t)
			: (t, Locator.error Non_empty_list.t) result
		=
		let (let*) = Result.bind in
		let* url = fill_locator ~models url in
		let* mirrors = fill_locators ~models [] mirrors in
		Ok {
			fetch_time = Fetch_time.value fetch_time;
			url;
			mirrors;
			extension;
		}

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Archive_lock"
			(fun fetch_time url mirrors ->
				{
					fetch_time;
					url;
					mirrors;
					extension = None;
				}
			)
		|> Object.mem "ft" Fetch_time.jsont ~enc: (fun i -> i.fetch_time)
		|> Object.mem "ur" Locator.jsont ~enc: (fun i -> i.url)
		|> Object.mem "ms" (list Locator.jsont) ~enc: (fun i -> i.mirrors)
		|> Object.finish

	let jsont_v1_4_0 : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Archive_lock_v1_4_0"
			(fun fetch_time url mirrors extension ->
				{
					fetch_time;
					url;
					mirrors;
					extension;
				}
			)
		|> Object.mem "ft" Fetch_time.jsont ~enc: (fun i -> i.fetch_time)
		|> Object.mem "ur" Locator.jsont ~enc: (fun i -> i.url)
		|> Object.mem "ms" (list Locator.jsont) ~enc: (fun i -> i.mirrors)
		|> Object.mem "ex" (option string) ~enc: (fun i -> i.extension)
		|> Object.finish
end

module Git = struct
	module Reference = struct
		type t = Input.Git.Reference.t
		[@@deriving show, eq, qcheck]

		let jsont : t Jsont.t =
			let open Jsont in
			let enc = function
				| `Ref ref -> encode_tag 0 string ref
				| `Branch brc -> encode_tag 1 string brc
				| `Tag tag -> encode_tag 2 string tag
			and dec = function
				| [|tag; value|] ->
					begin
						match Result.bind (Json.decode' uint8 tag) (function
							| 0 -> Json.decode' string value |> Result.map (fun r -> `Ref r)
							| 1 -> Json.decode' string value |> Result.map (fun b -> `Branch b)
							| 2 -> Json.decode' string value |> Result.map (fun t -> `Tag t)
							| n -> Error.msgf Meta.none "Unknown reference enum tag: %d" n
						) with
						| Ok v -> v
						| Error (ctx, meta, kind) -> Error.raise ctx meta kind
					end
				| _ ->
					Error.msgf Meta.none "Expected array of length 2"
			in
			map ~kind: "Git_reference_lock" ~enc ~dec (array json)
	end

	type t = {
		repository: Locator.t;
		mirrors: Locator.t list;
		reference: Reference.t;
		datetime: string option;
		submodules: bool;
		lfs: bool;
		latest_revision: string option;
		fetch_time: Fetch_time.t;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_lock
			~(models : Input.jg_models2)
			({
				repository;
				mirrors;
				reference;
				datetime;
				submodules;
				lfs;
				latest_revision;
				fetch_time;
				_
			}:
				Input.Git.t
			)
			: (t, Locator.error Non_empty_list.t) result
		=
		let (let*) = Result.bind in
		let* repository = fill_locator ~models repository in
		let* mirrors = fill_locators ~models [] mirrors in
		Ok {
			fetch_time = Fetch_time.value fetch_time;
			repository;
			mirrors;
			reference;
			datetime;
			submodules;
			lfs;
			latest_revision;
		}

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Git_lock"
			(fun fetch_time repository mirrors reference datetime submodules lfs latest_revision ->
				{
					fetch_time;
					repository;
					mirrors;
					reference;
					datetime;
					submodules;
					lfs;
					latest_revision;
				}
			)
		|> Object.mem "ft" Fetch_time.jsont ~enc: (fun i -> i.fetch_time)
		|> Object.mem "rp" Locator.jsont ~enc: (fun i -> i.repository)
		|> Object.mem "ms" (list Locator.jsont) ~enc: (fun i -> i.mirrors)
		|> Object.mem "rf" Reference.jsont ~enc: (fun i -> i.reference)
		|> Object.mem "dt" (option string) ~enc: (fun i -> i.datetime)
		|> Object.mem "sm" bool ~enc: (fun i -> i.submodules)
		|> Object.mem "lf" bool ~enc: (fun i -> i.lfs)
		|> Object.mem "lr" (option string) ~enc: (fun i -> i.latest_revision)
		|> Object.finish
end

module Darcs = struct
	module Reference = struct
		type t = Input.Darcs.Reference.t
		[@@deriving show, eq, qcheck]

		let jsont : t Jsont.t =
			let open Jsont in
			let context_jsont =
				let enc = function
					| `Assumed (Some actx) -> encode_tag 0 string actx
					| `Stated sctx -> encode_tag 1 string sctx
					(* We can’t lock without a stable reference *)
					| `Assumed None ->
						Error.msgf Meta.none "Darcs context cannot be None when locking"
				and dec = function
					| [|tag; value|] ->
						begin
							match Result.bind (Json.decode' uint8 tag) (function
								| 0 -> Json.decode' string value |> Result.map (fun a -> `Assumed (Some a))
								| 1 -> Json.decode' string value |> Result.map (fun s -> `Stated s)
								| n -> Error.msgf Meta.none "Unknown context enum tag: %d" n
							) with
							| Ok v -> v
							| Error (ctx, meta, kind) -> Error.raise ctx meta kind
						end
					| _ ->
						Error.msgf Meta.none "Expected array of length 2"
				in
				map ~kind: "Darcs_reference_context_lock" ~enc ~dec (array json)
			in
			let enc = function
				| `Context ctx -> encode_tag 0 context_jsont ctx
				| `Tag tag -> encode_tag 1 string tag
			and dec = function
				| [|tag; value|] ->
					begin
						match Result.bind (Json.decode' uint8 tag) (function
							| 0 -> Json.decode' context_jsont value |> Result.map (fun c -> `Context c)
							| 1 -> Json.decode' string value |> Result.map (fun t -> `Tag t)
							| n -> Error.msgf Meta.none "Unknown reference enum tag: %d" n
						) with
						| Ok v -> v
						| Error (ctx, meta, kind) -> Error.raise ctx meta kind
					end
				| _ ->
					Error.msgf Meta.none "Expected array of length 2"
			in
			map ~kind: "Darcs_reference_lock" ~enc ~dec (array json)
	end

	type t = {
		repository: Locator.t;
		mirrors: Locator.t list;
		datetime: string option;
		(* Darcs isn’t like the other girls; we don’t have a simple stable reference point.
			Either the tag or context can be used. *)
		reference: Reference.t;
		latest_weak_hash: string option;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_lock
			~(models : Input.jg_models2)
			({repository; mirrors; datetime; reference; latest_weak_hash; _}: Input.Darcs.t)
			: (t, Locator.error Non_empty_list.t) result
		=
		let (let*) = Result.bind in
		let* repository = fill_locator ~models repository in
		let* mirrors = fill_locators ~models [] mirrors in
		Ok {
			repository;
			mirrors;
			datetime;
			reference;
			latest_weak_hash;
		}

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Darcs_lock"
			(fun repository mirrors datetime reference latest_weak_hash ->
				{repository; mirrors; datetime; reference; latest_weak_hash}
			)
		|> Object.mem "rp" Locator.jsont ~enc: (fun i -> i.repository)
		|> Object.mem "ms" (list Locator.jsont) ~enc: (fun i -> i.mirrors)
		|> Object.mem "dt" (option string) ~enc: (fun i -> i.datetime)
		|> Object.mem "rf" Reference.jsont ~enc: (fun i -> i.reference)
		|> Object.mem "lw" (option string) ~enc: (fun i -> i.latest_weak_hash)
		|> Object.finish
end

module Pijul = struct
	(*
	module Reference = struct
		type t = Input.Pijul.Reference.t
		[@@deriving show]

		let jsont : t Jsont.t =
			let open Jsont in
			let enc = function
				| `Channel chn -> encode_tag 0 string chn
				| `State stt -> encode_tag 1 string stt
				| `Change chg -> encode_tag 2 string chg
			and dec = function
				| [|tag; value|] ->
					begin
						match Result.bind (Json.decode' uint8 tag) (function
							| 0 -> Json.decode' string value |> Result.map (fun c -> `Channel c)
							| 1 -> Json.decode' string value |> Result.map (fun c -> `State c)
							| 2 -> Json.decode' string value |> Result.map (fun t -> `Change t)
							| n -> Error.msgf Meta.none "Unknown reference enum tag: %d" n
						) with
						| Ok v -> v
						| Error (ctx, meta, kind) -> Error.raise ctx meta kind
					end
				| _ ->
					Error.msgf Meta.none "Expected array of length 2"
			in
			map ~kind: "Pijul_reference_lock" ~enc ~dec (array json)
	end
	*)

	type t = {
		remote: Locator.t;
		mirrors: Locator.t list;
		datetime: string option;
		(*reference: Reference.t;*)
		latest_state: string option;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_lock
			~(models : Input.jg_models2)
			({remote; mirrors; datetime; latest_state; _}: Input.Pijul.t)
			: (t, Locator.error Non_empty_list.t) result
		=
		let (let*) = Result.bind in
		let* remote = fill_locator ~models remote in
		let* mirrors = fill_locators ~models [] mirrors in
		Ok {
			remote;
			mirrors;
			datetime;
			latest_state;
		}

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map ~kind: "Pijul_lock" (fun remote mirrors datetime (*reference*) latest_state ->
			{remote; mirrors; datetime; (*reference;*) latest_state}
		)
		|> Object.mem "rm" Locator.jsont ~enc: (fun i -> i.remote)
		|> Object.mem "ms" (list Locator.jsont) ~enc: (fun i -> i.mirrors)
		|> Object.mem "dt" (option string) ~enc: (fun i -> i.datetime)
		(* |> Object.mem "rf" Reference.jsont ~enc: (fun i -> i.reference) *)
		|> Object.mem "ls" (option string) ~enc: (fun i -> i.latest_state)
		|> Object.finish
end

module Fossil = struct
	type t = {
		repository: Locator.t;
		date: string option;
		latest_checkin: string option;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_lock
			~(models : Input.jg_models2)
			({repository; date; latest_checkin; _}: Input.Fossil.t)
			: (t, Locator.error Non_empty_list.t) result
		=
		let (let*) = Result.bind in
		let* repository = fill_locator ~models repository in
		Ok {
			repository;
			date;
			latest_checkin;
		}

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map ~kind: "Fossil_lock" (fun repository date latest_checkin ->
			{repository; date; latest_checkin}
		)
		|> Object.mem "rp" Locator.jsont ~enc: (fun i -> i.repository)
		|> Object.mem "dt" (option string) ~enc: (fun i -> i.date)
		|> Object.mem "lc" (option string) ~enc: (fun i -> i.latest_checkin)
		|> Object.finish
end

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

	let to_lock
		~(models : Input.jg_models2)
		: Input.Kind.t -> (t, Locator.error Non_empty_list.t) result
	= function
		| `File f ->
			File.to_lock ~models f |> Result.map (fun f -> `File f)
		| `Archive a ->
			Archive.to_lock ~models a |> Result.map (fun a -> `Archive a)
		| `Git g ->
			Git.to_lock ~models g |> Result.map (fun g -> `Git g)
		| `Darcs d ->
			Darcs.to_lock ~models d |> Result.map (fun d -> `Darcs d)
		| `Pijul p ->
			Pijul.to_lock ~models p |> Result.map (fun p -> `Pijul p)
		| `Fossil f ->
			Fossil.to_lock ~models f |> Result.map (fun f -> `Fossil f)

	let jsont : t Jsont.t =
		let open Jsont in
		let enc = function
			| `File f -> encode_tag 0 File.jsont f
			| `Archive a -> encode_tag 1 Archive.jsont a
			| `Git g -> encode_tag 2 Git.jsont g
			| `Darcs d -> encode_tag 3 Darcs.jsont d
			| `Pijul p -> encode_tag 4 Pijul.jsont p
			| `Fossil p -> encode_tag 5 Fossil.jsont p
		and dec = function
			| [|tag; value|] ->
				begin
					match Result.bind (Json.decode' uint8 tag) (function
						| 0 ->
							Json.decode' File.jsont value
							|> Result.map (fun v -> `File v)
						| 1 ->
							Json.decode' Archive.jsont value
							|> Result.map (fun v -> `Archive v)
						| 2 ->
							Json.decode' Git.jsont value
							|> Result.map (fun v -> `Git v)
						| 3 ->
							Json.decode' Darcs.jsont value
							|> Result.map (fun v -> `Darcs v)
						| 4 ->
							Json.decode' Pijul.jsont value
							|> Result.map (fun v -> `Pijul v)
						| 5 ->
							Json.decode' Fossil.jsont value
							|> Result.map (fun v -> `Fossil v)
						| n ->
							Error.msgf Meta.none "Unknown reference enum tag: %d" n
					) with
					| Ok v -> v
					| Error (ctx, meta, kind) -> Error.raise ctx meta kind
				end
			| _ ->
				Error.msgf Meta.none "Expected array of length 2"
		in
		map ~kind: "Input_kind" ~enc ~dec (array json)

	let jsont_v1_4_0 : t Jsont.t =
		let open Jsont in
		let enc = function
			| `File f -> encode_tag 0 File.jsont f
			| `Archive a -> encode_tag 1 Archive.jsont_v1_4_0 a
			| `Git g -> encode_tag 2 Git.jsont g
			| `Darcs d -> encode_tag 3 Darcs.jsont d
			| `Pijul p -> encode_tag 4 Pijul.jsont p
			| `Fossil p -> encode_tag 5 Fossil.jsont p
		and dec = function
			| [|tag; value|] ->
				begin
					match Result.bind (Json.decode' uint8 tag) (function
						| 0 ->
							Json.decode' File.jsont value
							|> Result.map (fun v -> `File v)
						| 1 ->
							Json.decode' Archive.jsont_v1_4_0 value
							|> Result.map (fun v -> `Archive v)
						| 2 ->
							Json.decode' Git.jsont value
							|> Result.map (fun v -> `Git v)
						| 3 ->
							Json.decode' Darcs.jsont value
							|> Result.map (fun v -> `Darcs v)
						| 4 ->
							Json.decode' Pijul.jsont value
							|> Result.map (fun v -> `Pijul v)
						| 5 ->
							Json.decode' Fossil.jsont value
							|> Result.map (fun v -> `Fossil v)
						| n ->
							Error.msgf Meta.none "Unknown reference enum tag: %d" n
					) with
					| Ok v -> v
					| Error (ctx, meta, kind) -> Error.raise ctx meta kind
				end
			| _ ->
				Error.msgf Meta.none "Expected array of length 2"
		in
		map ~kind: "Input_kind" ~enc ~dec (array json)
end

module Hash = struct
	type algorithm = Input.Hash.algorithm
	[@@deriving show, eq, qcheck]

	let () =
		assert (Input.Hash.max_algorithm <= 0xFF)

	let algorithm_jsont : algorithm Jsont.t =
		let open Jsont in
		map
			~kind: "Hash_algorithm_lock"
			~dec: (fun i ->
				match Input.Hash.algorithm_of_enum i with
				| Some al ->
					al
				| None ->
					Jsont.Error.msgf Meta.none "Invalid hash algorithm enum: %d" i
			)
			~enc: Input.Hash.algorithm_to_enum
			uint8

	type t = {
		algorithm: algorithm;
		value: string option;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_lock ({algorithm; value; _}: Input.Hash.t) : t =
		{algorithm; value}

	let jsont : t Jsont.t =
		let open Jsont in
		let validate_SRI algorithm value =
			match value with
			| Some v ->
				begin
					match String.split_on_char '-' v with
					| [sri_algo; _sri_value] ->
						(
							match Input.Hash.algorithm_of_string sri_algo with
							| Some sri_algorithm when sri_algorithm = algorithm ->
								value
							| Some sri_algorithm ->
								Logs.warn (fun m ->
									m
										"Hash algorithm mismatch in lockfile: stated “%a” but SRI hash has “%a”. Treating as invalid."
										pp_algorithm
										algorithm
										pp_algorithm
										sri_algorithm
								);
								None
							| None ->
								Logs.warn (fun m -> m "Invalid algorithm “%s” in SRI hash. Treating as invalid." sri_algo);
								None
						)
					| _ ->
						Logs.warn (fun m -> m "Malformed SRI hash “%s”. Treating as invalid." v);
						None
				end
			| None -> None
		in
		Object.map
			~kind: "Hash_lock"
			(fun algorithm value ->
				{algorithm; value = validate_SRI algorithm value}
			)
		|> Object.mem "al" algorithm_jsont ~enc: (fun i -> i.algorithm)
		|> Object.mem "vl" (option string) ~enc: (fun i -> i.value)
		|> Object.finish
end

module Input' = struct
	type t = {
		store_name: Name.Store.t option;
		kind: Kind.t;
		hash: Hash.t;
		fresh_value: string option;
		patches: Name.Patch.t list;
	}
	[@@deriving show, eq, make, qcheck]

	let jsont_v0_5_0 : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Input_lock_no_sn"
			(fun kind hash fresh_value patches ->
				{store_name = None; kind; hash; fresh_value; patches}
			)
		|> Object.mem "kd" Kind.jsont ~enc: (fun i -> i.kind)
		|> Object.mem "ha" Hash.jsont ~enc: (fun i -> i.hash)
		|> Object.mem "fv" (option string) ~enc: (fun i -> i.fresh_value)
		|> Object.mem "ps" (list Name.Patch.jsont) ~enc: (fun i -> i.patches)
		|> Object.finish

	let jsont_v1_1_0 : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Input_lock_with_sn"
			(fun store_name kind hash fresh_value patches ->
				{store_name; kind; hash; fresh_value; patches}
			)
		|> Object.mem "sn" (option Name.Store.jsont) ~enc: (fun i -> i.store_name)
		|> Object.mem "kd" Kind.jsont ~enc: (fun i -> i.kind)
		|> Object.mem "ha" Hash.jsont ~enc: (fun i -> i.hash)
		|> Object.mem "fv" (option string) ~enc: (fun i -> i.fresh_value)
		|> Object.mem "ps" (list Name.Patch.jsont) ~enc: (fun i -> i.patches)
		|> Object.finish

	let jsont_v1_4_0 : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Input_lock_with_sn"
			(fun store_name kind hash fresh_value patches ->
				{store_name; kind; hash; fresh_value; patches}
			)
		|> Object.mem "sn" (option Name.Store.jsont) ~enc: (fun i -> i.store_name)
		|> Object.mem "kd" Kind.jsont_v1_4_0 ~enc: (fun i -> i.kind)
		|> Object.mem "ha" Hash.jsont ~enc: (fun i -> i.hash)
		|> Object.mem "fv" (option string) ~enc: (fun i -> i.fresh_value)
		|> Object.mem "ps" (list Name.Patch.jsont) ~enc: (fun i -> i.patches)
		|> Object.finish

	let [@inline]to_lock ~(models : Input.jg_models2) (input : Input.t) : (t, Locator.error Non_empty_list.t) result =
		let (let*) = Result.bind in
		let* kind = Kind.to_lock ~models input.kind in
		Ok {
			store_name = input.store_name;
			kind;
			hash = Hash.to_lock input.hash;
			fresh_value = input.fresh.value;
			patches = input.patches;
		}
end

type inputs = Input'.t Name.InputMap.t
[@@deriving show, eq, qcheck]

(* NOTE: only Remote patches are locked… Local patches are already version
	controlled via the VCS *)
module Patch = struct
	type t = {
		url: Locator.t;
		hash: Hash.t;
		fresh_value: string option;
		store_name: Name.Store.t option;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_lock
			~(models : Input.jg_models2)
			(patch : Input.Patch.t)
			: (t option, Locator.error Non_empty_list.t) result
		=
		match patch.source with
		| Input.Patch.Source.Local _ -> Ok None
		| Input.Patch.Source.Remote {url; hash; fresh; store_name; _} ->
			match Locator.parse (Input.Template.fill ~models url) with
			| Ok url ->
				let hash = Hash.to_lock hash in
				Ok (Some {url; hash; fresh_value = fresh.value; store_name})
			| Error e ->
				Error (e, [])

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map ~kind: "Patch_lock" (fun url hash ->
			make ~url ~hash ()
		)
		|> Object.mem "ur" Locator.jsont ~enc: (fun i -> i.url)
		|> Object.mem "ha" Hash.jsont ~enc: (fun i -> i.hash)
		|> Object.finish

	let jsont_v1_4_0 : t Jsont.t =
		let open Jsont in
		Object.map ~kind: "Patch_lock_v1_4_0" (fun url hash fresh_value store_name ->
			make ~url ~hash ?fresh_value ?store_name ()
		)
		|> Object.mem "ur" Locator.jsont ~enc: (fun i -> i.url)
		|> Object.mem "ha" Hash.jsont ~enc: (fun i -> i.hash)
		|> Object.mem "fv" (option string) ~enc: (fun i -> i.fresh_value)
		|> Object.mem "sn" (option Name.Store.jsont) ~enc: (fun i -> i.store_name)
		|> Object.finish
end

type patches = Patch.t Name.PatchMap.t
[@@deriving show, eq, qcheck]

module Version = Schema.Version

type t = {
	version: Version.t;
	patches: patches; [@default Name.PatchMap.empty]
	inputs: inputs; [@default Name.InputMap.empty]
}
[@@deriving show, eq, make]

let lockfile : t option ref = ref None

module V0_5_0 = struct
	type nonrec t = t

	let jsont : t Jsont.t =
		let open Jsont in
		let inputs = Name.InputMap.jsont ~kind: "Input" Input'.jsont_v0_5_0
		and patches = Name.PatchMap.jsont ~kind: "Patch" Patch.jsont
		in
		Object.map ~kind: "Lockfile_V0_5_0" (fun version inputs patches ->
			make ~version ~inputs ~patches ()
		)
		|> Object.mem "v" Version.jsont ~enc: (fun lock -> lock.version)
		|> Object.mem "i" inputs ~enc: (fun lock -> lock.inputs)
		|> Object.mem "p" patches ~enc: (fun lock -> lock.patches)
		|> Object.finish
end

module V1_1_0 = struct
	type nonrec t = t

	let jsont : t Jsont.t =
		let open Jsont in
		let inputs = Name.InputMap.jsont ~kind: "Input" Input'.jsont_v1_1_0
		and patches = Name.PatchMap.jsont ~kind: "Patch" Patch.jsont
		in
		Object.map ~kind: "Lockfile_V1_1_0" (fun version inputs patches ->
			{version; inputs; patches}
		)
		|> Object.mem "v" Version.jsont ~enc: (fun lock -> lock.version)
		|> Object.mem "i" inputs ~enc: (fun lock -> lock.inputs)
		|> Object.mem "p" patches ~enc: (fun lock -> lock.patches)
		|> Object.finish
end

module V1_4_0 = struct
	type nonrec t = t

	let jsont : t Jsont.t =
		let open Jsont in
		let inputs = Name.InputMap.jsont ~kind: "Input" Input'.jsont_v1_4_0
		and patches = Name.PatchMap.jsont ~kind: "Patch" Patch.jsont_v1_4_0
		in
		Object.map ~kind: "Lockfile_V1_4_0" (fun version inputs patches ->
			{version; inputs; patches}
		)
		|> Object.mem "v" Version.jsont ~enc: (fun lock -> lock.version)
		|> Object.mem "i" inputs ~enc: (fun lock -> lock.inputs)
		|> Object.mem "p" patches ~enc: (fun lock -> lock.patches)
		|> Object.finish
end

let jsont : t Jsont.t = V0_5_0.jsont

let encode (lock : t) =
	let encode_codec =
		match lock.version with
		| `v1_4_0 | `v1_5_0 -> V1_4_0.jsont
		| `v1_3_0 | `v1_2_0 | `v1_1_0 -> V1_1_0.jsont
		| `v1_0_0 | `v0_5_0 -> V0_5_0.jsont
	in
	Jsont.Json.encode encode_codec lock

let pp fmt lock =
	match encode lock with
	| Ok json -> Util.Jsont.pp_lockfile fmt json
	| Error _ -> Fmt.pf fmt "<lockfile encoding error>"

(* Shared table fold behind [make_lockfile] & [lock_subset_renders]: the regular
	lockfile-generation path, used whole rather than duplicated. *)
let build_tables ?(version = Lazy.(force Schema.Version.latest)) () : (t, Error.foreman_error) result =
	let (let*) = Result.bind in
	let* patches =
		Foreman.to_lockfile_patches (fun (patch : Input.Patch.t) ->
			Patch.to_lock ~models: (Input.Patch.jg_models2 patch) patch
		)
		|> Result.map (Name.PatchMap.filter_map (fun _ v -> v))
	in
	let* inputs =
		Foreman.to_lockfile (fun input ->
			Input'.to_lock ~models: (Input.jg_models2 input) input
		)
	in
	let lock : t = {version; patches; inputs} in
	Ok lock

let make_lockfile ?(version = Lazy.(force Schema.Version.latest)) () : (t, Error.t) result =
	let (let*) = Result.bind in
	Logs.info (fun m -> m "Making lockfile @@ version:%s …" (Schema.Version.to_string version));
	let* lock = Error.tag_foreman (build_tables ~version ()) in
	Logs.debug (fun m -> m "New JSON lockfile:@;%a@." pp lock);
	lockfile := Some lock;
	Ok lock

let render (lock : t) : (string, string) result =
	match encode lock with
	| Error msg -> Error msg
	| Ok json ->
		let buffer = Buffer.create 4096 in
		let fmt = Format.formatter_of_buffer buffer in
		Format.pp_set_margin fmt max_int;
		Util.Jsont.pp_lockfile fmt json;
		Format.pp_print_flush fmt ();
		Ok (Buffer.contents buffer)

(* Inputs providing the Nixpkgs bootstrap: the conventional names. Single home
	for the rule, shared by narrowing & the pre-check below. *)
let is_nixpkgs_input (name : Name.Input.t) : bool =
	String.starts_with ~prefix: "nixpkgs" (Name.Input.take name)

(* Narrow a candidate lockfile to one input for checking: the input (with the
	given patch subset), its patches, & any nixpkgs entries (for bootstrap). *)
let lock_subset (lock : t) (name : Name.Input.t) (subset : Name.Patch.t list) : t option =
	match Name.InputMap.find_opt name lock.inputs with
	| None -> None
	| Some input ->
		let inputs =
			Name.InputMap.fold
				(fun input_name entry acc ->
					if is_nixpkgs_input input_name then
						Name.InputMap.add input_name entry acc
					else
						acc
				)
				lock.inputs
				Name.InputMap.empty
			|> Name.InputMap.add name {input with patches = subset}
		in
		let patches =
			let series = Name.PatchSet.of_list subset in
			Name.PatchMap.fold
				(fun patch_name entry acc ->
					if Name.PatchSet.mem patch_name series then
						Name.PatchMap.add patch_name entry acc
					else
						acc
				)
				lock.patches
				Name.PatchMap.empty
		in
		Some {lock with inputs; patches}

(* Per-input subset renders for patch verification, from current tables:
	full candidate maps, then either a subset renderer or [No_bootstrap] when
	no nixpkgs entry exists to evaluate against. Tables only grow keys
	during Stage 2, so lookups stay valid across a verify run. *)
let lock_subset_renders
		(name : Name.Input.t)
		: ([`No_bootstrap | `Ready of (Name.Patch.t list -> string option)], Error.foreman_error) result
	=
	let (let*) = Result.bind in
	let* lock = build_tables () in
	if not (Name.InputMap.exists (fun input_name _ -> is_nixpkgs_input input_name) lock.inputs) then
		Ok `No_bootstrap
	else
		Ok (
			`Ready (fun subset ->
				match lock_subset lock name subset with
				| None -> None
				| Some variant ->
					match render variant with
					| Ok rendered -> Some rendered
					| Error _ -> None
			)
		)

let exists () : bool =
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	Eio.Path.is_file filepath

let read () : (t option, error) result =
	let (let*) = Result.bind in
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	Logs.info (fun m -> m "Reading lockfile @@ %a …" Eio.Path.pp filepath);
	if Eio.Path.is_file filepath then
		begin
			let* lock =
				Eio.Path.with_open_in ~follow: false filepath @@ fun flow ->
				(* Try newest first, then fall back thru legacy codecs *)
				match Util.Jsont.of_flow V1_4_0.jsont flow with
				| Ok l ->
					Logs.info (fun m -> m "Read lockfile with V1_4_0 codec");
					Ok l
				| Error _ ->
					begin
						match Eio.Path.with_open_in ~follow: false filepath @@ fun flow ->
						Util.Jsont.of_flow V1_1_0.jsont flow with
						| Ok l ->
							Logs.info (fun m -> m "Read lockfile with V1_1_0 codec");
							Ok l
						| Error e1 ->
							Logs.info (fun m -> m "V1_1_0 codec failed: %s, trying V0_5_0" e1);
							Eio.Path.with_open_in ~follow: false filepath @@ fun flow ->
							Util.Jsont.of_flow V0_5_0.jsont flow
							|> Result.map_error (fun e -> `Parsing e)
					end
			in
			lockfile := Some lock;
			Ok (Some lock)
		end
	else
		begin
			Logs.warn (fun m -> m "Lockfile missing @@ %a. Consider running the lock command." Eio.Path.pp filepath);
			Ok None
		end

let write ?(create = `Or_truncate 0o644) ?lock () : (unit, Error.t) result =
	let (let*) = Result.bind in
	let* lock =
		match lock with
		| Some l -> Ok l
		| None -> make_lockfile ()
	in
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	Logs.info (fun m -> m "Writing lockfile @@ %a …" Eio.Path.pp filepath);
	match encode lock with
	| Ok json ->
		Eio.Path.with_open_out ~follow: false ~create filepath @@ fun flow ->
		let buffer = Buffer.create 4096 in
		let fmt = Format.formatter_of_buffer buffer in
		Format.pp_set_margin fmt max_int;
		Util.Jsont.pp_lockfile fmt json;
		Format.pp_print_flush fmt ();
		Eio.Flow.copy_string (Buffer.contents buffer) flow;
		Logs.info (fun m -> m "Lockfile written.");
		(Ok (): (unit, Error.t) result)
	| Error err ->
		(Error (`Lockfile (`Serializing err)): (unit, Error.t) result)

let upgrade (to_version : Version.t) (lock : t) : (t, error) result =
	let rec loop (current : Version.t) lock =
		if Version.equal current to_version then
			Ok lock
		else
			let next_enum = Version.to_enum current + 1 in
			match Version.of_enum next_enum with
			| Some ver ->
				let migrated =
					match current with
					| `v0_5_0 ->
						Ok {lock with version = ver}
					| `v1_0_0 ->
						let migrate_input (input_name : Name.Input.t) (input : Input'.t) : Input'.t =
							{input with store_name = Input.derive_store_name input_name}
						in
						Ok {
							version = ver;
							inputs = Name.InputMap.mapi migrate_input lock.inputs;
							patches = lock.patches;
						}
					| `v1_1_0 ->
						Ok {lock with version = ver}
					| `v1_2_0 ->
						Ok {lock with version = ver}
					| `v1_3_0 ->
						let migrate_input (input : Input'.t) : Input'.t =
							match input.kind with
							| `Archive a -> {input with kind = `Archive {a with extension = None}}
							| _ -> input
						and migrate_patch (patch_name : Name.Patch.t) (patch : Patch.t) : Patch.t = {patch with
							fresh_value = None;
							store_name = Input.derive_patch_store_name patch_name;
						}
						in
						Ok {
							version = ver;
							inputs = Name.InputMap.map migrate_input lock.inputs;
							patches = Name.PatchMap.mapi migrate_patch lock.patches;
						}
					| `v1_4_0 ->
						Ok {lock with version = ver}
					| `v1_5_0 ->
						Ok {lock with version = ver}
				in
				begin
					match migrated with
					| Ok m -> loop ver m
					| Error _ as e -> e
				end
			| None ->
				Error (`Parsing (Fmt.str "No version after: %s" (Version.to_string current)))
	in
	loop lock.version lock
