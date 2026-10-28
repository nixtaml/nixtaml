(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
type error = Error.prefetch_error

let run_and_gather ~proc_mgr ?(proc_env : Eio.Process.Env.t option) ?(buffer_size = 1024) method' cmd : (string, error) result =
	Logs.debug (fun m -> m "Running %a cmd: %a" Error.pp_prefetch_method method' (Fmt.list ~sep: Fmt.sp Fmt.string) cmd);
	let (let*) = Result.bind in
	let stdout_buf = Buffer.create buffer_size
	and stderr_buf = Buffer.create buffer_size
	in
	let stdout_sink = Eio.Flow.buffer_sink stdout_buf
	and stderr_sink = Eio.Flow.buffer_sink stderr_buf
	in
	let* () =
		try
			Eio.Process.run proc_mgr ?env: proc_env ~stdout: stdout_sink ~stderr: stderr_sink cmd;
			Ok ()
		with
			| exn ->
				Error (`Run_exception (method', exn, String.trim (Buffer.contents stderr_buf)))
	in
	match String.trim (Buffer.contents stdout_buf) with
	| "" -> Error (`Empty_output method')
	| stdout ->
		Logs.debug (fun m -> m "Command %a output: %s" Error.pp_prefetch_method method' stdout);
		Ok stdout

let run_and_gather_error ~proc_mgr ?(proc_env : Eio.Process.Env.t option) ?(buffer_size = 8192) method' cmd : (string, error) result =
	Logs.debug (fun m -> m "Running %a cmd (stderr): %a" Error.pp_prefetch_method method' (Fmt.list ~sep: Fmt.sp Fmt.string) cmd);
	let (let*) = Result.bind in
	let stderr_buf = Buffer.create buffer_size in
	let stderr_sink = Eio.Flow.buffer_sink stderr_buf in
	let* () =
		try
			Eio.Process.run proc_mgr ?env: proc_env ~stdout: Eio.Flow.null ~stderr: stderr_sink cmd;
			Ok ()
		with
			(* We exit ≠ 0 likely what we are looking for so no try *)
			| _exn -> Ok ()
	in
	match String.trim (Buffer.contents stderr_buf) with
	| "" -> Error (`Empty_output method')
	| stderr ->
		Logs.debug (fun m -> m "Command %a stderr: %s" Error.pp_prefetch_method method' stderr);
		Ok stderr

let hash_to_sri ~proc_mgr ?(proc_env : Eio.Process.Env.t option) ~(algo_str : string) ~(value : string) method' : (string, error) result =
	let (let*) = Result.bind in
	let* sri_hash =
		run_and_gather ~proc_mgr ?proc_env method' [
			"nix-hash";
			"--to-sri";
			"--type";
			algo_str;
			value;
		]
	in
	Ok sri_hash

module Hash = struct
	type t = {
		algorithm: Input.Hash.algorithm;
		value: string;
	}
	[@@deriving show]

	(* Parse SRI hash like "sha256-XpTlkQ6FXaK0SgN0bu/yji2NASPDuseVaO5bgdFROkM=" *)
	let make_from_SRI_hash (value : string) : t =
		match String.split_on_char '-' value with
		| [algo_str; _sri_value] ->
			(
				match Input.Hash.algorithm_of_string algo_str with
				| Some algorithm -> {algorithm; value}
				| None -> Jsont.Error.msgf Jsont.Meta.none "Unsupported hash algorithm: %s" algo_str
			)
		| _ -> Jsont.Error.msgf Jsont.Meta.none "Invalid SRI hash format: %s" value

	let add_sri_case obj =
		let open Jsont in
		obj |> Object.mem "hash" string
end

module File = struct
	type t = {
		path: string;
		hash_value: string;
	}

	(* hash still needs SRI treatment; env can assert it is a path *)
	let of_prefetch_stdout ?env (stdout : string) : (string * string) option =
		match String.split_on_char '\n' stdout, env with
		| hash_value :: path :: _, None ->
			Some (hash_value, path)
		| hash_value :: path :: _, Some env' when Option.is_some (Eio.Path.native (Eio.Path.(Eio.Stdenv.fs env' / path))) ->
			Some (hash_value, path)
		| _ ->
			None

	let latest_cmd (f : Input.File.t) ~(hash_algo : string) ~models ?store_name () =
		let url = Input.Template.fill f.url ~models in
		let cmd = [
			"nix-prefetch-url";
			url;
			"--print-path";
			"--type";
			hash_algo;
		]
		in
		match store_name with
		| None -> cmd
		| Some sn -> cmd @ ["--name"; Name.Store.take sn]

	let get_latest ~env ~proc_mgr ?proc_env (f : Input.File.t) ~(hash_algo : string) ?store_name ~models () =
		let (let*) = Result.bind in
		let method' = `URL in
		let* prefetch_stdout = run_and_gather ~proc_mgr ?proc_env method' (latest_cmd f ~hash_algo ?store_name ~models ()) in
		let* (non_sri_hash_value, path) =
			match of_prefetch_stdout ~env prefetch_stdout with
			| Some tup -> Ok tup
			| None -> Error (`Bad_output (method', prefetch_stdout))
		in
		let* hash_value =
			hash_to_sri
				method'
				~proc_mgr
				?proc_env
				~algo_str: hash_algo
				~value: non_sri_hash_value
		in
		Ok {path; hash_value}
end

module Archive = struct
	type t = {
		path: string;
		hash_value: string;
	}

	(* hash still needs SRI treatment; env can assert it is a path *)
	let of_prefetch_stdout ?env (stdout : string) : (string * string) option =
		match String.split_on_char '\n' stdout, env with
		| hash_value :: path :: _, None ->
			Some (hash_value, path)
		| hash_value :: path :: _, Some env' when Option.is_some (Eio.Path.native (Eio.Path.(Eio.Stdenv.fs env' / path))) ->
			Some (hash_value, path)
		| _ ->
			None

	let latest_cmd (a : Input.Archive.t) ~(hash_algo : string) ?store_name ~models () =
		let url = Input.Template.fill a.url ~models in
		let cmd = [
			"nix-prefetch-url";
			url;
			"--print-path";
			"--unpack";
			"--type";
			hash_algo;
		]
		in
		match store_name with
		| None -> cmd
		| Some sn -> cmd @ ["--name"; Name.Store.take sn]

	let get_latest ~env ~proc_mgr ?proc_env (a : Input.Archive.t) ~(hash_algo : string) ?store_name ~models () =
		let (let*) = Result.bind in
		let method' = `URL in
		let* prefetch_stdout = run_and_gather ~proc_mgr ?proc_env method' (latest_cmd a ~hash_algo ?store_name ~models ()) in
		let* (non_sri_hash_value, path) =
			match of_prefetch_stdout ~env prefetch_stdout with
			| Some tup -> Ok tup
			| None -> Error (`Bad_output (method', prefetch_stdout))
		in
		let* hash_value =
			hash_to_sri
				method'
				~proc_mgr
				?proc_env
				~algo_str: hash_algo
				~value: non_sri_hash_value
		in
		Ok {path; hash_value}
end

module Git = struct
	type t = {
		datetime: string option;
		path: string;
		rev: string;
		hash: Hash.t;
	}
	[@@deriving make, show]

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Prefetch_Git"
			(fun path datetime rev hash_str ->
				let hash = Hash.make_from_SRI_hash hash_str in
				make ~path ?datetime ~rev ~hash ()
			)
		|> Object.mem "path" string ~enc: (fun i -> i.path)
		|> Object.opt_mem "date" string ~enc: (fun i -> i.datetime)
		|> Object.mem "rev" string ~enc: (fun i -> i.rev)
		|> Object.mem "hash" string
		|> Object.finish

	let latest_cmd (g : Input.Git.t) ?store_name ~models () =
		let cmd = [
			"nix-prefetch-git";
			"--no-deepClone";
			"--quiet";
			"--url";
			Input.Template.fill g.repository ~models;
		]
		in
		List.concat [
			cmd;
			(
				match store_name with
				| None -> []
				| Some sn -> ["--name"; Name.Store.take sn]
			);
			(
				match g.reference with
				| `Branch b -> ["--rev"; Fmt.str "refs/heads/%s" b]
				| `Tag t -> ["--rev"; Fmt.str "refs/tags/%s" t]
				| `Ref r -> ["--rev"; r]
			);
			if g.submodules then ["--fetch-submodules"] else [];
			if g.lfs then ["--fetch-lfs"] else [];
		]

	let get_latest ~proc_mgr ?proc_env (g : Input.Git.t) ?store_name ~models () =
		let (let*) = Result.bind in
		let method' = `Git in
		let* stdout = run_and_gather ~proc_mgr ?proc_env method' (latest_cmd g ?store_name ~models ()) in
		Jsont_bytesrw.decode_string jsont stdout
		|> Result.map_error (fun err -> `JSON_parsing (method', err))
end

module Darcs = struct
	type t = {
		path: string;
		datetime: string option;
		context: string;
		weak_hash: string;
		hash: Hash.t;
	}
	[@@deriving make, show]

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Prefetch_Darcs"
			(fun path datetime context weak_hash hash_str ->
				let hash = Hash.make_from_SRI_hash hash_str in
				make ~path ?datetime ~context ~weak_hash ~hash ()
			)
		|> Object.mem "path" string ~enc: (fun i -> i.path)
		|> Object.opt_mem "date" string ~enc: (fun i -> i.datetime)
		|> Object.mem "context" string ~enc: (fun i -> i.context)
		|> Object.mem "weak-hash" string ~enc: (fun i -> i.weak_hash)
		|> Object.mem "hash" string
		|> Object.finish

	let latest_cmd (d : Input.Darcs.t) ~models ?store_name () =
		let cmd = ["nix-prefetch-darcs"] in
		let cmd =
			match store_name with
			| Some sn -> cmd @ ["--name"; Name.Store.take sn]
			| None -> cmd
		in
		let cmd =
			match d.reference with
			| `Context (`Assumed _) -> cmd
			| `Context (`Stated sc) -> cmd @ ["--context"; sc]
			| `Tag t -> cmd @ ["--tag"; t]
		in
		cmd @ [
			Input.Template.fill d.repository ~models;
		]

	let get_latest ~proc_mgr ?proc_env (d : Input.Darcs.t) ?store_name ~models () =
		let (let*) = Result.bind in
		let method' = `Darcs in
		let* stdout = run_and_gather ~proc_mgr ?proc_env method' (latest_cmd d ?store_name ~models ()) in
		Jsont_bytesrw.decode_string jsont stdout
		|> Result.map_error (fun err -> `JSON_parsing (method', err))
end

module Pijul = struct
	type t = {
		path: string;
		datetime: string option;
		state: string;
		hash: Hash.t
	}
	[@@deriving make, show]

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Prefetch_Pijul"
			(fun path datetime state hash_str ->
				let hash = Hash.make_from_SRI_hash hash_str in
				make ~path ?datetime ~state ~hash ()
			)
		|> Object.mem "path" string ~enc: (fun i -> i.path)
		|> Object.opt_mem "date" string ~enc: (fun i -> i.datetime)
		|> Object.mem "state" string ~enc: (fun i -> i.state)
		|> Object.mem "hash" string
		|> Object.finish

	let latest_cmd (p : Input.Pijul.t) ?store_name ~models () =
		let cmd = [
			"nix-prefetch-pijul";
			"--remote";
			Input.Template.fill p.remote ~models;
		]
		in
		let cmd =
			match store_name with
			| Some sn -> cmd @ ["--name"; Name.Store.take sn]
			| None -> cmd
		in
		cmd @
			match p.reference with
			| `Change c -> ["--change"; c]
			| `Channel c -> ["--channel"; c]
			| `State s -> ["--state"; s]

	let get_latest ~proc_mgr ?proc_env (p : Input.Pijul.t) ?store_name ~models () =
		let (let*) = Result.bind in
		let method' = `Pijul in
		let* stdout = run_and_gather ~proc_mgr ?proc_env method' (latest_cmd p ?store_name ~models ()) in
		Jsont_bytesrw.decode_string jsont stdout
		|> Result.map_error (fun err -> `JSON_parsing (method', err))
end

module Fossil = struct
	type t = {
		path: string;
		datetime: string option;
		checkin: string;
		hash: Hash.t
	}
	[@@deriving make, show]

	let jsont : t Jsont.t =
		let open Jsont in
		Object.map
			~kind: "Prefetch_Fossil"
			(fun path datetime checkin hash_str ->
				let hash = Hash.make_from_SRI_hash hash_str in
				make ~path ?datetime ~checkin ~hash ()
			)
		|> Object.mem "path" string ~enc: (fun i -> i.path)
		|> Object.opt_mem "date" string ~enc: (fun i -> i.datetime)
		|> Object.mem "rev" string ~enc: (fun i -> i.checkin)
		|> Object.mem "hash" string
		|> Object.finish

	let latest_cmd (f : Input.Fossil.t) ?store_name ~models () =
		let cmd = [
			"nix-prefetch-fossil";
			"--url";
			Input.Template.fill f.repository ~models;
		]
		in
		let cmd =
			match store_name with
			| None -> cmd
			| Some sn -> cmd @ ["--name"; Name.Store.take sn]
		in
		cmd @
			match f.reference with
			| `Branch b -> ["--rev"; b]
			| `Tag t -> ["--rev"; t]
			| `Checkin c -> ["--rev"; c]

	let get_latest ~proc_mgr ?proc_env (f : Input.Fossil.t) ?store_name ~models () =
		let (let*) = Result.bind in
		let method' = `Fossil in
		let* stdout = run_and_gather ~proc_mgr ?proc_env method' (latest_cmd f ?store_name ~models ()) in
		Jsont_bytesrw.decode_string jsont stdout
		|> Result.map_error (fun err -> `JSON_parsing (method', err))
end

type prefetch_kind_result = (
	[
		| `File of File.t
		| `Archive of Archive.t
		| `Git of Git.t
		| `Darcs of Darcs.t
		| `Pijul of Pijul.t
		| `Fossil of Fossil.t
	],
	error
) result

let get_latest ~env ~proc_mgr (input : Input.t) : prefetch_kind_result =
	let (let*) = Result.bind in
	let hash_algo = Input.Hash.algorithm_to_hash_token input.hash.algorithm in
	let proc_env = Eio.Process.Env.override [("NIX_HASH_ALGO", Some hash_algo)] (Eio.Process.environment proc_mgr) in
	let models = Input.jg_models2 input in
	let store_name = input.store_name in
	match input.kind with
	| `File file ->
		let* file = File.get_latest ~env ~proc_mgr ~proc_env file ~hash_algo ?store_name ~models () in
		Ok (`File file)
	| `Archive archive ->
		let* archive = Archive.get_latest ~env ~proc_mgr ~proc_env archive ~hash_algo ?store_name ~models () in
		Ok (`Archive archive)
	| `Git git ->
		let* git = Git.get_latest ~proc_mgr ~proc_env git ?store_name ~models () in
		Ok (`Git git)
	| `Darcs darcs ->
		let* darcs = Darcs.get_latest ~proc_mgr ~proc_env darcs ?store_name ~models () in
		Ok (`Darcs darcs)
	| `Pijul pijul ->
		let* pijul = Pijul.get_latest ~proc_mgr ~proc_env pijul ?store_name ~models () in
		Ok (`Pijul pijul)
	| `Fossil fossil ->
		let* fossil = Fossil.get_latest ~proc_mgr ~proc_env fossil ?store_name ~models () in
		Ok (`Fossil fossil)

(* nix-prefetch-scripts doesn’t expose nix-prefetch-fetchpatch2 *)
let prefetch_nix_expr ~url ~hash_value ?store_name () : string =
	let open Util.String in
	let name_attr =
		match store_name with
		| None -> ""
		| Some sn -> Fmt.str {| name = "%s";|} (escape_nix (Name.Store.take sn))
	in
	Fmt.str
		{|let pkgs = import <nixpkgs> {}; in pkgs.fetchpatch2 {url = "%s"; hash = "%s";%s}|}
		(escape_nix url)
		(escape_nix hash_value)
		name_attr

let prefetch_patch ~proc_mgr ~url ~(hash : Input.Hash.t) ?store_name ?(proc_env : Eio.Process.Env.t option) () : (string, error) result =
	let (let*) = Result.bind in
	let hash_value =
		match hash.expected with
		| None -> Input.Hash.fake_hash hash.algorithm
		| Some exp -> exp
	in
	let expr = prefetch_nix_expr ~url ~hash_value ?store_name () in
	let* drv_path =
		run_and_gather ~proc_mgr ?proc_env `URL [
			"nix-instantiate";
			"--impure";
			"--expr";
			expr;
		]
	in
	let* stderr =
		run_and_gather_error ~proc_mgr ?proc_env `URL [
			"nix-store";
			"--realize";
			drv_path;
		]
	in
	(* Look for lines like:
		"hash mismatch in fixed-output derivation …"
		"  specified: sha256:0000…"
		"  got:       sha256:abcd…"
	*)
	let got_hash =
		let needle = "got:" in
		let needle_len = String.length needle in
		String.split_on_char '\n' stderr
		|> List.find_map (fun line ->
				let line = String.trim line in
				if String.starts_with ~prefix: "got:" line then
					let drop_needle =
						String.(sub line needle_len (length line - needle_len))
					in
					Some (String.trim drop_needle)
				else
					None
			)
	in
	match got_hash with
	| Some h -> Ok h
	| None -> Error (`Bad_output (`URL, stderr))

let prefetch_patch_realize ~proc_mgr ~url ~hash_value ?store_name ?(proc_env : Eio.Process.Env.t option) () : (string, error) result =
	let (let*) = Result.bind in
	let expr = prefetch_nix_expr ~url ~hash_value ?store_name () in
	let* drv_path =
		run_and_gather ~proc_mgr ?proc_env `URL [
			"nix-instantiate";
			"--impure";
			"--expr";
			expr;
		]
	in
	run_and_gather ~proc_mgr ?proc_env `URL [
		"nix-store";
		"--realize";
		drv_path;
	]
