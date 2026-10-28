(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
type manifest_error = [
	| `Not_set_up
	| `Parsing of KDL.Valid.err list
	| `Version_not_found
]
[@@deriving show]

type error = Error.manifest_error

(* TODO: modify the KDL tree in-place to preserve comments… this is probably a
	hefty refactor since I would possibly need to drop the codec type for
	Kdl.lens (tho the concepts of “lens” & “codec” are similar) *)
let filename = "manifest.kdl"

module Template = struct
	include Input.Template

	let to_arg ?annot tpl =
		KDL.arg ?annot (`String (take tpl))

	let of_child ~name kdl =
		let open KDL.L in
		let open KDL.Valid in
		ll @@ Result.map make (kdl.@(child name // arg 0 // string_value))

	let of_mirrors kdl =
		let open KDL.L in
		let open KDL.Valid in
		kdl.@(child "mirrors" // args // each string_value)
		|> ll
		|> map (List.map make)
		|> optional ~default: []
end

module File = struct
	type t = {
		fetch_time: Input.Fetch_time.t option;
		url: Template.t;
		mirrors: Template.t list;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest ({url; mirrors; fetch_time; _}: Input.File.t) : t =
		make ~url ~mirrors ?fetch_time ()

	let [@inline]of_manifest ({url; mirrors; fetch_time}: t) : Input.File.t =
		Input.File.make ~url ~mirrors ?fetch_time ()

	let codec : t KDL.codec = {
		to_kdl = (fun file ->
			let open KDL in
			let props =
				match file.fetch_time with
				| None ->
					[]
				| Some ft ->
					["fetch-time", arg (`String (Input.Fetch_time.to_string ft))]
			and nodes =
				if List.is_empty file.mirrors then
						[]
				else
						[node "mirrors" ~args: (List.map Template.to_arg file.mirrors) []]
			in
			let nodes =
				node "url" ~args: [Template.to_arg file.url] [] :: nodes
			in
				[node "file" ~props nodes]
		);
		of_kdl = (fun kdl ->
			let open KDL.L in
			let open KDL.Valid in
			let* file = ll @@ kdl.@(node "file") in
			let+ fetch_time =
				file.@(prop "fetch-time" // string_value)
				|> ll
				|> optional_bind ~default: None (fun ftv ->
						match Input.Fetch_time.of_string ftv with
						| Some ft -> Ok (Some ft)
						| None -> Error [`Invalid_fetch_time ftv]
					)
			and+ url = Template.of_child ~name: "url" file
			and+ mirrors = Template.of_mirrors file
			in
				{fetch_time; url; mirrors}
		);
	}
end

module Archive = struct
	type t = {
		fetch_time: Input.Fetch_time.t option;
		url: Template.t;
		mirrors: Template.t list;
		extension: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest ({fetch_time; url; mirrors; extension; _}: Input.Archive.t) : t =
		make ?fetch_time ~url ~mirrors ?extension ()

	let [@inline]of_manifest ({fetch_time; url; mirrors; extension}: t) : Input.Archive.t =
		Input.Archive.make ?fetch_time ~url ~mirrors ?extension ()

	let codec : t KDL.codec = {
		to_kdl = (fun archive ->
			let open KDL in
			let props =
				match archive.fetch_time with
				| None ->
					[]
				| Some ft ->
					["fetch-time", arg (`String (Input.Fetch_time.to_string ft))]
			and url =
				node "url" ~args: [Template.to_arg archive.url] [];
			and nodes =
				match archive.extension with
				| None -> []
				| Some ext -> [node "extension" ~args: [arg (`String ext)] []]
			in
			let nodes =
				if List.is_empty archive.mirrors then
					nodes
				else
					node "mirrors" ~args: (List.map Template.to_arg archive.mirrors) [] :: nodes
			in
			let nodes = url :: nodes in
				[node "archive" ~props nodes]
		);
		of_kdl = (fun kdl ->
			let open KDL.L in
			let open KDL.Valid in
			let* archive = ll @@ kdl.@(node "archive") in
			let+ fetch_time =
				archive.@(prop "fetch-time" // string_value)
				|> ll
				|> optional_bind ~default: None (fun ftv ->
						match Input.Fetch_time.of_string ftv with
						| Some ft -> Ok (Some ft)
						| None -> Error [`Invalid_fetch_time ftv]
					)
			and+ url = Template.of_child ~name: "url" archive
			and+ mirrors = Template.of_mirrors archive
			and+ extension : UTF8.t option =
				archive.@(child "extension" // arg 0 // string_value)
				|> ll
				|> find
			in
				{fetch_time; url; mirrors; extension}
		);
	}
end

module Git = struct
	module Reference = struct
		type t = Input.Git.Reference.t
		[@@deriving show, eq, qcheck]

		let codec : t KDL.codec = {
			to_kdl = (fun ref ->
				let open KDL in
				match ref with
				| `Branch b -> [KDL.node "branch" ~args: [arg (`String b)] []]
				| `Tag t -> [KDL.node "tag" ~args: [arg (`String t)] []]
				| `Ref r -> [KDL.node "ref" ~args: [arg (`String r)] []]
			);
			of_kdl = (fun kdl ->
				let open KDL.L in
				let open KDL.Valid in
				let node_names = ["branch"; "tag"; "ref"]
				and branch = ll @@ kdl.@(node "branch" // arg 0 // string_value)
				and tag = ll @@ kdl.@(node "tag" // arg 0 // string_value)
				and ref = ll @@ kdl.@(node "ref" // arg 0 // string_value)
				in
				match present
					~absent: (fun _ _ -> true)
					~named: [
						"branch", map (fun b -> `Branch b) branch;
						"tag", map (fun t -> `Tag t) tag;
						"ref", map (fun r -> `Ref r) ref;
					] with
				| One (r, _) -> Ok r
				| All_absent | Broken _ -> Error [`One_required node_names]
				| Clash _ -> Error [`Only_one_of node_names]
			);
		}
	end

	type t = {
		fetch_time: Input.Fetch_time.t option;
		repository: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
		submodules: bool; [@default false]
		lfs: bool; [@default false]
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest
			({fetch_time; repository; mirrors; reference; submodules; lfs; _}: Input.Git.t)
			: t
		=
		make ?fetch_time ~repository ~mirrors ~reference ~submodules ~lfs ()

	let [@inline]of_manifest
			({fetch_time; repository; mirrors; reference; submodules; lfs}: t)
			: Input.Git.t
		=
		Input.Git.make ?fetch_time ~repository ~mirrors ~reference ~submodules ~lfs ()

	let codec : t KDL.codec = {
		to_kdl = (fun git ->
			let open KDL in
			let props =
				match git.fetch_time with
				| None ->
					[]
				| Some ft ->
					["fetch-time", arg (`String (Input.Fetch_time.to_string ft))]
			and repository =
				node "repository" ~args: [Template.to_arg git.repository] []
			and nodes =
				if git.lfs then [node "lfs" []] else []
			in
			let nodes =
				if git.submodules then node "submodules" [] :: nodes else nodes
			in
			let nodes =
				match git.reference with
				| `Branch b -> node "branch" ~args: [arg (`String b)] [] :: nodes
				| `Tag t -> node "tag" ~args: [arg (`String t)] [] :: nodes
				| `Ref r -> node "ref" ~args: [arg (`String r)] [] :: nodes
			in
			let nodes =
				if List.is_empty git.mirrors then
					nodes
				else
					node "mirrors" ~args: (List.map Template.to_arg git.mirrors) [] :: nodes
			in
			let nodes = repository :: nodes in
				[node "git" ~props nodes]
		);
		of_kdl = (fun kdl ->
			let open KDL.L in
			let open KDL.Valid in
			let* git = ll @@ kdl.@(node "git") in
			let+ fetch_time =
				git.@(prop "fetch-time" // string_value)
				|> ll
				|> optional_bind ~default: None (fun ftv ->
						match Input.Fetch_time.of_string ftv with
						| Some ft -> Ok (Some ft)
						| None -> Error [`Invalid_fetch_time ftv]
					)
			and+ repository = Template.of_child ~name: "repository" git
			and+ mirrors = Template.of_mirrors git
			and+ reference = Reference.codec.of_kdl git.children
			and+ submodules =
				match git.@(child "submodules") |> ll |> find with
				| Ok None -> Ok false
				| Ok (Some sms) ->
					ll @@ (
						match sms.@(arg 0 // bool_value) with
						| Ok smb -> Ok smb
						| Error (`Missing_index 0) -> Ok true
						| Error err -> Error err
					)
				| Error errs -> Error errs
			and+ lfs =
				match git.@(child "lfs") |> ll |> find with
				| Ok None -> Ok false
				| Ok (Some sms) ->
					ll @@ (
						match sms.@(arg 0 // bool_value) with
						| Ok smb -> Ok smb
						| Error (`Missing_index 0) -> Ok true
						| Error err -> Error err
					)
				| Error errs -> Error errs
			in
				{fetch_time; repository; mirrors; reference; submodules; lfs}
		);
	}
end

module Darcs = struct
	module Reference = struct
		type context_grounds = [
			| `Assumed of UTF8.t option
			| `Stated of UTF8.t
		]
		[@@deriving show, eq]

		let gen_context_grounds =
			let open QCheck.Gen in
			oneof [
				return (`Assumed None);
				map (fun s -> `Stated s) UTF8.gen;
			]

		type t = [
			| `Context of context_grounds
			| `Tag of UTF8.t
		]
		[@@deriving show, eq, qcheck]

		let codec : t KDL.codec = {
			to_kdl = (fun ref ->
				let open KDL in
				match ref with
				| `Context (`Stated sc) -> [KDL.node "context" ~args: [arg (`String sc)] []]
				| `Context (`Assumed _) -> []
				| `Tag t -> [KDL.node "tag" ~args: [arg (`String t)] []]
			);
			of_kdl = (fun kdl ->
				let open KDL.L in
				let open KDL.Valid in
				let node_names = ["tag"; "context"]
				and context = ll @@ kdl.@(node "context" // arg 0 // string_value)
				and tag = ll @@ kdl.@(node "tag" // arg 0 // string_value)
				in
				match present
					~absent: (fun _ _ -> true)
					~named: [
						"context", map (fun c -> `Context (`Stated c)) context;
						"tag", map (fun t -> `Tag t) tag;
					] with
				| One (r, _) -> Ok r
				| All_absent | Broken _ -> Ok (`Context (`Assumed None))
				| Clash _ -> Error [`Only_one_of node_names]
			);
		}
	end

	type t = {
		repository: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest ({repository; mirrors; reference; _}: Input.Darcs.t) : t =
		make ~repository ~mirrors ~reference ()

	let [@inline]of_manifest ({repository; mirrors; reference}: t) : Input.Darcs.t =
		Input.Darcs.make ~repository ~mirrors ~reference ()

	let codec : t KDL.codec = {
		to_kdl = (fun darcs ->
			let open KDL in
			let repository =
				node "repository" ~args: [Template.to_arg darcs.repository] []
			and nodes =
				Reference.codec.to_kdl darcs.reference
			in
			let nodes =
				if List.is_empty darcs.mirrors then
					nodes
				else
					node "mirrors" ~args: (List.map Template.to_arg darcs.mirrors) [] :: nodes
			in
			let nodes = repository :: nodes in
				[node "darcs" nodes]
		);
		of_kdl = (fun kdl ->
			let open KDL.L in
			let open KDL.Valid in
			let* darcs = ll @@ kdl.@(node "darcs") in
			let+ repository = Template.of_child ~name: "repository" darcs
			and+ mirrors = Template.of_mirrors darcs
			and+ reference = Reference.codec.of_kdl darcs.children
			in
				{repository; mirrors; reference}
		);
	}
end

module Pijul = struct
	module Reference = struct
		type t = Input.Pijul.Reference.t
		[@@deriving show, eq, qcheck]

		let codec : t KDL.codec = {
			to_kdl = (fun ref ->
				let open KDL in
				match ref with
				| `Channel c -> [KDL.node "channel" ~args: [arg (`String c)] []]
				| `State s -> [KDL.node "state" ~args: [arg (`String s)] []]
				| `Change c -> [KDL.node "change" ~args: [arg (`String c)] []]
			);
			of_kdl = (fun kdl ->
				let open KDL.L in
				let open KDL.Valid in
				let node_names = ["channel"; "state"; "change"]
				and channel = ll @@ kdl.@(node "channel" // arg 0 // string_value)
				and state = ll @@ kdl.@(node "state" // arg 0 // string_value)
				and change = ll @@ kdl.@(node "change" // arg 0 // string_value)
				in
				match present
					~absent: (fun _ _ -> true)
					~named: [
						"channel", map (fun c -> `Channel c) channel;
						"state", map (fun s -> `State s) state;
						"change", map (fun c -> `Change c) change;
					] with
				| One (r, _) -> Ok r
				| All_absent | Broken _ -> Error [`One_required node_names]
				| Clash _ -> Error [`Only_one_of node_names]
			);
		}
	end

	type t = {
		remote: Template.t;
		mirrors: Template.t list;
		reference: Reference.t;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest ({remote; mirrors; reference; _}: Input.Pijul.t) : t =
		make ~remote ~mirrors ~reference ()

	let [@inline]of_manifest ({remote; mirrors; reference}: t) : Input.Pijul.t =
		Input.Pijul.make ~remote ~mirrors ~reference ()

	let codec : t KDL.codec = {
		to_kdl = (fun pijul ->
			let open KDL in
			let remote =
				node "remote" ~args: [Template.to_arg pijul.remote] []
			and nodes =
				Reference.codec.to_kdl pijul.reference
			in
			let nodes =
				if List.is_empty pijul.mirrors then
					nodes
				else
					node "mirrors" ~args: (List.map Template.to_arg pijul.mirrors) [] :: nodes
			in
			let nodes = remote :: nodes in
				[node "pijul" nodes]
		);
		of_kdl = (fun kdl ->
			let open KDL.L in
			let open KDL.Valid in
			let* pijul = ll @@ kdl.@(node "pijul") in
			let+ remote = Template.of_child ~name: "remote" pijul
			and+ mirrors = Template.of_mirrors pijul
			and+ reference = Reference.codec.of_kdl pijul.children
			in
				{remote; mirrors; reference}
		);
	}
end

module Fossil = struct
	module Reference = struct
		type t = Input.Fossil.Reference.t
		[@@deriving show, eq, qcheck]

		let codec : t KDL.codec = {
			to_kdl = (fun ref ->
				let open KDL in
				match ref with
				| `Branch c -> [KDL.node "branch" ~args: [arg (`String c)] []]
				| `Tag s -> [KDL.node "tag" ~args: [arg (`String s)] []]
				| `Checkin c -> [KDL.node "check-in" ~args: [arg (`String c)] []]
			);
			of_kdl = (fun kdl ->
				let open KDL.L in
				let open KDL.Valid in
				let node_names = ["branch"; "tag"; "check-in"]
				and branch = ll @@ kdl.@(node "branch" // arg 0 // string_value)
				and tag = ll @@ kdl.@(node "tag" // arg 0 // string_value)
				and checkin = ll @@ kdl.@(node "check-in" // arg 0 // string_value)
				in
				match present
					~absent: (fun _ _ -> true)
					~named: [
						"branch", map (fun b -> `Branch b) branch;
						"tag", map (fun t -> `Tag t) tag;
						"check-in", map (fun c -> `Checkin c) checkin;
					] with
				| One (r, _) -> Ok r
				| All_absent | Broken _ -> Error [`One_required node_names]
				| Clash _ -> Error [`Only_one_of node_names]
			);
		}
	end

	type t = {
		repository: Template.t;
		reference: Reference.t;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest ({repository; reference; _}: Input.Fossil.t) : t =
		make ~repository ~reference

	let [@inline]of_manifest ({repository; reference}: t) : Input.Fossil.t =
		Input.Fossil.make ~repository ~reference ()

	let codec : t KDL.codec = {
		to_kdl = (fun fossil ->
			let open KDL in
			let repository =
				node "repository" ~args: [Template.to_arg fossil.repository] []
			and nodes =
				Reference.codec.to_kdl fossil.reference
			in
			let nodes = repository :: nodes in
				[node "fossil" nodes]
		);
		of_kdl = (fun kdl ->
			let open KDL.L in
			let open KDL.Valid in
			let* fossil = ll @@ kdl.@(node "fossil") in
			let+ repository = Template.of_child ~name: "repository" fossil
			and+ reference = Reference.codec.of_kdl fossil.children
			in
				{repository; reference}
		);
	}
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

	let to_manifest : Input.Kind.t -> t = function
		| `File f -> `File (File.to_manifest f)
		| `Archive a -> `Archive (Archive.to_manifest a)
		| `Git g -> `Git (Git.to_manifest g)
		| `Darcs d -> `Darcs (Darcs.to_manifest d)
		| `Pijul p -> `Pijul (Pijul.to_manifest p)
		| `Fossil f -> `Fossil (Fossil.to_manifest f)

	let of_manifest : t -> Input.Kind.t = function
		| `File f -> `File (File.of_manifest f)
		| `Archive a -> `Archive (Archive.of_manifest a)
		| `Git g -> `Git (Git.of_manifest g)
		| `Darcs d -> `Darcs (Darcs.of_manifest d)
		| `Pijul p -> `Pijul (Pijul.of_manifest p)
		| `Fossil f -> `Fossil (Fossil.of_manifest f)

	let codec : t KDL.codec = {
		to_kdl = (function
			| `File f -> File.codec.to_kdl f
			| `Archive a -> Archive.codec.to_kdl a
			| `Git g -> Git.codec.to_kdl g
			| `Darcs d -> Darcs.codec.to_kdl d
			| `Pijul p -> Pijul.codec.to_kdl p
			| `Fossil f -> Fossil.codec.to_kdl f
		);
		of_kdl = (fun kdl ->
			let kind_names = ["file"; "archive"; "git"; "darcs"; "pijul"; "fossil"] in
			let variant_of_name name =
				List.assoc name [
					"file", `File;
					"archive", `Archive;
					"git", `Git;
					"darcs", `Darcs;
					"pijul", `Pijul;
					"fossil", `Fossil;
				]
			in
			let open KDL.Valid in
			match present
				~absent: (fun name errs ->
					match errs with
					| [] -> true
					| [`Not_found (n, _)] -> String.equal name n
					| _ -> false
				)
				~named: [
					"file", map (fun f -> `File f) @@ File.codec.of_kdl kdl;
					"archive", map (fun a -> `Archive a) @@ Archive.codec.of_kdl kdl;
					"git", map (fun g -> `Git g) @@ Git.codec.of_kdl kdl;
					"darcs", map (fun d -> `Darcs d) @@ Darcs.codec.of_kdl kdl;
					"pijul", map (fun p -> `Pijul p) @@ Pijul.codec.of_kdl kdl;
					"fossil", map (fun f -> `Fossil f) @@ Fossil.codec.of_kdl kdl;
				] with
			| One (kind, _) -> Ok kind
			| All_absent -> Error [`One_required kind_names]
			| Broken [(name, errs)] when errs <> [] ->
				Error [`Invalid_kind (variant_of_name name, errs)]
			| Broken _ | Clash _ -> Error [`Only_one_of kind_names]
		);
	}
end

module Hash = struct
	type t = {
		algorithm: Input.Hash.algorithm; [@default !Input.Hash.default_algorithm]
		expected: UTF8.t option;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest ({algorithm; expected; _}: Input.Hash.t) : t =
		make ~algorithm ?expected ()

	let [@inline]of_manifest ({algorithm; expected}: t) : Input.Hash.t =
		Input.Hash.make ~algorithm ?expected ()

	let codec : t KDL.codec = {
		to_kdl = (fun hash ->
			let open KDL in
			let props =
				match hash.expected with
				| None -> []
				| Some exp_hash ->
					["expected", (None, `String exp_hash)]
			in
			let props =
				let algo_str = Input.Hash.algorithm_to_string hash.algorithm in
				("algorithm", (None, `String algo_str)) :: props
			in
				[node "hash" ~props []]
		);
		of_kdl = (fun kdl ->
			let open KDL.L in
			let open KDL.Valid in
			let* hash = ll @@ kdl.@(node "hash") in
			let+ algorithm : Input.Hash.algorithm =
				hash.@(prop "algorithm" // string_value)
				|> ll
				|> optional_bind ~default: !Input.Hash.default_algorithm (fun algo_val ->
						match Input.Hash.algorithm_of_string algo_val with
						| Some av -> Ok av
						| None ->
							let len : int = Input.Hash.max_algorithm - Input.Hash.min_algorithm + 1
							and algo_str (i : int) : UTF8.t =
								i + Input.Hash.min_algorithm
								|> Input.Hash.algorithm_of_enum
								|> Option.get
								|> Input.Hash.algorithm_to_string
							in
							let algo_str_list : UTF8.t list = List.init len algo_str in
							Logs.err (fun m ->
								m
									"Got hash algorithm “%s”, but exepected one of %a"
									algo_val
									Fmt.(brackets (list ~sep: semi string))
									algo_str_list
							);
							Error [`One_required algo_str_list]
					)
			and+ expected : UTF8.t option =
				hash.@(prop "expected" // string_value)
				|> ll
				|> find
			in
			make ~algorithm ?expected ()
		);
	}

	let of_input : Input.Hash.t -> t = fun {Input.Hash.algorithm; expected; _} ->
		make ~algorithm ?expected ()

	let to_input : t -> Input.Hash.t = fun {algorithm; expected} ->
		Input.Hash.make ~algorithm ?expected ()

	let default : t = make ()
end

module Fresh_cmd = struct
	type t = Input.Fresh.Cmd.t option
	[@@deriving show, eq, qcheck]

	let [@inline]to_manifest (latest : Input.Fresh.t) : t =
		latest.cmd

	let [@inline]of_manifest (cmd : t) : Input.Fresh.t =
		Input.Fresh.make ?cmd ()

	let codec : t KDL.codec = {
		to_kdl = (function
			| None -> []
			| Some (exec, pipes) ->
				let open KDL in
				let cmd_args ({prog; args}: Input.Fresh.Cmd.cmd) =
					List.map (Template.to_arg) (prog :: args)
				in
				let nodes =
					List.map (fun pcmd -> node "|" ~args: (cmd_args pcmd) []) pipes
				in
				let nodes =
					node "$" ~args: (cmd_args exec) [] :: nodes
				in
					[node "fresh-cmd" nodes]
		);
		of_kdl = (fun kdl ->
			let open KDL.L in
			let open KDL.Valid in
			let extract_cmd (node : KDL.node) : Input.Fresh.Cmd.cmd KDL.Valid.t =
				if List.is_empty node.props then
					match Util.Non_empty_list.of_list node.args with
					| Some (arg_prog, arg_args) ->
						begin
							match arg_prog.@(string_value), arg_args.@(each string_value) with
							| Error _err, _ -> Error [`Invalid_fresh_cmd "Bad program specified for command"]
							| _, Error _err -> Error [`Invalid_fresh_cmd "Bad arguments specified for command"]
							| Ok prog, Ok args ->
								Ok {
									prog = Template.make prog;
									args = List.map Template.make args;
								}
						end
					| None -> Error [`Invalid_fresh_cmd "Empty command"]
				else
					Error [`Invalid_fresh_cmd "Props aren’t supported (yet?); you probably meant to add straight quotation marks (for example “\"--foo=bar\"”)."]
			in
			match kdl.@(node "fresh-cmd") |> ll |> find with
			| Ok None -> Ok None
			| Ok (Some fresh_cmd) ->
				let+ exec : Input.Fresh.Cmd.cmd =
					let* exec' = ll @@ fresh_cmd.@(child ~nth: 0 "$") in
					extract_cmd exec'
				and+ pipes : Input.Fresh.Cmd.cmd list =
					let* ps = fresh_cmd.@(child_many "|") |> ll |> optional ~default: [] in
					all (List.map extract_cmd ps)
				in
				let cmd : Input.Fresh.Cmd.t = (exec, pipes) in
				Some cmd
			| Error errs -> Error errs
		);
	}
end

module Patch = struct
	module Source = struct
		type remote = {
			url: Template.t;
			fresh_cmd: Fresh_cmd.t;
			hash: Hash.t;
			frozen: bool; [@default false]
			gc: Input.GC.t option;
			store_name: Name.Store.t option;
			vendored: bool; [@default false]
		}
		[@@deriving show, eq, qcheck]

		let default_vendored = ref false

		type t =
			| Local of string
			| Remote of remote
		[@@deriving show, eq, qcheck]

		let gen : t QCheck.Gen.t =
			let open QCheck.Gen in
			let a_to_z = char_range 'a' 'z' in
			oneof_weighted [
				1, map (fun s -> Local ("./" ^ s)) (string_size ~gen: a_to_z (int_bound 20));
				3, map (fun rem -> Remote rem) gen_remote;
			]
	end

	type t = {
		name: Name.Patch.t;
		source: Source.t;
	}
	[@@deriving show, make, eq, qcheck]

	let [@inline]to_manifest (patch : Input.Patch.t) : t =
		let source =
			match patch.source with
			| Input.Patch.Source.Local path ->
				Source.Local path
			| Input.Patch.Source.Remote remote ->
				Source.Remote {
					url = remote.url;
					fresh_cmd = Fresh_cmd.to_manifest remote.fresh;
					hash = Hash.to_manifest remote.hash;
					frozen = remote.frozen;
					gc = remote.gc;
					store_name = remote.store_name;
					vendored = remote.vendored;
				}
		in
			{name = patch.name; source}

	let [@inline]of_manifest (mnfst : t) : Input.Patch.t =
		let source =
			match mnfst.source with
			| Source.Local path ->
				Input.Patch.Source.Local path
			| Source.Remote remote ->
				let fresh = Fresh_cmd.of_manifest remote.fresh_cmd in
				let hash = Hash.of_manifest remote.hash in
				let rem =
					Input.Patch.Source.make_remote
						~url: remote.url
						~fresh
						~hash
						~frozen: remote.frozen
						?gc: remote.gc
						?store_name: remote.store_name
						~vendored: remote.vendored
						()
				in
				Input.Patch.Source.Remote rem
		in
		Input.Patch.make ~name: mnfst.name ~source

	let codec : t KDL.node_codec = {
		to_node = (fun patch ->
			let open KDL in
			let name = Name.Patch.take patch.name in
			match patch.source with
			| Source.Local path ->
				node name ~args: [arg (`String path)] []
			| Source.Remote remote ->
				let hash = Hash.codec.to_kdl remote.hash
				and fresh_cmd = Fresh_cmd.codec.to_kdl remote.fresh_cmd
				in
				let nodes = hash @ fresh_cmd
				and args = [Template.to_arg remote.url]
				and props =
					if remote.frozen then
							[("frozen", arg (`Bool true))]
					else
							[]
				in
				let props =
					match remote.gc with
					| Some gc -> ("gc", arg (`String (Input.GC.to_string gc))) :: props
					| None -> props
				in
				let props =
					match remote.store_name with
					| None -> props
					| Some sn -> ("name", arg (`String (Name.Store.take sn))) :: props
				in
				let props =
					if remote.vendored then ("vendor", arg (`Bool true)) :: props else props
				in
				node name ~args ~props nodes
		);
		of_node = (fun patch ->
			let open KDL.L in
			let open KDL.Valid in
			let* name, source =
				let+ name' =
					ll @@ patch.@(node_name)
				and+ source' =
					ll @@ patch.@(arg 0 // string_value)
				in
					(Name.Patch.make name', source')
			in
			let warn_if_vendor () =
				match patch.@(prop "vendor" // bool_value) with
				| Ok true ->
					Logs.warn (fun m ->
						m "Patch %a is local, so vendoring does not apply; ignoring `vendor`" Name.Patch.pp name
					)
				| Ok false | Error _ -> ()
			in
			if String.starts_with ~prefix: "./" source || String.starts_with ~prefix: "../" source then
				(
					warn_if_vendor ();
					Ok {
						name;
						source = Source.Local source
					}
				)
			else if String.starts_with ~prefix: "file:./" source || String.starts_with ~prefix: "file:../" source then
				(
					warn_if_vendor ();
					Ok {
						name;
						source = Source.Local (String.sub source 5 (String.length source - 5))
					}
				)
			else
				let+ fresh_cmd =
					Fresh_cmd.codec.of_kdl patch.children
				and+ hash =
					Hash.codec.of_kdl patch.children |> optional ~default: (Hash.make ())
				and+ frozen =
					patch.@(prop "frozen" // bool_value) |> ll |> optional ~default: false
				and+ gc =
					patch.@(prop "gc" // string_value)
					|> ll
					|> optional_bind ~default: None (fun str_val ->
							match Input.GC.of_string str_val with
							| Some gc -> Ok (Some gc)
							| None -> Error [`Invalid_GC str_val]
						)
				and+ store_name =
					patch.@(prop "name" // string_value)
					|> ll
					|> map Name.Store.make
					|> find
				and+ vendored =
					patch.@(prop "vendor" // bool_value) |> ll |> optional ~default: !Source.default_vendored
				in
				{
					name;
					source = Source.Remote {url = Template.make source; fresh_cmd; hash; frozen; gc; store_name; vendored}
				}
		);
	}
end

module Input' = struct
	type t = {
		name: Name.Input.t;
		store_name: Name.Store.t option;
		kind: Kind.t;
		fresh_cmd: Fresh_cmd.t;
		hash: Hash.t;
		frozen: bool; [@default false]
		gc: Input.GC.t option;
		patches: Name.Patch.t list;
	}
	[@@deriving show, eq, make, qcheck]

	let [@inline]to_manifest (input : Input.t) : t = {
		name = input.name;
		store_name = input.store_name;
		kind = Kind.to_manifest input.kind;
		fresh_cmd = Fresh_cmd.to_manifest input.fresh;
		hash = Hash.to_manifest input.hash;
		frozen = input.frozen;
		gc = input.gc;
		patches = input.patches;
	}

	let [@inline]of_manifest (mnfst : t) : Input.t = {
		name = mnfst.name;
		store_name = mnfst.store_name;
		fetch_status = Fetch_status.default;
		kind = Kind.of_manifest mnfst.kind;
		fresh = Fresh_cmd.of_manifest mnfst.fresh_cmd;
		hash = Hash.of_manifest mnfst.hash;
		frozen = mnfst.frozen;
		gc = mnfst.gc;
		patches = mnfst.patches;
	}

	let codec : t KDL.node_codec = {
		to_node = (fun input ->
			let open KDL in
			let props =
				if input.frozen then
						[("frozen", arg (`Bool true))]
				else
						[]
			in
			let props =
				match input.gc with
				| Some gc -> ("gc", arg (`String (Input.GC.to_string gc))) :: props
				| None -> props
			in
			let props =
				match input.store_name with
				| None -> props
				| Some sn -> ("name", arg (`String (Name.Store.take sn))) :: props
			in
			let kind = Kind.codec.to_kdl input.kind
			and hash = Hash.codec.to_kdl input.hash
			and fresh_cmd = Fresh_cmd.codec.to_kdl input.fresh_cmd
			and patches =
				if List.is_empty input.patches then
						[]
				else
						[node "patches" ~args: (List.map (fun p -> arg (`String (Name.Patch.take p))) input.patches) []]
			in
			let nodes = kind @ hash @ fresh_cmd @ patches in
			node (Name.Input.take input.name) ~props nodes
		);
		of_node = (fun input ->
			let open KDL.L in
			let open KDL.Valid in
			let* input' =
				let+ name =
					ll @@ input.@(node_name)
					|> Result.map Name.Input.make
				and+ fresh_cmd =
					Fresh_cmd.codec.of_kdl input.children
				and+ hash =
					Hash.codec.of_kdl input.children |> optional ~default: (Hash.make ())
				and+ kind =
					Kind.codec.of_kdl input.children
				and+ frozen =
					input.@(prop "frozen" // bool_value) |> ll |> optional ~default: false
				and+ gc =
					input.@(prop "gc" // string_value)
					|> ll
					|> optional_bind ~default: None (fun str_val ->
							match Input.GC.of_string str_val with
							| Some gc -> Ok (Some gc)
							| None -> Error [`Invalid_GC str_val]
						)
				and+ patches =
					input.@(child "patches" // args // each string_value)
					|> ll
					|> map (List.map Name.Patch.make)
					|> optional ~default: []
				and+ store_name =
					input.@(prop "name" // string_value)
					|> ll
					|> map Name.Store.make
					|> find
				in
					{name; store_name; kind; fresh_cmd; hash; frozen; gc; patches}
			in
			(* validate features at odds with eval fetching; state if needed *)
			if List.is_empty input'.patches then
				(* no patches, no worries *)
				Ok input'
			else
				match input'.kind with
				| `File {fetch_time; _}
				| `Archive {fetch_time; _}
				| `Git {fetch_time; _} when fetch_time = Some Input.Fetch_time.Eval ->
					Error [`Eval_fetch_time_at_odds_with `Patches]
				| _ ->
					Ok input'
		);
	}
end

type t = {
	version: Schema.Version.t; [@default Lazy.(force Schema.Version.latest)]
	default_hash_algorithm: Input.Hash.algorithm option;
	default_fetch_time: Input.Fetch_time.t option;
	default_gc: Input.GC.t option;
	default_vendor_patch: bool;
	inputs: Input'.t list;
	patches: Patch.t list;
}
[@@deriving show, eq, make]

let is_document_latest_version (doc : KDL.t) : bool =
	let open KDL.L in
	match doc.@(node "version" // arg 0 // string_value) with
	| Error _ -> false
	| Ok version_str ->
		match Schema.Version.of_string version_str with
		| Some v -> v = Lazy.(force Schema.Version.latest)
		| None -> false

let document_to_t (doc : KDL.t) : t KDL.Valid.t =
	let open KDL.L in
	let open KDL.Valid in
	let* manifest_default_hash_algorithm : Input.Hash.algorithm option =
		doc.@(node "default-hash-algorithm" // arg 0 // string_value)
		|> ll
		|> optional_bind ~default: None (fun dha ->
				match Input.Hash.algorithm_of_string dha with
				| Some ha -> Ok (Some ha)
				| None -> Error [`Invalid_hash_algorithm dha]
			)
	in
	let* manifest_default_fetch_time : Input.Fetch_time.t option =
		doc.@(node "default-fetch-time" // arg 0 // string_value)
		|> ll
		|> optional_bind ~default: None (fun dft ->
				match Input.Fetch_time.of_string dft with
				| Some ft -> Ok (Some ft)
				| None -> Error [`Invalid_fetch_time dft]
			)
	in
	let* manifest_default_gc : Input.GC.t option =
		doc.@(node "default-gc" // arg 0 // string_value)
		|> ll
		|> optional_bind ~default: None (fun str ->
				match Input.GC.of_string str with
				| Some gc -> Ok (Some gc)
				| None -> Error [`Invalid_GC str]
			)
	in
	let* manifest_default_vendor_patch : bool =
		doc.@(node "default-vendor-patch" // arg 0 // bool_value)
		|> ll
		|> optional ~default: false
	in
	(* IMPURE: setting default values *)
	Option.iter (fun dha -> Input.Hash.default_algorithm := dha) manifest_default_hash_algorithm;
	Option.iter (fun dft -> Input.Fetch_time.default := dft) manifest_default_fetch_time;
	Option.iter (fun dgc -> Input.GC.default := dgc) manifest_default_gc;
	Patch.Source.default_vendored := manifest_default_vendor_patch;
	(* /IMPURE *)
	let* version : Schema.Version.t =
		let* version_str = ll @@ doc.@(node "version" // arg 0 // string_value) in
		required ~none: (`Invalid_version version_str) (Schema.Version.of_string version_str)
	in
	let patches : Patch.t list =
		match doc.@(node "patches" // children) |> ll |> optional ~default: [] with
		| Ok patch_nodes ->
			List.filter_map
				(fun (patch_node : KDL.node) ->
					match Patch.codec.of_node patch_node with
					| Ok patch -> Some patch
					| Error errs ->
						Logs.warn (fun m ->
							m "Failed to parse patch %s: %a" patch_node.name (Fmt.list KDL.Valid.pp_err) errs
						);
						None
				)
				patch_nodes
		| Error errs ->
			Logs.warn (fun m ->
				m "Error parsing patches section: %a" (Fmt.list KDL.Valid.pp_err) errs
			);
			[]
	in
	let* inputs : Input'.t list =
		let* nodes = doc.@(node "inputs" // children) |> ll in
		all (List.map Input'.codec.of_node nodes)
	in
	Ok {
		version;
		default_hash_algorithm = manifest_default_hash_algorithm;
		default_fetch_time = manifest_default_fetch_time;
		default_gc = manifest_default_gc;
		default_vendor_patch = manifest_default_vendor_patch;
		patches;
		inputs;
	}

let manifest : KDL.t option ref = ref None

let exists () : bool =
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	Eio.Path.is_file filepath

let read () =
	let (let*) = Result.bind in
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	Logs.info (fun m -> m "Reading manifest @@ %a …" Eio.Path.pp filepath);
	let* kdl =
		Eio.Path.with_open_in ~follow: false filepath @@ fun flow ->
		KDL.of_flow flow
	in
	let () = manifest := Some kdl in
	Ok kdl

let make
		?(version = Lazy.(force Schema.Version.latest))
		?default_hash_algorithm
		?default_fetch_time
		?default_gc
		?(patches = [])
		()
		: unit
	=
	Logs.app (fun m -> m "Making manifest file @@ version:%s" (Schema.Version.to_string version));
	let open KDL in
	let inputs_node =
		node "inputs" (
			Foreman.to_manifest (fun s ->
				let open Input' in
					[codec.to_node (to_manifest s)]
			)
		)
	in
	let patches_node =
		if List.is_empty patches then
				[]
		else
				[node "patches" (List.map Patch.codec.to_node patches)]
	in
	let default_hash_algorithm_node =
		match default_hash_algorithm with
		| None ->
			[]
		| Some dha ->
			[node "default-hash-algorithm" ~args: [arg (`String (Input.Hash.algorithm_to_string dha))] []]
	and default_fetch_time_node =
		match default_fetch_time with
		| None ->
			[]
		| Some dft ->
			let args = [
				arg (`String (Input.Fetch_time.to_string dft))
			]
			in
				[node "default-fetch-time" ~args []]
	and default_gc_node =
		match default_gc with
		| None ->
			[]
		| Some dgc ->
			[node "default-gc" ~args: [arg (`String (Input.GC.to_string dgc))] []]
	in
	let doc =
		node "version" ~args: [arg (`String (Schema.Version.to_string version))] [] :: default_hash_algorithm_node @
		default_fetch_time_node @
		default_gc_node @
		inputs_node :: patches_node
	in
	Logs.debug (fun m -> m "New KDL doc:@;%a@." KDL.pp doc);
	manifest := Some doc

(* TODO: unify with `read`, but requries Working_directory to be set, which
	isn’t yet *)
let list_names ~fs (section : [< `Inputs | `Patches]) : (string list, Error.t) result =
	let parent =
		match section with
		| `Inputs -> "inputs"
		| `Patches -> "patches"
	in
	let filepath = Eio.Path.(fs / filename) in
	let manifest_content =
		Eio.Path.with_open_in ~follow: false filepath @@ fun flow ->
		let buf = Eio.Buf_read.of_flow flow ~max_size: max_int in
		Eio.Buf_read.take_all buf
	in
	match manifest_content with
	| content ->
		begin
			match KDL.of_string content with
			| Ok doc ->
				begin
					match KDL.L.(doc.@(node parent // children)) with
					| Ok children ->
						Ok (List.map (fun ({name; _}: KDL.node) -> name) children)
					| Error _ -> Ok []
				end
			| Error err ->
				Error (`Manifest (`Parsing [`Parse_error err]))
		end
	| exception exn ->
		Error (`Manifest (`Read_error (Printexc.to_string exn)))

let exists () : bool =
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	Eio.Path.is_file filepath

let read () =
	let (let*) = Result.bind in
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	Logs.info (fun m -> m "Reading manifest @@ %a …" Eio.Path.pp filepath);
	let* kdl =
		Eio.Path.with_open_in ~follow: false filepath @@ fun flow ->
		KDL.of_flow flow
	in
	let () = manifest := Some kdl in
	Ok kdl

let write () : (unit, error) result =
	let (let*) = Result.bind in
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	let* mnfst =
		match !manifest with
		| Some m -> Ok m
		| None -> Error `Not_set_up
	in
	Logs.info (fun m -> m "Writing manifest @@ %s …" filename);
	let result =
		Eio.Path.with_open_out ~follow: false ~create: (`Exclusive 0o644) filepath @@ fun flow ->
		let banner = [
			Cstruct.of_string "// ┏┓╻+╻ ╱┏┳┓┏┓┏┳┓┏┓╻\n";
			Cstruct.of_string "// ┃┃┃┃┗━┓╹┃╹┣┫┃┃┃┣┫┃   Read the manpage:\n";
			Cstruct.of_string "// ╹┗┛╹╱ ╹ ╹ ╹╹╹ ╹╹╹┗┛  $ man nixtamal-manifest\n";
		]
		in
		Eio.Flow.write flow banner;
		KDL.to_flow flow mnfst;
		Eio.Flow.write flow ([Cstruct.of_string "\n"])
	in
	Logs.info (fun m -> m "Manifest written.");
	Ok result

let upgrade (content : string) : (string, error) result =
	let all_lines = String.split_on_char '\n' content in
	let rec find_index acc = function
		| [] ->
			None
		| line :: rest ->
			let trimmed = String.trim line in
			if String.starts_with ~prefix: "version" trimmed then
				Some (List.rev acc)
			else
				find_index (line :: acc) rest
	in
	match find_index [] all_lines with
	| None ->
		Error `Version_not_found
	| Some before ->
		let after = List.tl (List.drop (List.length before) all_lines)
		and new_line = Fmt.str {|version "%s"|} (Schema.Version.to_string Lazy.(force Schema.Version.latest))
		in
		Ok (String.concat "\n" (before @ new_line :: after))
