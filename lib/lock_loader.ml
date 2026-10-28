(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
(* Loads the lockfile for Nix usage broadly *)
let filename = "default.nix"

let render_jingoo_template ?(version = Lazy.(force Schema.Version.latest)) ?features: features_opt () : string =
	let year_range (first : int) : string =
		(* replaced by Nix *)
		match int_of_string_opt "@release_year@" with
		| Some last when last > first -> Fmt.str "%a–%a" Fmt.int first Fmt.int last
		| _ -> Fmt.str "%a" Fmt.int first
	in
	let features =
		match features_opt with
		| Some feats -> feats
		| None ->
			let feats =
				Foreman.inputs
				|> Saturn.Htbl.to_seq
				|> Seq.fold_left
						(fun acc (_name, input) ->
							Features.add_input input acc
						)
						Features.empty
			in
			let feats =
				match Saturn.Htbl.to_seq Foreman.patches |> Seq.uncons with
				| None -> feats
				| Some _ -> Features.(add Flag.patches feats)
			in
			feats
	in
	let jg_models2 : (string, Jingoo.Jg_types.tvalue) Hashtbl.t =
		let open Jingoo.Jg_types in
		let hash_tokens =
			let open Input.Hash in
			let hts = ref [] in
			for idx =
			max_algorithm downto min_algorithm do
				match algorithm_of_enum idx with
				| Some algo ->
					let hash_token = string_of_int idx, Tstr (algorithm_to_hash_token algo) in
					hts := hash_token :: !hts
				| None -> ()
			done;
			Tobj !hts
		in
		let local_patches =
			match Foreman.local_patches () with
			| [] -> Tnull
			| patches ->
				let to_pair (name, path) = (Name.Patch.take name, Tstr path) in
				Tobj (List.map to_pair patches)
		in
		let builtin_fetchers, fetchers =
			let btbl = Hashtbl.create 3
			and ftbl = Hashtbl.create 6
			in
			if Features.(has Flag.file features) then
				begin
					Hashtbl.add btbl "file" (Tint 0);
					Hashtbl.add ftbl "file" (Tint 0);
				end;
			if Features.(has Flag.archive features) then
				begin
					Hashtbl.add btbl "archive" (Tint 1);
					Hashtbl.add ftbl "archive" (Tint 1);
				end;
			if Features.(has Flag.git features) then
				begin
					Hashtbl.add btbl "git" (Tint 2);
					Hashtbl.add ftbl "git" (Tint 2);
				end;
			if Features.(has Flag.darcs features) then
				Hashtbl.add ftbl "darcs" (Tint 3);
			if Features.(has Flag.pijul features) then
				Hashtbl.add ftbl "pijul" (Tint 4);
			if Features.(has Flag.fossil features) then
				Hashtbl.add ftbl "fossil" (Tint 5);
			Thash btbl, Thash ftbl
		in
		let htbl = Hashtbl.create 8 in
		Hashtbl.add htbl "maker_year_range" (Tstr (year_range 2025));
		Hashtbl.add htbl "hackmates_year_range" (Tstr (year_range 2026));
		Hashtbl.add htbl "schema_version" (Tstr (Schema.Version.to_string version));
		Hashtbl.add htbl "using_patches" (Tbool Features.(has Flag.patches features));
		Hashtbl.add htbl "hash_tokens" hash_tokens;
		Hashtbl.add htbl "local_patches" local_patches;
		Hashtbl.add htbl "builtin_fetchers" builtin_fetchers;
		Hashtbl.add htbl "fetchers" fetchers;
		htbl
	in
	let models needle : Jingoo.Jg_types.tvalue =
		match Hashtbl.find_opt jg_models2 needle with
		| Some value -> value
		| None -> Tnull
	and jg_tpl : string = [%blob "lock_loader.jinja"]
	in
	Jingoo.Jg_template2.from_string ~models jg_tpl

let write ?(version = Lazy.(force Schema.Version.latest)) () =
	Logs.info (fun m -> m "Writing lock loader @@ %s …" filename);
	let working_dir = Working_directory.get () in
	let filepath = Eio.Path.(working_dir / filename) in
	let () =
		Eio.Path.with_open_out ~follow: false ~create: (`Or_truncate 0o644) filepath @@ fun flow ->
		Eio.Flow.copy_string (render_jingoo_template ~version ()) flow
	in
	Logs.info (fun m -> m "Lock loader written.")
