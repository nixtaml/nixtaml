(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
type vcs =
	| Git
	| Mercurial
	| Darcs
	| Pijul
	| Jujutsu
	| Fossil
[@@deriving show, enum]

type mode =
	| Auto
	| Forced of vcs
	| None_
[@@deriving show]

let markers = function
	| Git -> [".git"]
	| Darcs -> ["_darcs"]
	| Pijul -> [".pijul"]
	| Mercurial -> [".hg"]
	| Jujutsu -> [".jj"]
	| Fossil -> ["_FOSSIL_"; ".fslckout"]

let directory vcs = List.hd (markers vcs)

type ignore_style = Glob | Regex

type ignore_pattern = {
	glob: string;
	regex: string;
}

let get_VCS_style (vcs : vcs) : (ignore_style * string) option =
	match vcs with
	| Git | Jujutsu -> Some (Glob, ".gitignore")
	| Darcs -> Some (Regex, ".boring")
	| Pijul -> Some (Glob, ".ignore")
	| Mercurial -> Some (Glob, ".hgignore")
	| Fossil -> None

let exists path : bool =
	match Eio.Path.kind ~follow: true path with
	| `Directory | `Regular_file -> true
	| _ -> false

let detect cwd : vcs option =
	let rec loop (idx : int) : vcs option =
		if idx <= max_vcs then
			match vcs_of_enum idx with
			| Some cur_vcs ->
				let rec check_markers = function
					| [] -> loop (idx + 1)
					| m :: ms ->
						if exists (Eio.Path.(cwd / m)) then Some cur_vcs
						else check_markers ms
				in
				check_markers (markers cur_vcs)
			| None ->
				loop (idx + 1)
		else
			None
	in
	loop min_vcs

module Identity = struct
	type t = {
		name: string option;
		contact: string option;
	}
	[@@deriving show]

	let parse_name_and_bracked_contact s =
		match String.index_opt s '<', String.rindex_opt s '>' with
		| Some lt, Some gt when lt < gt ->
			let name = String.trim (String.sub s 0 lt)
			and email = String.sub s (lt + 1) (gt - lt - 1)
			in
			Some {name = Some name; contact = Some email}
		| _ -> None

	let first_non_blank_line s =
		let lines = String.split_on_char '\n' s in
		let rec go = function
			| [] -> None
			| line :: rest ->
				let trimmed = String.trim line in
				if trimmed = "" then go rest
				else Some trimmed
		in
		go lines

	let parse_show_authors_line s =
		match first_non_blank_line s with
		| Some line ->
			let parts = String.split_on_char '\t' line in
			(
				match parts with
				| rank :: _ :: rest when String.length rank > 0 && rank.[0] = '#' ->
					Some (String.concat "\t" rest)
				| _ -> Some line
			)
		| None ->
			None

	let get_Git ~env : t option =
		let proc_mgr = Eio.Stdenv.process_mgr env in
		let buf = Buffer.create 128 in
		match Eio.Process.run proc_mgr ~stdout: (Eio.Flow.buffer_sink buf) ["git"; "var"; "GIT_AUTHOR_IDENT"] with
		| _ ->
			let s = String.trim (Buffer.contents buf) in
			parse_name_and_bracked_contact s
		| exception _ -> None

	let get_Darcs ~env : t option =
		let cwd = Eio.Stdenv.cwd env in
		let fs = Eio.Stdenv.fs env in
		let proc_mgr' = Eio.Stdenv.process_mgr env in
		let home_dir =
			match Eio.Process.getenv_opt proc_mgr' "HOME" with
			| Some d -> d
			| None ->
				let base = Xdg.create ~env: (Eio.Process.getenv_opt proc_mgr') () in
				Xdg.home_dir base
		and try_env (var : string) : t option =
			match Eio.Process.getenv_opt proc_mgr' var with
			| Some s ->
				let trimmed = String.trim s in
				if trimmed = "" then None
				else parse_name_and_bracked_contact trimmed
			| None -> None
		and try_file (path : _ Eio.Path.t) : t option =
			try
				let content = Eio.Path.load ~follow: false path in
				match first_non_blank_line content with
				| Some line -> parse_name_and_bracked_contact line
				| None -> None
			with
				| _ -> None
		in
		let try_show_authors () : t option =
			let proc_mgr = Eio.Stdenv.process_mgr env in
			let buf = Buffer.create 64 in
			match Eio.Process.run proc_mgr ~stdout: (Eio.Flow.buffer_sink buf) ["darcs"; "show"; "authors"] with
			| _ ->
				match parse_show_authors_line (Buffer.contents buf) with
				| Some str -> parse_name_and_bracked_contact str
				| None -> None
				| exception _ -> None
		in
		let env1 = try_env "DARCS_EMAIL" in
		match env1 with
		| Some v -> Some v
		| None ->
			let env2 = try_env "EMAIL" in
			match env2 with
			| Some v -> Some v
			| None ->
				let repo1 = try_file (Eio.Path.(cwd / "_darcs" / "prefs" / "author")) in
				match repo1 with
				| Some v -> Some v
				| None ->
					let repo2 = try_file (Eio.Path.(cwd / "_darcs" / "prefs" / "authors")) in
					match repo2 with
					| Some v -> Some v
					| None ->
						let user = try_file (Eio.Path.(fs / home_dir / ".darcs" / "author")) in
						match user with
						| Some v -> Some v
						| None ->
							try_show_authors ()

	let get_Jujutsu ~env : t option =
		let proc_mgr = Eio.Stdenv.process_mgr env in
		let name : string option =
			let buf = Buffer.create 64 in
			match Eio.Process.run proc_mgr ~stdout: (Eio.Flow.buffer_sink buf) ["jj"; "config"; "get"; "user.name"] with
			| _ ->
				match String.trim (Buffer.contents buf) with
				| "" -> None
				| s -> Some s
				| exception _ -> None
		in
		let email : string option =
			let buf = Buffer.create 64 in
			match Eio.Process.run proc_mgr ~stdout: (Eio.Flow.buffer_sink buf) ["jj"; "config"; "get"; "user.email"] with
			| _ ->
				match String.trim (Buffer.contents buf) with
				| "" -> None
				| s -> Some s
				| exception _ -> None
		in
		Some {name; contact = email}

	let get ~env : t option =
		let from_vcs =
			match detect (Eio.Stdenv.cwd env) with
			| Some Git -> get_Git ~env
			| Some Darcs -> get_Darcs ~env
			| Some Jujutsu -> get_Jujutsu ~env
			| _ -> None
		in
		let proc_mgr = Eio.Stdenv.process_mgr env in
		let name =
			match from_vcs with
			| Some {name = Some n; _} -> Some n
			| _ -> Eio.Process.getenv_opt proc_mgr "USER"
		in
		let contact =
			match from_vcs with
			| Some {contact = Some c; _} -> Some c
			| _ -> Eio.Process.getenv_opt proc_mgr "EMAIL"
		in
		match name, contact with
		| None, None -> None
		| n, c -> Some {name = n; contact = c}
end

let append_to_ignore_content ~(lines_to_add : string list) (content : string) : string option =
	let section_marker = "# Nixtamal" in
	let section_len = String.length section_marker in

	(* Find existing "# Nixtamal" section by scanning byte positions.
		Returns (start_pos, end_pos) where end is the blank line after section
		content. Section ends at double-newline or end of file. *)
	let rec find_section (line_idx : int) : (int * int) option =
		if line_idx + section_len > String.length content then
			None
		else if String.sub content line_idx section_len = section_marker then
			let rec find_section_end (search_idx : int) : int =
				if search_idx >= String.length content then
					String.length content
				else if content.[search_idx] = '\n' then
					(* Double newline marks end of section *)
					if search_idx + 1 < String.length content && content.[search_idx + 1] = '\n' then
						search_idx + 1
					else
						find_section_end (search_idx + 1)
				else find_section_end (search_idx + 1)
			in
			Some (line_idx, find_section_end (line_idx + section_len))
		else find_section (line_idx + 1)
	in

	(* Check if string needle exists in the string haystack *)
	let contains_str (needle : string) (haystack : string) : bool =
		let n = String.length needle
		and h = String.length haystack
		in
		let rec go i = i + n <= h && (String.sub haystack i n = needle || go (i + 1)) in
		go 0
	in

	(* Filter out lines already present (duplicates) & lines containing newlines *)
	let lines_to_write =
		List.filter (fun l -> not (String.exists (fun c -> c = '\n') l) && not (contains_str l content)) lines_to_add
	in
	if List.is_empty lines_to_write then
		None
	else
		match find_section 0 with
		| Some (section_start, section_end) ->
			(* Section found: splice new lines into existing section.
				[before][existing_section][new_lines][after] *)
			let new_size =
				String.length content + List.fold_left (fun acc l -> acc + String.length l + 1) 0 lines_to_write
			in
			let b = Buffer.create new_size in
			Buffer.add_substring b content 0 section_start;
			Buffer.add_substring b content section_start (section_end - section_start);
			List.iter (fun l -> Buffer.add_string b l; Buffer.add_char b '\n') lines_to_write;
			if section_end < String.length content then
				Buffer.add_substring b content section_end (String.length content - section_end);
			Some (Buffer.contents b)
		| None ->
			(* No existing section: append new section at end *)
			let prefix = if content = "" then "" else "\n" in
			Some (content ^ prefix ^ "# Nixtamal\n" ^ (String.concat "\n" lines_to_add) ^ "\n")

let append_unique_line_with_comment ~file ~lines_to_add =
	let file_exists =
		match Eio.Path.kind ~follow: true file with
		| `Not_found -> false
		| _ -> true
	in
	if not file_exists then
		let content = "# Nixtamal\n" ^ (String.concat "\n" lines_to_add) ^ "\n" in
		Eio.Path.save ~follow: false ~create: (`Or_truncate 0o644) file content
	else
		Eio.Path.with_open_in ~follow: false file @@ fun input ->
		let buf = Eio.Buf_read.of_flow input in
		let content = Eio.Buf_read.take_all (buf ~max_size: max_int) in
		match append_to_ignore_content ~lines_to_add content with
		| None -> ()
		| Some new_content -> Eio.Path.save ~follow: false ~create: (`Or_truncate 0o644) file new_content

let rel_path ~base ~target =
	let base = Eio.Path.native_exn base
	and target = Eio.Path.native_exn target
	in
	let base =
		if String.ends_with ~suffix: "/" base then base else base ^ "/"
	in
	if String.starts_with ~prefix: base target then
		String.sub
			target
			(String.length base)
			(String.length target - String.length base)
	else
		failwith "target path is not under base"

let set_up_ignore ~env ~(mode : mode) =
	let cwd = Eio.Stdenv.cwd env
	and working_dir = Working_directory.get ()
	in

	(* Compute relative paths *)
	let silo_rel =
		rel_path
			~base: cwd
			~target: Eio.Path.(working_dir / Working_directory.silo_dir)
	and target_VCS =
		match mode with
		| Auto -> detect cwd
		| Forced vcs -> Some vcs
		| None_ -> None
	in
	Option.bind target_VCS get_VCS_style
	|> Option.iter (fun (style, filename) ->
			(* Build ignore entries with both glob & regex *)
			let ignores =
				let silo =
					let glob = silo_rel
					and regex =
						String.split_on_char '.' silo_rel
						|> String.concat "\\."
						|> Fmt.str "(^|/)%s($|/)"
					in
						{glob; regex}
				in
				[
					silo;
				]
			in

			(* Map to appropriate style *)
			let lines_to_add =
				List.map (fun p -> match style with Glob -> p.glob | Regex -> p.regex) ignores
			in
			append_unique_line_with_comment
				~file: Eio.Path.(cwd / filename)
				~lines_to_add
		)
