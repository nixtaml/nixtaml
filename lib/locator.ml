(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025–2026 toastal <https://toast.al/contact/>        │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
(*
	ocaml-uri (RFC 3986) was once used here, but VCS programs & nix-prefetch-*
	scripts accept raw strings for URLs — including SCP-style syntax
	(git@git.example.tld:user/repo) & local paths — which are not valid URIs,
	but are valid for our tools. Following RFC 3986 percent-encoding (meaning
	`@` → `%40`), our tool-valid URI-likes were transformed to valid URIs which
	tooling doesn't understand. As such, `Locator.t` is just an unboxed wrapper
	over a raw resource-address string (URI, SCP-style, local path, etc.) with
	only structural validation — no RFC conformance.

	MERGE NOTE (community fork): Security hardening from uRI.ml has been ported
	here — scheme whitelist & path traversal detection — while preserving the
	upstream Locator architecture that supports SCP-style & local paths.
*)
type t =
	Locator of UTF8.t
[@@unboxed]
[@@deriving eq, ord, show, qcheck]

type error = [
	| `Empty_locator
	| `Invalid_scheme of string
	| `Path_traversal of string
]
[@@deriving eq, show]

let pp_error ppf = function
	| `Empty_locator ->
		Fmt.pf ppf "Invalid locator: empty string"
	| `Invalid_scheme scheme ->
		Fmt.pf ppf "Invalid locator: unacceptable scheme %S" scheme
	| `Path_traversal path ->
		Fmt.pf ppf "Invalid locator: path traversal detected in %S" path

(* ── Security hardening (ported from community uRI.ml) ─────────────────────── *)

let acceptable_schemes = [
	"http"; "https"; "ftp"; "sftp"; "file"; "ssh";
	"git"; "darcs"; "pijul"; "fossil";
]

let is_acceptable_scheme scheme =
	List.mem (String.lowercase_ascii scheme) acceptable_schemes

(* URL-decode a string — handles %XX hex sequences *)
let url_decode s =
	let buf = Buffer.create (String.length s) in
	let i = ref 0 in
	while !i < String.length s do
		if s.[!i] = '%' && !i + 2 < String.length s then
			try
				let hex = String.sub s (!i + 1) 2 in
				let code = int_of_string ("0x" ^ hex) in
				Buffer.add_char buf (Char.chr code);
				i := !i + 3
			with _ ->
				Buffer.add_char buf s.[!i];
				incr i
		else
			Buffer.add_char buf s.[!i];
			incr i
	done;
	Buffer.contents buf

let contains_substring s substr =
	let len_s = String.length s in
	let len_sub = String.length substr in
	if len_sub = 0 then true
	else if len_sub > len_s then false
	else
		let rec loop i =
			if i > len_s - len_sub then false
			else if String.sub s i len_sub = substr then true
			else loop (i + 1)
		in
		loop 0

let has_path_traversal raw_path =
	let decoded_path = url_decode raw_path in
	let check_path path_str =
		contains_substring path_str ".." && (
			contains_substring path_str "/../" ||
			contains_substring path_str "\\..\\" ||
			String.starts_with ~prefix:"../" path_str ||
			String.ends_with ~suffix:"/.." path_str
		)
	in
	check_path raw_path || check_path decoded_path

(* Extract scheme from a locator string (before the first ':' or "://") *)
let extract_scheme str =
	match String.index_opt str ':' with
	| None -> None
	| Some i ->
		let scheme = String.sub str 0 i in
		(* SCP-style (git@host:path) has no scheme — the ':' is a path sep *)
		if String.contains scheme '@' then None
		else Some (String.lowercase_ascii scheme)

(* ── Core API ─────────────────────────────────────────────────────────────── *)

(* Full validation: structural + security *)
let parse = function
	| "" -> Error `Empty_locator
	| str ->
		if has_path_traversal str then
			Error (`Path_traversal str)
		else
			match extract_scheme str with
			| Some scheme when not (is_acceptable_scheme scheme) ->
				Error (`Invalid_scheme scheme)
			| _ -> Ok (Locator str)

(* WARNING: this is 'unsafe', better to use `parse` when dealing with user data *)
let [@inline]make u = Locator u
let [@inline]take (Locator u) = u

let is_valid u = Result.is_ok (parse u)

let jsont : t Jsont.t =
	let dec s =
		match parse s with
		| Ok loc -> loc
		| Error invalid ->
			Jsont.Error.msgf Jsont.Meta.none "%a" pp_error invalid
	in
	Jsont.string
	|> Jsont.map ~kind: "Locator" ~dec ~enc: take

let gen : t QCheck.Gen.t = QCheck.Gen.map make UTF8.gen
