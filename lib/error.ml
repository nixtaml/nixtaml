(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
module Non_empty_list = Util.Non_empty_list

type input_kind =
	| File
	| Archive
	| Git
	| Darcs
	| Pijul
	| Fossil
[@@deriving show, enum]

let print_input_kind = function
	| File -> "File"
	| Archive -> "Archive"
	| Git -> "Git"
	| Darcs -> "Darcs"
	| Pijul -> "Pijul"
	| Fossil -> "Fossil"

let pp_exn ppf exn =
	Fmt.pf ppf "%s" (Printexc.to_string exn)

type manifest_error = [
	| `Not_set_up
	| `Parsing of KDL.Valid.err list
	| `Read_error of string
	| `Version_not_found
	| `Version_invalid
]
[@@deriving show]

type lockfile_error = [
	| `Not_set_up
	| `Parsing of string
	| `Serializing of string
	| `Version_not_found
	| `Upgrade of string
]
[@@deriving show]

type meld_error = [
	| `Input_kind_mismatch of Name.Input.t * input_kind * input_kind
]
[@@deriving show]

type prefetch_method = [
	| `URL
	| `Git
	| `Darcs
	| `Pijul
	| `Fossil
]
[@@deriving show]

type fresh_error = [
	| `Cmd_exception of exn * string
	| `Cmd_output_empty
]
[@@deriving show]

type prefetch_error = [
	| `Run_exception of prefetch_method * exn * string
	| `Empty_output of prefetch_method
	| `Bad_output of prefetch_method * string
	| `JSON_parsing of prefetch_method * string
	| `Algorithm_mismatch of prefetch_method * Input.Hash.algorithm * Input.Hash.algorithm
	| `Darcs_context of string
	| `Vendored_patch of string
	| `Invalid_store_path of string * string
]
[@@deriving show]

type upgrade_error = [
	| `Manifest of manifest_error
	| `Lockfile of lockfile_error
	| `Could_not_verify of [`Manifest | `Lockfile]
	| `Cleanup of [`Manifest | `Lockfile] * exn
	| `Rollback of [`Manifest | `Lockfile] * exn
	| `Run_exception of exn
]
[@@deriving show]

let [@inline]tag_upgrade_error (res : ('a, upgrade_error) result) =
	Result.map_error (fun err -> `Upgrade err) res

type hashtbl_op = [
	| `Add
	| `Drop
	| `Get
	| `Set
]
[@@deriving show]

type silo_error = [
	| `Silo_not_directory of string
	| `GC_exception of exn
]
[@@deriving show]

type foreman_error = [
	| `Pool_exception of exn
	| `Model_op of [`Input of Name.Input.t | `Patch of Name.Patch.t] * hashtbl_op
	| `Fresh of Name.Input.t * fresh_error
	| `Prefetch of [`Input of Name.Input.t | `Patch of Name.Patch.t] * prefetch_error
	| `Patch_check of Name.Input.t * Patch_check.error
	| `Patch_missing of Name.Input.t * Name.Patch.t list
	| `Lockfile of [`Input of Name.Input.t | `Patch of Name.Patch.t] * Locator.error Non_empty_list.t
	| `Silo of [`Input of Name.Input.t | `Patch of Name.Patch.t] * silo_error
	| `Many_errors of foreman_error list
]
[@@deriving show]

type blake3_error = [
	| `Feature_missing of string
	| `Feature_not_enabled of string
]
[@@deriving show]

type t = [
	| `Manifest of manifest_error
	| `Lockfile of lockfile_error
	| `Meld of meld_error
	| `Upgrade of upgrade_error
	| `Blueprint of exn
	| `Version_mismatch of Schema.Version.t * Schema.Version.t
	| `Version_not_latest
	| `Foreman of foreman_error
	| `Silo of silo_error
	| `BLAKE3_not_supported of blake3_error
	| `Darcs_context of string
	| `Vendored_patch of string
]

let pp_meld_error ppf = function
	| `Input_kind_mismatch (name, manifest_kind, lockfile_kind) ->
		Fmt.pf
			ppf
			"Input kind mismatch for %a (manifest: %s, lockfile: %s). If you purposefully switched your input’s kind in the manifest, you can force locking the input with `nixtamal lock --force %s`."
			Name.Input.pp
			name
			(print_input_kind manifest_kind)
			(print_input_kind lockfile_kind)
			(Name.Input.take name)

let pp_blake3_error ppf = function
	| `Feature_missing nix_version ->
		Fmt.pf
			ppf
			"Your Nix version «%s» doesn’t include the `blake3-hashes` experimental feature. Upgrade to Nix 2.27 or later."
			nix_version
	| `Feature_not_enabled nix_version ->
		Fmt.pf
			ppf
			"Your Nix version «%s» supports the `blake3-hashes` experimental feature, but it is not enabled. Add `blake3-hashes` to experimental-features in your nix.conf (such as ~/.config/nix/nix.conf or /etc/nix/nix.conf)."
			nix_version

let pp ppf = function
	| `Manifest m -> Fmt.pf ppf "Manifest: %a" pp_manifest_error m
	| `Lockfile l -> Fmt.pf ppf "Lockfile: %a" pp_lockfile_error l
	| `Meld m -> Fmt.pf ppf "Meld: %a" pp_meld_error m
	| `Upgrade u -> Fmt.pf ppf "Upgrade: %a" pp_upgrade_error u
	| `Version_mismatch (mv, lv) ->
		Fmt.pf ppf "Manifest version (%a) does not match lockfile version (%a)" Schema.Version.pp mv Schema.Version.pp lv
	| `Blueprint e -> Fmt.pf ppf "Blueprint scaffolding: %a" pp_exn e
	| `Foreman (`Patch_check (name, err)) ->
		Fmt.pf ppf "Patch check failed for input %a: %a" Name.Input.pp name Patch_check.pp_error err
	| `Foreman (`Patch_missing (name, missing)) ->
		Fmt.pf
			ppf
			"Input %a references patches that are not defined in the manifest: %a. Define them or drop them from the input’s patches list."
			Name.Input.pp
			name
			Fmt.(list ~sep: comma Name.Patch.pp)
			missing
	| `Foreman f -> Fmt.pf ppf "Foreman: %a" pp_foreman_error f
	| `Silo s -> Fmt.pf ppf "Silo: %a" pp_silo_error s
	| `Version_not_latest ->
		Fmt.pf ppf "Manifest ∨ lockfile is not using the latest schema version"
	| `BLAKE3_not_supported b3 ->
		Fmt.pf ppf "BLAKE3 hash algorithm: %a" pp_blake3_error b3
	| `Darcs_context msg ->
		Fmt.pf ppf "Darcs context error: %s" msg
	| `Vendored_patch msg ->
		Fmt.pf ppf "Vendored patch error: %s" msg

let [@inline]tag_manifest (res : ('a, manifest_error) result) =
	Result.map_error (fun err -> `Manifest err) res

let [@inline]tag_lockfile (res : ('a, lockfile_error) result) =
	Result.map_error (fun err -> `Lockfile err) res

let [@inline]tag_foreman res =
	Result.map_error (fun err -> `Foreman err) res

let [@inline]tag_meld (res : ('a, meld_error) result) =
	Result.map_error (fun err -> `Meld err) res

let [@inline]tag_silo (res : ('a, silo_error) result) =
	Result.map_error (fun err -> `Silo err) res

let [@inline]tag_upgrade (res : ('a, upgrade_error) result) =
	Result.map_error (fun err -> `Upgrade err) res

let [@inline]tag_blake3_error (res : ('a, blake3_error) result) =
	Result.map_error (fun err -> `BLAKE3_not_supported err) res
