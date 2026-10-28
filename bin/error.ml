(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
(*
	This is for the *user-facing* errors / codes
	Other values are considered “internal errors”
*)
type t =
	| Manifest_not_set_up [@value 65]
	| Version_mismatch
	| Version_not_latest
	| Manifest_parsing
	| Lockfile_parsing
	| Lockfile_serializing
	| Fresh_exception
	| Fresh_empty_output
	| Prefetch_exception
	| Prefetch_unusable_output
	| Prefetch_parsing
	| Darcs_context
	| Vendored_patch
	| Silo_error
	| Manifest_version_error
	| Upgrade_failed
	| BLAKE3_not_supported
	| Input_kind_mismatch
	| Blueprint_error
	| Patch_doesn't_apply
	| Patch_missing
[@@deriving enum]

let exit_doc : t -> string = function
	| Manifest_not_set_up -> "Nixtamal working directory or manifest not set up. Run `nixtamal set-up`."
	| Version_mismatch -> "Version mismatch between manifest and lockfile. Run `nixtamal lock`, but might require *removing* the lockfile first until `nixtaml fast-forward` is made."
	| Version_not_latest -> "Manifest ∨ lockfile is not using the latest schema version. Run `nixtamal upgrade` to upgrade."
	| Manifest_parsing -> "Failed to parse the manifest file."
	| Lockfile_parsing -> "Failed to parse the lockfile."
	| Lockfile_serializing -> "Failed to serialize lockfile."
	| Fresh_exception -> "Fresh command execution threw an exception."
	| Fresh_empty_output -> "Fresh command output nothing."
	| Prefetch_exception -> "Prefetch command threw an exception."
	| Prefetch_unusable_output -> "Prefetch command output was unusable."
	| Prefetch_parsing -> "Prefetch command flopped on JSON output."
	| Darcs_context -> "Darcs context file is missing or invalid. Try running `nixtamal lock` on the input."
	| Vendored_patch -> "Vendored patch file is missing. Try running `nixtamal lock` to fetch it."
	| Silo_error -> "Silo error (such as bad hex in generation number)."
	| Manifest_version_error -> "Version not found or invalid in manifest file."
	| Upgrade_failed -> "Upgrading Nixtamal failed."
	| BLAKE3_not_supported -> "BLAKE3 hash algorithm is requested but this Nix doesn’t support it."
	| Input_kind_mismatch -> "Input kind mismatch between manifest & lockfile which prevents values to be melded."
	| Blueprint_error -> "Blueprint scaffolding failed to write files."
	| Patch_doesn't_apply -> "A patch failed to apply."
	| Patch_missing -> "An input references patches that are not defined in the manifest."

let to_code : t -> Cmdliner.Cmd.Exit.code = to_enum

let exits : Cmdliner.Cmd.Exit.info list =
	let infos = ref Cmdliner.Cmd.Exit.defaults in
	(* decrementing to avoid List.rev *)
	for i =
	max downto min do
		match of_enum i with
		| Some err_variant ->
			let exit_info =
				Cmdliner.Cmd.Exit.info
					~doc: (exit_doc err_variant)
					i
			in
			infos := exit_info :: !infos
		| None -> ()
	done;
	!infos
