(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
type done_via = [
	| `Lacked_fresh_cmd
	| `Already_fresh
	| `Prefetched
	| `Patched
	| `Stale_patch
	| `Siloed
	| `Freeze
	| `Local_patch
	| `Promoted
	| `Errored of Error.foreman_error
]
[@@deriving show]

type _ input_phase =
	| Biding : [> `Biding] input_phase
	| Checking : [> `Checking] input_phase
	| Prefetching : [> `Prefetching] input_phase
	| Patching : [> `Patching] input_phase
	| Done : done_via -> [> `Done] input_phase

type _ patch_phase =
	| Biding : [> `Biding] patch_phase
	| Prefetching : [> `Prefetching] patch_phase
	| Done : done_via -> [> `Done] patch_phase

type phase_name = [
	| `Biding
	| `Checking
	| `Prefetching
	| `Patching
	| `Done of done_via
]
[@@deriving show]

type on_phase_callback = Name.Input.t -> phase_name -> unit

let phase_to_label : phase_name -> string = function
	| `Biding -> "Biding"
	| `Checking -> "Checking"
	| `Prefetching -> "Prefetching"
	| `Patching -> "Patching"
	| `Done `Prefetched -> "Prefetched"
	| `Done `Patched -> "Patched"
	| `Done `Stale_patch -> "Stale patch"
	| `Done `Siloed -> "Siloed"
	| `Done `Freeze -> "Frozen"
	| `Done `Local_patch -> "Local patch"
	| `Done `Already_fresh -> "Fresh"
	| `Done `Lacked_fresh_cmd -> "Lacked"
	| `Done `Promoted -> "Promoted"
	| `Done (`Errored _) -> "Errored"

let phase_to_symbol : phase_name -> string = function
	| `Biding ->
		"◌"
	| `Checking ->
		"△"
	| `Prefetching ->
		"↯"
	| `Patching ->
		"±"
	| `Done `Local_patch
	| `Done `Prefetched
	| `Done `Patched
	| `Done `Siloed
	| `Done `Already_fresh
	| `Done `Lacked_fresh_cmd
	| `Done `Promoted ->
		"✓"
	| `Done `Freeze ->
		"❆"
	| `Done `Stale_patch ->
		"⊖"
	| `Done (`Errored _) ->
		"✗"

(* INPUT PHASES ***************************************************************)

let to_checking_input
		?on_phase
		(_phase : [> `Biding] input_phase)
		(name : Name.Input.t)
		: ([> `Checking] input_phase, _) result
	=
	let checking : [> `Checking] input_phase = Checking in
	Option.iter (fun f -> f name `Checking) on_phase;
	Ok checking

let to_prefetching_input
		?on_phase
		(_phase : [> `Checking] input_phase)
		(name : Name.Input.t)
		: ([> `Prefetching] input_phase, _) result
	=
	let prefetching : [> `Prefetching] input_phase = Prefetching in
	Option.iter (fun f -> f name `Prefetching) on_phase;
	Ok prefetching

let to_patching_input
		?on_phase
		(_phase : [> `Prefetching] input_phase)
		(name : Name.Input.t)
		: ([> `Patching] input_phase, _) result
	=
	let patching : [> `Patching] input_phase = Patching in
	Option.iter (fun f -> f name `Patching) on_phase;
	Ok patching

let to_done_input
		?on_phase
		(_phase : [> `Prefetching | `Patching] input_phase)
		(name : Name.Input.t)
		(via : done_via)
		: [> `Done] input_phase
	=
	let done_phase : [> `Done] input_phase = Done via in
	Option.iter (fun f -> f name (`Done via)) on_phase;
	done_phase

(* PATCH PHASES ***************************************************************)

let to_prefetching_patch
		?on_phase
		(_phase : [> `Biding] patch_phase)
		(name : Name.Patch.t)
		: ([> `Prefetching] patch_phase, _) result
	=
	let prefetching_p : [> `Prefetching] patch_phase = (Prefetching : [> `Prefetching] patch_phase) in
	Option.iter (fun f -> f name `Prefetching) on_phase;
	Ok prefetching_p

let to_done_patch
		?on_phase
		(_phase : [> `Prefetching] patch_phase)
		(name : Name.Patch.t)
		(via : done_via)
		: [> `Done] patch_phase
	=
	let done_phase_p : [> `Done] patch_phase = (Done via: [> `Done] patch_phase) in
	Option.iter (fun f -> f name (`Done via)) on_phase;
	done_phase_p
