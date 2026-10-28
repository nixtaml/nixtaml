(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2026 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
(* DOMAIN STATE ***************************************************************)

type input_phase_info = {
	kind: Input.Kind.t;
	phase: Phase.phase_name;
}

type patch_phase_info = {
	phase: Phase.phase_name;
}

type item =
	| Input of Name.Input.t
	| Patch of Name.Patch.t

(* This structure owns a small, fixed region at the bottom of stdout.
	`lines` tracks how many *physical terminal rows* we currently own.
	`has_rendered` tracks if we’ve rendered at least once (for clearing initial
	empty state). *)
type t = {
	inputs: (Name.Input.t, input_phase_info) Hashtbl.t;
	patches: (Name.Patch.t, patch_phase_info) Hashtbl.t;
	mutable lines: int;
	mutable has_rendered: bool;
	mutex: Eio.Mutex.t;
}

(* Set up the TUI with all items in Biding state.
	This ensures all inputs+patches are visible from the start,
	regardless of when the Biding callback is invoked. *)
let set_up_items (t : t) (inputs : Name.Input.t list) (patches : Name.Patch.t list) : unit =
	Eio.Mutex.lock t.mutex;
	Fun.protect
		~finally: (fun () -> Eio.Mutex.unlock t.mutex)
		(fun () ->
			List.iter
				(fun name ->
					let kind =
						match Foreman.get name with
						| Ok input -> input.kind
						| Error _ ->
							`File (
								Input.File.make
									~url: (Input.Template.make "")
									()
							)
					in
					Hashtbl.replace t.inputs name {kind; phase = `Biding}
				)
				inputs;
			List.iter
				(fun name -> Hashtbl.replace t.patches name {phase = `Biding})
				patches
		)

let make ~(input_count : int) ~(patch_count : int) () : t = {
	inputs = Hashtbl.create input_count;
	patches = Hashtbl.create patch_count;
	lines = 0;
	has_rendered = false;
	mutex = Eio.Mutex.create ();
}

(* TERMINAL HELPERS ***********************************************************)

let esc = "\027["

let move_up n =
	if n > 0 then
		Fmt.pr "%s%dA" esc n

(* Clear exactly [n] rows starting at the current cursor position.
	This assumes we are positioned at the *top* of the owned region. *)
let clear_lines n =
	for _i = 1 to n do
		(* clear line, move to next *)
		Fmt.pr "\r%s2K%sB" esc esc
	done;
	(* move cursor back to where we started *)
	move_up n

(* Count physical rows by counting newlines.
	This is the only reliable accounting method. *)
let count_lines (s : string) : int =
	String.fold_left
		(fun acc c -> if c = '\n' then acc + 1 else acc)
		0
		s

(* RENDERING ******************************************************************)

let render_state (t : t) : string =
	let buf = Buffer.create 256
	and input_entries =
		Hashtbl.to_seq t.inputs
		|> List.of_seq
		|> List.sort (fun (n1, _) (n2, _) -> Name.Input.compare n1 n2)
	and patch_entries =
		Hashtbl.to_seq t.patches
		|> List.of_seq
		|> List.sort (fun (n1, _) (n2, _) -> Name.Patch.compare n1 n2)
	in

	(* Inputs block *)
	if not (List.is_empty input_entries) then
		begin
			Buffer.add_string buf "INPUTS\n";
			List.iter
				(fun ((name, info): Name.Input.t * input_phase_info) ->
					let symbol = Phase.phase_to_symbol info.phase
					and label = Phase.phase_to_label info.phase
					and kind_str = Input.Kind.to_string info.kind
					in
					(* Calculate available width for name using display widths, not byte lengths *)
					let symbol_width = UTF8.width symbol
					and kind_width = UTF8.width kind_str
					in
					(* Account for: symbol + " " + name + " (" + kind + ")" *)
					let max_name_display_width = 35 - symbol_width - 1 - kind_width - 3 in
					let name_truncated = UTF8.truncate_to_width (Name.Input.take name) max_name_display_width in
					let left : string =
						Fmt.str "%s %s (%s)" symbol name_truncated kind_str
					and right : string =
						label
					in
					(* Manual padding based on display width, not byte length *)
					let left_width = UTF8.width left in
					let padding = max 0 (35 - left_width) in
					Buffer.add_string buf left;
					Buffer.add_string buf (String.make padding ' ');
					Buffer.add_string buf (Fmt.str " %12s\n" right)
				)
				input_entries;
			Buffer.add_char buf '\n'
		end;

	(* Patches block *)
	if not (List.is_empty patch_entries) then
		begin
			Buffer.add_string buf "PATCHES\n";
			List.iter
				(fun ((name, info): Name.Patch.t * patch_phase_info) ->
					let symbol = Phase.phase_to_symbol info.phase
					and label = Phase.phase_to_label info.phase
					in
					(* Calculate available width for name using display widths *)
					let symbol_width = UTF8.width symbol in
					let max_name_display_width = 35 - symbol_width - 1 in
					let name_truncated = UTF8.truncate_to_width (Name.Patch.take name) max_name_display_width in
					let left : string =
						Fmt.str "%s %s" symbol name_truncated
					and right : string =
						label
					in
					(* Manual padding based on display width *)
					let left_width = UTF8.width left in
					let padding = max 0 (35 - left_width) in
					Buffer.add_string buf left;
					Buffer.add_string buf (String.make padding ' ');
					Buffer.add_string buf (Fmt.str " %12s\n" right)
				)
				patch_entries
		end;
	Buffer.contents buf

(* ENTRYPOINT *****************************************************************)

let on_phase (t : t) (item : item) (phase : Phase.phase_name) : unit =
	Eio.Mutex.lock t.mutex;
	Fun.protect
		~finally: (fun () -> Eio.Mutex.unlock t.mutex)
		(fun () ->
			(* Update internal state *)
			begin
				match item with
				| Input name ->
					let kind =
						match Foreman.get name with
						| Ok input -> input.kind
						| Error _ ->
							`File (
								Input.File.make
									~url: (Input.Template.make "")
									()
							)
					in
					Hashtbl.replace t.inputs name {kind; phase}
				| Patch name ->
					Hashtbl.replace t.patches name {phase}
			end;

			(* Render new frame *)
			let rendered = render_state t in
			let new_lines = count_lines rendered in

			(* Erase previous frame only if we’ve rendered before *)
			if t.has_rendered && t.lines > 0 then
				begin
					move_up t.lines;
					clear_lines t.lines
				end;

			(* Draw new frame *)
			Fmt.pr "%s%!" rendered;

			(* Remember exactly how many rows we now own and that we’ve rendered *)
			t.lines <- new_lines;
			t.has_rendered <- true
		)
