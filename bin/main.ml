(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: GPL-3.0-or-later                                    │
└─────────────────────────────────────────────────────────────────────────────*)
let info =
	let top_level_man = [
		`S "LICENSE";
		`P "GNU General Public License, version 3.0 later (GPL-3.0-or-later)";
		`S "MAKER";
		`P "toastal <https://toast.al/contact/>";
		`S "FUNDING";
		`P "See: https://toast.al/funding/";
		`S "SEE ALSO";
		`I ("nixtamal-set-up(1)", "Set up working directory for Nixtamal");
		`I ("nixtamal-check-soundness(1)", "Check manifest × lockfile soundness");
		`I ("nixtamal-tweak(1)", "Tweak manifest with \\$EDITOR");
		`I ("nixtamal-show(1)", "Show current inputs");
		`I ("nixtamal-lock(1)", "Lock not-yet-locked inputs");
		`I ("nixtamal-list-stale(1)", "List stale inputs");
		`I ("nixtamal-refresh(1)", "Refresh non-frozen inputs");
		`I ("nixtamal-gc(1)", "Garbage collect old generations");
		`I ("nixtamal-upgrade(1)", "Upgrade lockfile & manifest schema");
		`I ("nixtamal-manifest(5)", "manifest.kdl format");
		`S "BUGS";
		`P "No tracker is set up; contact the maker directly or join the XMPP chatroom @ <xmpp:nixtamal@chat.toastal.in.th?join>.";
	]
	in
	Cmdliner.Cmd.info
		"nixtamal"
		~version: "@version@"
		~doc: "fulfilling input pinning for Nix"
		~exits: Error.exits
		~man: top_level_man

let cmd ~env =
	let subcommands = [
		Cmd.Set_up.cmd ~env;
		Cmd.Check_soundness.cmd ~env;
		Cmd.Tweak.cmd ~env;
		Cmd.Show.cmd ~env;
		Cmd.Lock.cmd ~env;
		Cmd.List_stale.cmd ~env;
		Cmd.Refresh.cmd ~env;
		Cmd.GC.cmd ~env;
		Cmd.Upgrade.cmd ~env;
	]
	in
	Cmdliner.Cmd.group info subcommands

let () =
	Eio_main.run @@ fun env ->
	(* if !Sys.interactive then () else *)
	exit @@ Cmdliner.Cmd.eval' (cmd ~env)
