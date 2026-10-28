# Nixtamal completions for fish
# IMPORTANT: keep in sync with bin/cmd.ml until Cmdliner supports fish

# subcommands
complete -c nixtamal -n __fish_use_subcommand -a set-up \
    -d 'Set up working directory for Nixtamal'
complete -c nixtamal -n __fish_use_subcommand -a check-soundness \
    -d 'Checks that the manifest × lockfile is sound'
complete -c nixtamal -n __fish_use_subcommand -a tweak \
    -d 'Tweak the manifest file with $VISUAL, $EDITOR, or vi'
complete -c nixtamal -n __fish_use_subcommand -a show \
    -d 'Shows current inputs as understood by Nixtamal for earthlings'
complete -c nixtamal -n __fish_use_subcommand -a lock \
    -d 'Lock all not-yet-locked inputs'
complete -c nixtamal -n __fish_use_subcommand -a list-stale \
    -d 'List stale inputs with fresh-cmd, without refreshing'
complete -c nixtamal -n __fish_use_subcommand -a refresh \
    -d 'Refreshes all non-frozen inputs using the fresh-cmd & then locks stale inputs'
complete -c nixtamal -n __fish_use_subcommand -a gc \
    -d 'Garbage collect old generations (experimental)'
complete -c nixtamal -n __fish_use_subcommand -a upgrade \
    -d 'Upgrade lockfile & manifest to current schema version'

# global flags (work with any subcommand)
complete -c nixtamal -f

complete -c nixtamal -l color -r -xa 'auto always never' \
    -d 'Colorize output'
complete -c nixtamal -s v -l verbose \
    -d 'Increase verbosity'
complete -c nixtamal -s q -l quiet \
    -d 'Decrease verbosity'
complete -c nixtamal -l directory -r -F \
    -d 'Working directory for Nixtamal-related files'
complete -c nixtamal -s j -l jobs -r \
    -d "Nixtamal's executor pool domain size"

# set-up
complete -c nixtamal -n '__fish_seen_subcommand_from set-up' \
    -l hash-algorithm -r -xa 'SHA-256 SHA-512 BLAKE3' \
    -d 'The default algorithm to be used for inputs & patches'
complete -c nixtamal -n '__fish_seen_subcommand_from set-up' \
    -l fetch-time -r -xa 'build eval' \
    -d 'The default fetch time to be used for inputs'
complete -c nixtamal -n '__fish_seen_subcommand_from set-up' \
    -l no-nixpkgs \
    -d 'Do not add Nixpkgs to the pinned inputs list by default'
complete -c nixtamal -n '__fish_seen_subcommand_from set-up' \
    -l use-channels \
    -d 'Use Nixpkgs channel archives (Zstandard newer & LZMA older) instead of Git'
complete -c nixtamal -n '__fish_seen_subcommand_from set-up' \
    -l use-mirrors \
    -d 'Use known TUNA mirrors for resilience'
complete -c nixtamal -n '__fish_seen_subcommand_from set-up' \
    -l nixpkgs-channel -r \
    -d 'Nixpkgs channel (e.g. nixpkgs-unstable)'
complete -c nixtamal -n '__fish_seen_subcommand_from set-up' \
    -l nixpkgs-ref -r \
    -d 'Nixpkgs Git ref (e.g. refs/heads/nixos-25.11)'
complete -c nixtamal -n '__fish_seen_subcommand_from set-up' \
    -l nixpkgs-revision -r \
    -d 'Nixpkgs Git revision'
complete -c nixtamal -n '__fish_seen_subcommand_from set-up' \
    -s b -l blueprint -r -xa 'release release-split' \
    -d 'Build a project structure based on a blueprint'
complete -c nixtamal -n '__fish_seen_subcommand_from set-up' \
    -l vcs -r -xa 'auto darcs git hg pijul jj none' \
    -d 'VCS for ignore file setup'

# check-soundness

# tweak

# show
complete -c nixtamal -n '__fish_seen_subcommand_from show' \
    -l input-names \
    -d 'List all input names from the manifest'
complete -c nixtamal -n '__fish_seen_subcommand_from show' \
    -l patch-names \
    -d 'List all patch names from the manifest'

# lock
complete -c nixtamal -n '__fish_seen_subcommand_from lock' \
    -s f -l force \
    -d 'Force input to lock'
complete -c nixtamal -n '__fish_seen_subcommand_from lock' \
    -xa '(nixtamal show --input-names 2>/dev/null)'
complete -c nixtamal -n '__fish_seen_subcommand_from lock' \
    -s p -l patch -r -xa '(nixtamal show --patch-names 2>/dev/null)'
complete -c nixtamal -n '__fish_seen_subcommand_from lock' \
    -l skip-patches \
    -d 'Skip processing all patches'
complete -c nixtamal -n '__fish_seen_subcommand_from lock' \
    -l no-tui \
    -d 'Disable the TUI & use Logs.app for status output'

# list-stale
complete -c nixtamal -n '__fish_seen_subcommand_from list-stale' \
    -l skip-patches \
    -d 'Skip processing all patches'

# refresh
complete -c nixtamal -n '__fish_seen_subcommand_from refresh' \
    -xa '(nixtamal show --input-names 2>/dev/null)'
complete -c nixtamal -n '__fish_seen_subcommand_from refresh' \
    -s p -l patch -r -xa '(nixtamal show --patch-names 2>/dev/null)'
complete -c nixtamal -n '__fish_seen_subcommand_from refresh' \
    -l skip-patches \
    -d 'Skip processing all patches'
complete -c nixtamal -n '__fish_seen_subcommand_from refresh' \
    -l no-tui \
    -d 'Disable the TUI & use Logs.app for status output'

# gc
complete -c nixtamal -n '__fish_seen_subcommand_from gc' \
    -l generations -r \
    -d 'Number of generations to keep (default: 2)'

# upgrade
complete -c nixtamal -n '__fish_seen_subcommand_from upgrade' \
    -l from -r -xa '1.2.0 1.1.0 1.0.0 0.5.0' \
    -d 'Upgrade from a specific version'
complete -c nixtamal -n '__fish_seen_subcommand_from upgrade' \
    -l to -r -xa '1.2.0 1.1.0 1.0.0' \
    -d 'Upgrade to a specific version'
complete -c nixtamal -n '__fish_seen_subcommand_from upgrade' \
    -l dry-run \
    -d 'Show what would be upgraded without making changes'
