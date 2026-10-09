# TODO

## Emacs performance optimization — open points (was plan.md)

Target: the dev machine (x86_64-darwin, Emacs 31 NS daemon). Priority:
**performance → stability → startup → feel**. Build-variant preference:
**release > experimental (igc)**.

### §3 — igc / MPS concurrent GC trial (experimental, biggest GC-pause win)

`emacs-igc` is verified buildable on x86_64-darwin via the pinned overlay.
Trial only before any promotion. The distribution overlay already takes
`variant` (`nix/mk-overlay.nix`), so a full igc distribution is
`import … { variant = "igc"; }`, a cache **miss** on Darwin (builds from
source).

- `just build-igc`, then run the result as a **side daemon** on its own
  socket (`./result/bin/emacs --fg-daemon=jotain-igc --init-directory=…`)
  next to the release daemon. A/B it for about a week on real workloads
  (large files, LSP, magit); watch for crashes and confirm the pause
  reduction is real on this CPU.
- Quantify with `(setq garbage-collection-messages t)` under both daemons.

### §4 — `ultra-scroll` (feel, lowest priority)

The config uses the built-in `pixel-scroll-mode` (`lisp/init-ui.el`). Only if
a variant switch happens (e.g. to igc), replace it with `ultra-scroll`
(smoother on Intel).

### Verification (for the open work)

1. Baseline: `just bench-built var/bench/before.txt`; profile a freeze with
   `M-x jotain-profile-toggle`.
2. After build changes: re-run `just bench-built`, diff load times, and
   confirm the eln-cache holds `init-*.eln`.
3. GC: `(setq garbage-collection-messages t)`, exercise completion/LSP under
   the release and igc daemons, compare pause counts.
4. Cache parity unchanged: run the `nix-instantiate` parity check from
   `AGENTS.md`. On Linux the default (`unstable`) variant must still equal
   `pkgs.emacs-unstable-pgtk`, and `mainline` `pkgs.emacs-pgtk`.

## In-code deferred work

- `nix/extra-packages.nix`: TEMPORARY (2026-07-21) ghostel epkg fetch
  workaround; revert to plain `epkgs.ghostel` once the upstream fetch works.
- `lisp/init-vc.el`: ideas not yet wired up: mergiraf (structural merge
  driver), magit-delta (delta-rendered magit diffs), and smerge/vc.el
  integration for syntax-aware conflict resolution.
- `module.nix` / `module-system.nix` / `module-nix-on-droid.nix`: the
  `services.jotain.spell.dictionaries` option and the hardcoded
  `aspellDicts.en` do nothing for spell-checking. libaspell's `NIX_PROFILES`
  patch only feeds dictionary enumeration, and the distribution wrapper's
  default `ASPELL_CONF` points at its own bundled en/fi/de/fr set
  (`nix/mk-overlay.nix`; see journal/2026-07-23.md). Build an
  `aspellWithDicts` from the option and export `ASPELL_CONF` in the module
  wrappers, replacing the profile install.
