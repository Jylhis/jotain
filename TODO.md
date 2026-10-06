# TODO

## Emacs performance optimization — open points (was plan.md)

Optimization of jotain Emacs for the dev machine (
an x86_64 CPU, **x86_64-darwin**; Emacs 31 NS
daemon). Priority order: **performance → stability → startup → feel**.
Build-variant preference: **release > experimental (igc)**.

### §3 — igc / MPS concurrent GC trial (experimental, biggest GC-pause win)

`emacs-igc` is verified buildable on x86_64-darwin via the pinned overlay.
Experimental, so trial-only before any promotion.

- `just build-igc` → run the result as a **side daemon** on its own socket
  (`./result/bin/emacs --fg-daemon=jotain-igc --init-directory=…`) alongside
  the release daemon. A/B for ~a week on real workloads (large files, LSP,
  magit); watch for crashes and confirm the pause reduction is real on this CPU.
- Quantify with `(setq garbage-collection-messages t)` under both daemons.
- Only if stable: parameterize `jotainEmacsPackages` in `overlay.nix` to accept
  an igc base so the full distribution (packages + grammars) can run on igc.
  Note this is a cache **miss** on Darwin (builds from source).

### §4 — `ultra-scroll` (feel, lowest priority)

`pixel-scroll-precision-mode` is fine on NS-31. Only if a variant switch
happens (e.g. to igc), replace it with `ultra-scroll` (smoother on Intel).
File: `lisp/init-ui.el`.

### Verification (for the open work)

1. Baseline: `just bench-built var/bench/before.txt`; profile a freeze with
   `M-x jotain-profile-toggle`.
2. After build changes: re-run `just bench-built`, diff load times; confirm the
   eln-cache holds `init-*.eln`.
3. GC: `(setq garbage-collection-messages t)`, exercise completion/LSP under
   release vs igc daemons; compare pause counts.
4. Cache parity unchanged: run the `nix-instantiate` parity check from
   `AGENTS.md`; the default (`unstable`) variant must still equal
   `pkgs.emacs-unstable`, and the `mainline` variant `pkgs.emacs`.

## Deferred review findings (docs/reviews/2026-07-emacs-nix-deep-review.md)

- **Finding 21** — make `devenv-env--turn-on` subprocess-free. The long-TTL
  trust cache and the async modeline probe shipped (`lisp/devenv.el`
  `devenv-modeline--cached-trust` / `devenv-modeline--probe-trust`); the
  remaining gap is `devenv-env--turn-on`, which still calls
  `devenv--trust-state` synchronously and `call-process`es
  `devenv hook-should-activate` on a cache miss, so the first find-file in an
  uncached project blocks. Rework to consult only the cache and replay via the
  async probe callback. Needs a live Emacs to validate.
- **Finding 52** — bench harness fidelity. The old "disabled stub" `just bench`
  is gone; `bench-built` (`Justfile:49`) is a working harness. Confirm it times
  autoload-driven post-init loads (via a `load` file-name handler or a
  `features` snapshot) rather than only `require`, and measures the archive
  refresh synchronously.

## In-code deferred work

- `nix/extra-packages.nix` — TEMPORARY (2026-07-21) ghostel epkg fetch
  workaround; revert to plain `epkgs.ghostel` once the upstream fetch works.
- `lisp/init-vc.el` — future ideas not yet wired up: mergiraf (structural merge
  driver), magit-delta (delta-rendered magit diffs), and smerge/vc.el
  integration for syntax-aware conflict resolution.
- `module.nix` / `module-system.nix` / `module-nix-on-droid.nix` — the
  `services.jotain.spell.dictionaries` option and the hardcoded `aspellDicts.en`
  are no-ops for actual spell-checking (jinx reads `jinx-languages`, not the
  profile dicts; see journal/2026-07-23.md). Build an `aspellWithDicts` from
  the option and export `ASPELL_CONF` in the module wrappers, replacing the
  profile install.
- `etc/elisp-doc/elisp-doc-extract.el` — two TODOs in the vendored elisp-doc
  engine (L517, L839); upstream-owned, track here only if we fork it.

## Investigate

All prior candidates triaged 2026-08-14; decisions captured out of band. Add
new candidates here.
