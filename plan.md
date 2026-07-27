# Emacs performance optimization — open points

Optimization of jotain Emacs for the dev machine (
an x86_64 CPU, **x86_64-darwin**; Emacs 31 NS
daemon). Priority order: **performance → stability → startup → feel**.
Build-variant preference: **release > proven fork (macport) > experimental (igc)**.

## Done — §1 runtime tuning (release build, low risk)

Shipped; verified clean by the `elisp-compile` flake check.

- `early-init.el` — `native-comp-async-jobs-number 3` (leave a physical core
  free for redisplay/input during background JIT).
- `lisp/init-ui.el` — `fast-but-imprecise-scrolling t` in the pixel-scroll block.
- `lisp/init-prog.el` — `jit-lock-defer-time 0.05` (treesit font-lock level 4
  is heavy on this CPU).
- `Justfile` — `compile-native` disabled stub (matches the convention that
  emacs recipes are stubs while emacs is out of the devenv shell).

**Already optimal — do not regress:** GC ramp (early-init `most-positive-fixnum`
→ init-core 16 MB + minibuffer pause + 5 s idle GC), `read-process-output-max`
4 MB, `eglot-events-buffer :size 0`, bidi off, `inhibit-compacting-font-caches`,
native-comp speed 2, and the `treesit-auto` alist-only routing (NOT
`global-treesit-auto-mode`, which costs ~3.6 s/find-file).

## Done — §5 x86_64-darwin binary-cache EOL

Shipped. `flake.nix` adds the dedicated `nixpkgs-x86_64-darwin` input pinned to
`nixpkgs-26.05-darwin` and selects it only for `x86_64-darwin` via `nixpkgsFor`
(every other system stays on `nixpkgs-unstable`); `emacs.nix` reads the same
per-system node out of `flake.lock`'s root input map. Because the pin is its
own flake input — not a divergence of the shared `nixpkgs` input — `just
verify` and the `devenv.yaml`/`devenv.lock` sync invariant are unaffected, and
no CLAUDE.md exception is needed. Remaining operational check (on the Darwin
host itself): confirm `jylhis.cachix.org` is in the host's substituters and
carries the x86_64-darwin Emacs + full distribution.

---

## Open points

### §2 — `-march=icelake-client` from-source perf build (opt-in)

If this machine ever must build Emacs from source anyway (the §5 Darwin pin,
now shipped, keeps prebuilt binaries flowing for the time being), a CPU-tuned
build costs nothing extra and gains a few percent on this i5.

- Add an opt-in flag/recipe (`just build-perf` or an `emacs.nix` arg) that
  sets `NIX_CFLAGS_COMPILE = "-O3 -march=icelake-client -mtune=icelake-client"`
  via `overrideAttrs`. **Do not** make it the default — it busts cache parity.
- Consider dropping `withXwidgets` for this build if the embedded WebKit
  widget is unused — smaller, faster local builds.
- Files: `emacs.nix`, `Justfile`.

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
happens (macport breaks pixel-scroll; igc), replace it with `ultra-scroll`
(smoother on Intel). File: `lisp/init-ui.el`.

---

## Verification (for the open work)

1. Baseline: `just bench bench-before.txt` (currently a disabled stub —
   re-enable it first); profile a freeze with `M-x jotain-profile-toggle`.
2. After build changes: re-run `just bench`, diff load times; confirm the
   eln-cache holds `init-*.eln`.
3. GC: `(setq garbage-collection-messages t)`, exercise completion/LSP under
   release vs igc daemons; compare pause counts.
4. Cache parity unchanged: run the `nix-instantiate` parity check from
   `CLAUDE.md` — the default (`unstable`) variant must still equal
   `pkgs.emacs-unstable`, and the `mainline` variant `pkgs.emacs`.
