# TODO

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
- `etc/elisp-doc/elisp-doc-extract.el` — two TODOs in the vendored elisp-doc
  engine (L517, L839); upstream-owned, track here only if we fork it.

## Investigate

All prior candidates triaged 2026-08-14; decisions captured out of band. Add
new candidates here.
