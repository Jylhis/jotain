# TODO

- https://github.com/federicotdn/verb
- Disable "Process zoxide finished" messages in zoxide buffer
- Fix themes in terminal

## Deferred review findings (docs/reviews/2026-07-emacs-nix-deep-review.md)

- ~~**Finding 12, option (b)** — adopt `completion-preview` alongside corfu in
  `init-completion.el` (with the Emacs 31 `completion-preview-sort-function`
  pairing).~~ **Done 2026-08-26** — `completion-preview-mode` in
  `init-completion.el`, gated by `jotain-completion-inline-preview`, with the
  sort pairing, the R2 `C-i` unbind, and a comment/string inhibit. See
  `docs/design/completion.md` §6 and `docs/reviews/2026-08-completion-ux-research.md`.
- **Finding 21** — make `devenv-env--turn-on` subprocess-free: consult only
  `devenv-modeline--cached-trust` and replay via the async
  `devenv-modeline--probe-trust` callback (the long-TTL trust cache shipped;
  the async rework needs a live Emacs to validate).
- **Finding 48, part (c)** — scanner-fidelity flake check: batch-read
  `lisp/*.el` with Emacs's own reader, collect `(use-package NAME)` heads, and
  diff against `scanDirectory` output (new CI machinery; parts a/b shipped in
  `nix/use-package.nix`).
- **Finding 52** — bench harness rework: time autoload-driven loads via a
  file-name handler for the `load` operation (or attribute post-init loads by
  snapshotting `features`), and measure the archive refresh synchronously
  (`just bench` is currently a disabled stub anyway).
- **Finding 53, full fix** — generate `config/eca/config.json`'s model list and
  `init-ai.el`'s gptel `:models` from a single source, or add a checks.nix
  drift check (the minimal cross-reference comments shipped).

## Investigate

All prior candidates triaged 2026-08-14; decisions captured out of band. Add
new candidates here.
