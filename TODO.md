# TODO

- https://github.com/federicotdn/verb
- Disable "Process zoxide finished" messages in zoxide buffer
- Fix themes in terminal

## Deferred review findings (docs/reviews/2026-07-emacs-nix-deep-review.md)

- **Finding 12, option (b)** — adopt `completion-preview` alongside corfu in
  `init-completion.el` (with the Emacs 31 `completion-preview-sort-function`
  pairing). Feature decision; the docs-only fix (option a) shipped instead.
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

- https://github.com/kickingvegas/casual
- https://github.com/emacs-vs/line-reminder
- https://github.com/sulami/literate-calc-mode.el
- https://github.com/benma/visual-regexp.el
- https://www.emacswiki.org/emacs/AlignCommands
- https://github.com/universal-ctags/citre
- https://www.gnu.org/software/emacs/manual/html_node/ebrowse/index.html
- https://github.com/pythonic-emacs/anaconda-mode
- https://github.com/emacs-elsa/Elsa
- https://github.com/elisp-lsp/ellsp
- https://github.com/Wilfred/suggest.el
- https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html
- https://github.com/elixir-editors/emacs-elixir
- https://github.com/swift-emacs/swift-mode
- https://github.com/Emacs-Kotlin-Mode-Maintainers/kotlin-mode
- https://github.com/hlissner/emacs-mips-mode
- https://github.com/AdamNiederer/riscv-mode
- https://github.com/kubernetes-el/kubernetes-el
- https://github.com/lastquestion/explain-pause-mode
- https://github.com/syl20bnr/spacemacs
- https://github.com/purcell/emacs.d
- https://github.com/bbatsov/prelude
- https://github.com/doomemacs/doomemacs
- https://github.com/seagle0128/.emacs.d
- https://github.com/thefrontside/frontmacs
- https://github.com/overtone/emacs-live
- https://github.com/daviwil/emacs-from-scratch
- https://codeberg.org/ashton314/emacs-bedrock
- https://github.com/LionyxML/emacs-solo
