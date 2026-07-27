# Hooks, advice, modes, and loading

Source: GNU Elisp Reference Manual ch. 16 (Loading), 13.12 (Advising
Functions), 24 (Major/Minor Modes), 24.1 (Hooks). The order of preference
for changing behavior is **hook > new command + remap > advice** — reach
for the lowest-impact tool that works.

## Hooks

- Normal hook: name ends `-hook`, holds a list of no-arg functions.
  Abnormal hook: ends `-functions`, functions take args / return meaningful
  values. Singular `-function`: one function value — modify with
  `add-function`, not `add-hook`.
- `add-hook HOOK FN &optional DEPTH LOCAL`:
  - No duplicate adds (`equal` comparison — this is why anonymous lambdas on
    hooks are a problem: an edited lambda leaves the old one behind and
    can't be removed. Use **named functions**).
  - New functions run **first** by default. If order matters use DEPTH
    (−100..100, higher = later; legacy `t`/`append` ≈ 90). Never use ±100 —
    leave room for others.
  - LOCAL non-nil makes it buffer-local and appends `t` (meaning "also run
    the global value").
- Modify hooks only with `add-hook`/`remove-hook`, never `setq` or `setopt`
  with a list literal (both clobber other packages' functions; `add-hook` is
  additive). `let`-binding a hook for temporary suppression is fine.
- In `use-package`, `:hook (text-mode . foo-mode)` is the idiomatic way to
  attach; it implies deferral. Names are given without the `-hook` suffix
  unless `use-package-hook-name-suffix` is nil.

## Advice

- Prefer `advice-add`/`advice-remove`/`define-advice` over raw
  `add-function` on `symbol-function`: they handle macros, autoloads,
  docstrings, and not-yet-defined functions, and are listable/undoable.

### Combinator semantics

| WHERE | composition | returns |
|-------|-------------|---------|
| `:before` | `fn` then `old` | old's |
| `:after` | `old` then `fn` | old's |
| `:override` | replace `old` | fn's |
| `:around` | `fn` gets `old` as its first arg | fn's |
| `:before-while` | `(and (fn) (old))` | gated |
| `:before-until` | `(or (fn) (old))` | short-circuit |
| `:after-while` | `(and (old) (fn))` | fn's |
| `:after-until` | `(or (old) (fn))` | fn's if old nil |
| `:filter-args` | `fn` transforms the arg list, then `old` | old's |
| `:filter-return` | `old` then `fn` transforms its result | fn's |

### When NOT to advise

- Advice on a named function others rely on breaks their assumptions and
  confuses debugging. Prefer a hook; if none exists prefer a new command +
  command remapping; use advice only in personal config or when there is no
  hook. Released libraries must not advise other packages' or Emacs's
  functions — request an upstream hook instead.
- **Never advise primitives**: the advice machinery uses some of them
  (infinite recursion), and native-code C-to-C calls bypass advice unless a
  trampoline exists (see the internals `compilation.md`).
- `defadvice` is obsolete since Emacs 30 — use `advice-add`/`define-advice`.

## Mode conventions

- **`define-derived-mode`** is the way to write a major mode: it creates the
  keymap/syntax-table/abbrev-table/hook, handles `delay-mode-hooks` and
  `run-mode-hooks`, and takes `:syntax-table`, `:abbrev-table`,
  `:interactive nil`, `:after-hook`. Parent should be `prog-mode`,
  `text-mode`, `special-mode`, or `fundamental-mode`. Emacs 30:
  `derived-mode-p` takes a list; `major-mode-remap-alist` /
  `major-mode-remap-defaults` remap modes (how this repo routes to
  tree-sitter modes instead of `treesit-auto`).
- **`define-minor-mode`** generates the toggle command, the mode variable,
  and the `minor-mode-alist` lighter (lighter starts with a space).
- Major modes must not set user preferences (don't force Auto Fill); they
  configure so user-enabled features work (`font-lock-defaults`,
  `indent-line-function`, `completion-at-point-functions`,
  `imenu-generic-expression`, buffer-local `eldoc-documentation-functions`).
- Use `make-local-variable` in the mode body; **never**
  `make-variable-buffer-local` on another package's variable (it's global).
- Keybinding reservations: `C-c LETTER` and `F5`–`F9` are the **user's** —
  never bind them from a mode. `C-c C-<letter>` / `C-c DIGIT` are the mode's.

## Loading

- `(require 'feat)` loads `feat.el(c)` unless already in `features`; the file
  must `(provide 'feat)`. Top-level `require` runs during byte-compilation
  (gives macro visibility, silences warnings).
- **Don't `require` inside a `let` that binds the library's own variables** —
  they become unbound when the `let` exits. Require outside.
- Emacs 30: `require-with-check` detects a feature provided by a *different*
  file than expected (shadowed installs).
- **`with-eval-after-load LIBRARY BODY`** runs BODY after LIBRARY loads (or
  immediately if already loaded). LIBRARY is a bare filename string or a
  feature symbol. Caveats: BODY runs in a null lexical context (don't
  capture); an error aborts the rest of BODY; heavy work delays every
  reload. The manual restricts it to personal init files — which is exactly
  this repo — but `use-package`'s `:config`/`:after` are the cleaner
  expression of the same idea and should be preferred here.
