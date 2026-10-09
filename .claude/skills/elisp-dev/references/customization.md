# Customization: defcustom, setopt, and options

Source: GNU Elisp Reference Manual ch. 15 (Customization) and 12.8 (Setting
Variables). This underpins the repo's hard rule: **`setopt` for `defcustom`
variables, `setq` only for plain `defvar`s.**

## `defcustom`

- `(defcustom OPTION STANDARD DOC &rest KEYWORDS)` declares a user option and
  marks the symbol special (dynamic). STANDARD is an *expression* that must
  stay harmless to re-evaluate; a saved/customized value wins over it.
- **Always give `:type`** — it drives validation and the Customize UI.
  `:options` suggests values for `hook`/`alist`/`plist` types.
- `:set SETFN` — `(lambda (symbol value) …)` run by Customize **and by
  `setopt`** and by `C-M-x`; default `set-default-toplevel-value`. Options
  that rebuild keymaps/timers/font-lock or toggle a mode define a `:set`, so
  they only take effect when set through `setopt`/Customize.
- `:initialize` — usually left default; `custom-initialize-default` is the
  choice for a minor-mode variable so that merely defining it doesn't enable
  the mode; `custom-initialize-delay` for preloaded files; 31 adds
  `custom-initialize-after-file-load` (used automatically by
  `define-globalized-minor-mode` with a non-nil `:init-value`).
- `:local t` (auto buffer-local; 31 also accepts `permanent-only`), `:risky`,
  `:safe`/`safe-local-variable`, `:set-after` (ordering), `:require` (load a
  feature when set via Custom), `:group`, `:package-version`.

## `setopt` vs `setq`

- `setopt` is like `setq` but routes through the Customize machinery: it
  **runs the option's `:set` function** and **type-checks against `:type`**. A
  mismatch only *warns* (`Value does not match …'s type`) and the value is
  still assigned (the manual says "signal an error"; the docstring and
  behavior say warning), so read the `*Warnings*` buffer. It does *not* mark
  the variable for saving to `custom-file` (unlike `customize-set-variable`),
  so it's the correct verb for a declarative init.
- `setq` on an option with a `:set` callback **silently skips the
  callback** — the value is stored but the side effect (rebuild/toggle)
  never runs. That is the entire reason for the repo rule.
- `setopt` also works on plain (non-custom) variables — it just sets them —
  so it is a safe default for "set a user option" in config. Avoid it in
  performance-critical inner loops (the manual: "much less efficient than
  `setq`"); there `setq` a plain `defvar`.
- Emacs 29 introduced `setopt`. In 31 `C-h v` states when an option needs
  `setopt`.
- 31 adds `set-local` (the buffer-local `set`) and `setopt-local`, which
  sets the buffer-local value and leaves the global one alone. It calls
  `:set` with a third BUFFER-LOCAL argument and signals if the `:set`
  function doesn't accept it, so new `:set` functions should take
  `(symbol value &optional buffer-local)`. Both are 31-only: gate them.

## `custom-file` in this repo

`init.el` points `custom-file` at `var/custom.el` and treats it as
**write-only** — it is never loaded back. The declarative config in
`lisp/init-*.el` is the single source of truth, so any option a user might
want to change should be expressed as a `setopt` (or `use-package :custom`)
in the modules, not left to interactive Customize. Don't add code that
`(load custom-file)`.

## `use-package :custom`

`:custom (foo 1)` expands to `custom-theme-set-variables` under the
`use-package` theme (or `customize-set-variable` when
`use-package-use-theme` is nil). It runs the option's `:set` like
`setopt`, but does **no `:type` check**, so a wrong value is accepted
silently. It is the preferred way to set an option owned by the package a
`use-package` block configures; prefer it over `setq` in `:init`/`:config`
when the option belongs to that package. When type safety matters (or the
option belongs to another package), `setopt` in `:config` validates.
