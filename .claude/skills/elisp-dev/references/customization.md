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
  the mode; `custom-initialize-delay` for preloaded/autoloaded files.
- `:local t` (auto buffer-local), `:risky`, `:safe`/`safe-local-variable`,
  `:set-after` (ordering), `:require` (load a feature when set via Custom),
  `:group`, `:package-version`.

## `setopt` vs `setq`

- `setopt` is like `setq` but routes through the Customize machinery: it
  **runs the option's `:set` function** and **type-checks against `:type`**
  (error on mismatch). It does *not* mark the variable for saving to
  `custom-file` (unlike `customize-set-variable`), so it's the correct verb
  for a declarative init.
- `setq` on an option with a `:set` callback **silently skips the
  callback** — the value is stored but the side effect (rebuild/toggle)
  never runs. That is the entire reason for the repo rule.
- `setopt` also works on plain (non-custom) variables — it just sets them —
  so it is a safe default for "set a user option" in config. Avoid it in
  performance-critical inner loops (validation overhead); there `setq` a
  plain `defvar`.
- Emacs 29 introduced `setopt`; Emacs 31 adds `setopt-local` and
  `set-local`.

## `custom-file` in this repo

`init.el` points `custom-file` at `var/custom.el` and treats it as
**write-only** — it is never loaded back. The declarative config in
`lisp/init-*.el` is the single source of truth, so any option a user might
want to change should be expressed as a `setopt` (or `use-package :custom`)
in the modules, not left to interactive Customize. Don't add code that
`(load custom-file)`.

## `use-package :custom`

`:custom (foo 1)` goes through the same `:set`/`:type` path as `setopt` and
is the preferred way to set an option owned by the package a `use-package`
block configures. Prefer it over `setq`/`setopt` in `:init`/`:config` when
the option belongs to that package.
