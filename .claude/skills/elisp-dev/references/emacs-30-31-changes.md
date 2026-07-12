# Emacs 30 / 31 changes for config and package authors

Source: `etc/NEWS.30`, `etc/NEWS.31`, use-package manual. Filtered to what
matters for a config repo like this one. Emacs 31 is branched (feature
freeze May 2026) and near release as of mid-2026; treat its items as
"landing soon", and gate anything version-specific with `static-if` /
`(when (>= emacs-major-version 31) …)`. Conversely, don't guard for versions
at or below the repo's Emacs 30 floor (e.g. `(>= emacs-major-version 29)`) —
such guards are dead and misleading.

## Emacs 30 (this repo's baseline)

### Runtime / compiler
- **Native compilation on by default** when libgccjit is present; bytecode
  loaded eagerly. `compilation-safety` option added.
- New byte-compiler warnings (all fail `just compile`): missing
  `lexical-binding` cookie, empty bodies, quoted error names in
  `condition-case`/`ignore-error`, `condition-case` without handlers,
  `unwind-protect` without unwind forms, **mutation of constants**, ignored
  return values, control chars in docstrings, over-wide docstrings.
- New object types `closure` / `interpreted-function`; `cl-type-of` is
  finer-grained.
- **Obsoletions**: `defadvice`; `easy-mmode-define-{minor,global}-mode`;
  `subr-native-elisp-p` → `native-comp-function-p`.

### New Lisp worth using
- `handler-bind`, `static-if`, `value<`, extended `sort`
  (`:key`/`:lessp`/`:reverse`/`:in-place`), `merge-ordered-lists`,
  `require-with-check`, `define-advice` (names advice `SYMBOL@NAME`),
  `derived-mode-p` list API + `derived-mode-add-parents`,
  `major-mode-remap-alist` / `major-mode-remap-defaults`, `declare` `ftype`
  and `important-return-value`, built-in JSON parser
  (`json-available-p` always t).

### Config-facing features (relevant to modules here)
- **`use-package :vc`** (install from upstream repo) + `use-package-vc-prefer-newest`.
- **which-key**, **completion-preview-mode**, **EditorConfig**, Compat,
  Track-Changes now built in.
- Tree-sitter: TS modes registered as submodes of their non-TS mode (so
  dir-locals inherit), `treesit-thing-settings`, outline/imenu support,
  `treesit-install-language-grammar`. This repo routes via
  `major-mode-remap-alist` (dropped `treesit-auto`).
- `trusted-content` / `safe-local-variable-directories` security options
  (CVE-2024-53920 context — never set `trusted-content` to `:all` from a mode).
- `advice-remove` interactive; `customize-dirlocals`.

## Emacs 31 (landing soon)

### Lexical-binding endgame
- Default changeable via `(set-default-toplevel-value 'lexical-binding t)`;
  **load-time warning for cookie-less files**; `-x`/`--script` lexical by
  default. Keep every file's cookie.

### Obsolete / incompatible (watch when bumping)
- **`if-let`/`when-let` obsolete → `if-let*`/`when-let*`/`and-let*`**; the
  single-binding `(if-let (sym val))` form is gone.
- Nested backquotes banned inside pcase patterns.
- `rx` atom `any` (the "." sense) obsolete → `anychar`.
- Mutating string/list literals now genuinely breaks (more constant
  coalescing).
- `purecopy` → obsolete alias of `identity` (pure space removed).
- `FOO-ts-mode-indent-offset` → `FOO-ts-indent-offset`.
- `debug` in batch no longer kills Emacs; `exec-path` default matches PATH.

### New Lisp
- **`cond*`** (pattern-matching conditional); un-prefixed
  `incf`/`decf`/`plusp`/`minusp`/`oddp`/`evenp`; `take-while`/`drop-while`,
  `all`/`any`; `static-when`/`static-unless`; `setopt-local`/`set-local`;
  `defvar-local` value optional; `hash-table-contains-p`;
  `ensure-proper-list`; `with-work-buffer`; binary `%b`/`%B` in `format`;
  `secure-hash` SHA-3; `native-compile-directory`; load-path lookup caching.

### Config-facing features
- **User Lisp directory** (`user-lisp/` under the config dir): auto
  byte-compiled, autoload-scraped, and added to `load-path`
  (`user-lisp-directory`, `user-lisp-auto-scrape`). Obsoletes the
  `use-package :vc` + `:load-path` combo and
  `package-vc-install-from-checkout`. Relevant if this repo ever ships local
  Lisp outside `lisp/`.
- **`treesit-enabled-modes`** (t or a list — populates `major-mode-remap-alist`
  from `treesit-major-mode-remap-alist`), `treesit-auto-install-grammar`
  (`ask`/`always`), keyword `treesit-language-source-alist` (`:commit`),
  built-in grammar recipes for TypeScript/Rust/TOML/YAML/Dockerfile — may
  let this repo simplify its tree-sitter and grammar wiring.
- `defvar-keymap :prefix`/`:continue`, `use-package`/`bind-keys`
  `:continue-only` for repeat-mode; PGTK follows system dark/light; ElDoc
  and Elisp semantic-highlighting improvements; `lisp-indent-local-overrides`
  file-local.
