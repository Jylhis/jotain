# Emacs 30 / 31 changes for config and package authors

Source: `etc/NEWS.30`, `etc/NEWS` (31.1), use-package manual, checked
against the shipped 31.1 binary. Filtered to what matters for a config repo
like this one. **Emacs 31.1 is released (2026-08-24) and is the repo's
default build; the floor is 30.1** (`init.el` `Package-Requires`). Gate
31-only APIs with `static-if` / `(when (>= emacs-major-version 31) …)`, or
keep the `cl-` spellings where both exist. Conversely, don't guard for
versions at or below the 30 floor (e.g. `(>= emacs-major-version 29)`):
such guards are dead and misleading.

## Emacs 30 (the floor)

### Runtime / compiler
- **Native compilation on by default** at configure time when libgccjit
  works (a build default; runtime JIT is `native-comp-jit-compilation`);
  `compilation-safety` option added. 30.2 is a bug-fix release.
- New byte-compiler warnings (all fail the `elisp-compile` flake check,
  run via `just check`): missing `lexical-binding` cookie (compile time),
  empty bodies, quoted error names in `condition-case`/`ignore-error`,
  comparison by identity with literals (`(eq x "str")`), `condition-case`
  without handlers, `unwind-protect` without unwind forms, useless
  trailing `cond` clauses, **mutation of constants**, more ignored return
  values, control chars in docstrings. The wide-docstring warning is
  older (28) and only became separately suppressible (`docstrings-wide`).
- New object types `closure` / `interpreted-function`; `cl-type-of` is
  finer-grained.
- **Obsoletions**: `defadvice`; `easy-mmode-define-{minor,global}-mode`;
  `subr-native-elisp-p` → `native-comp-function-p`; the `&rest` calling
  form of `derived-mode-p` (pass a list).

### New Lisp worth using
- `handler-bind`, `static-if`, `value<`, extended `sort`
  (`:key`/`:lessp`/`:reverse`/`:in-place`), `merge-ordered-lists`,
  `drop`, `require-with-check`, `derived-mode-p` list API +
  `derived-mode-add-parents`, `major-mode-remap-defaults` (the
  `major-mode-remap-alist` user option is 29), `declare` `ftype` and
  `important-return-value`, built-in JSON parser (`json-available-p`
  always t).
- `define-advice` now sets the advice's `name` property, so
  `(advice-remove SYM NAME)` works.

### Config-facing features (relevant to modules here)
- **`use-package :vc`** (install from upstream repo via package-vc) +
  `use-package-vc-prefer-newest` (default nil: latest release, not HEAD).
- Built in: **which-key** (`which-key-mode` still off by default, so
  `:ensure nil` plus enabling it), **completion-preview-mode**,
  **EditorConfig**, Compat, Track-Changes.
- Tree-sitter: TS modes become extra parents of their non-TS mode
  (dir-locals and yasnippet inherit; mode *hooks* do not), loading a
  ts-mode file auto-remaps its non-TS mode (a self-mapping entry in
  `major-mode-remap-alist` blocks that), `treesit-thing-settings`,
  outline/imenu support, `treesit-install-language-grammar`. This repo
  routes via `major-mode-remap-alist` (dropped `treesit-auto`).
- `trusted-content` (list of files/dirs, or `:all`; default nil) /
  `safe-local-variable-directories` security options (CVE-2024-53920
  context; never set `trusted-content` to `:all` from a mode). Untrusted
  files get no `elisp-flymake-byte-compile`, so Flymake is quiet on Elisp
  outside trusted dirs.
- `advice-remove` interactive; `customize-dirlocals`.

## Emacs 31.1 (default build)

### Startup
- **`site-start.el` now loads *before* `early-init.el`** (was after). On
  Nix this means nixpkgs' `site-start.el` (load-path, tree-sitter grammar
  path, Info trailing colon) is already in effect when this repo's
  `early-init.el` runs.
- New **User Lisp directory**: a `user-lisp/` subdir of the config dir is
  recursively byte-compiled, autoload-scraped, and added to `load-path`
  (`user-lisp-directory`, `user-lisp-auto-scrape`, `prepare-user-lisp`).
  Obsoletes the `use-package :vc` + `:load-path` combo and
  `package-vc-install-from-checkout`. Relevant only if this repo ever
  ships local Lisp outside `lisp/`.
- Warnings from daemon startup now show in the first client frame
  (relevant to the Home Manager daemon). `xterm-mouse-mode` is on by
  default in compatible terminals (affects the `-nox` build).

### Lexical binding
- Still **not** the default. The default is changeable via
  `(set-default-toplevel-value 'lexical-binding t)`; **loading** a
  cookie-less file now warns (separate from 30's compile-time warning);
  `-x`/`--script` files are lexical by default. Keep every file's cookie.

### Obsolete / incompatible (watch when bumping)
- **`if-let`/`when-let` obsolete → `if-let*`/`when-let*`/`and-let*`**,
  which also obsoletes the single-binding `(if-let (SYM VAL) …)` spelling
  (it still runs, with a warning, which fails `elisp-compile`).
- **Font-lock face *variables* obsolete** (`font-lock-keyword-face` and
  13 others as variables): an unquoted reference is now a byte-compile
  warning. Use the quoted face symbol. Quoted uses in data are fine.
- Nested backquotes no longer supported inside pcase patterns.
- `rx` atom `any` (an old alias of `not-newline`) is obsolete and warns;
  use `nonl`/`not-newline` for ".", or `anychar` when newline should match
  too. The `(any …)` set form is unaffected.
- **String mutation restricted**: `aset` on a unibyte string needs a byte
  (0–255); on a multibyte string both old and new chars must be ASCII.
  Violations signal. The compiler also merges more `equal` constants, so
  `eq` between literals is unpredictable.
- `purecopy` → obsolete alias of `identity`.
- `FOO-ts-mode-indent-offset` → `FOO-ts-indent-offset`.
- `text-property-default-nonsticky` is buffer-local when set.
- `debug` in batch no longer kills Emacs. With `PATH` unset or empty,
  `exec-path` now acts as if PATH were the system default
  (`/bin:/usr/bin` on GNU/Linux).
- `byte-compile-cond-use-jump-table`, `cl-member-if` and `cl-gensym`
  (use `gensym`) are obsolete; `redisplay-dont-pause` is removed.

### New Lisp
- **`cond*`** (pattern-matching conditional); un-prefixed
  `incf`/`decf`/`plusp`/`minusp`/`oddp`/`evenp`/`member-if` (the `cl-`
  names are now deprecated aliases; the unprefixed ones don't exist on 30,
  so gate or keep `cl-`); `take-while`/`drop-while`, `all`/`any`;
  `static-when`/`static-unless`; `setopt-local`/`set-local` (see
  `customization.md`); `buffer-local-toplevel-value` /
  `set-buffer-local-toplevel-value`; `defvar-local` value optional;
  `hash-table-contains-p`; `ensure-proper-list`; `with-work-buffer`;
  `let-alist` list indexing (`.key.0`); binary `%b`/`%B` in `format`;
  `secure-hash` SHA-3; `native-compile-directory`.
- Error descriptor API: `error-type-p`, `error-has-type-p`,
  `error-slot-value`, `(signal ERR)` with a whole error object.
- New advice combinator `:interactive-only` (changes only the interactive
  spec); new `interactive` code `"R"` (region bounds or nil).
- `load-path` lookup caching, opt-in: set `load-path-filter-function` to
  `load-path-filter-cache-directory-files`.
- ERT: `ert-with-test-buffer`, `ert-with-buffer-selected`,
  `ert-with-buffer-renamed` moved from `ert-x` into `ert`; new
  `ert-play-keys` (in `ert-x`).

### Config-facing features
- **`treesit-enabled-modes`** (default nil; t or a list, populates
  `major-mode-remap-alist` from `treesit-major-mode-remap-alist`),
  `treesit-auto-install-grammar` (`never`/`always`/`ask`/`ask-dir`,
  default `ask`), keyword form for `treesit-language-source-alist`
  entries (`(LANG URL :commit REV :source-dir DIR …)`), and pinned grammar
  recipes for many built-in modes (Rust, JS/TS/TSX, Go, C/C++, JSON, Lua,
  TOML, YAML, Dockerfile; not Python). This repo gets its grammars from
  Nix, so these mainly matter for an `emacs -Q` fallback.
- `defvar-keymap :prefix t` (shorthand for naming the prefix command after
  the map; `:prefix SYMBOL` is 29). `defvar-keymap :continue`, the
  `repeat-continue` symbol property, and `use-package`/`bind-keys`
  `:continue-only` extend repeat-mode maps.
- `describe-variable` (`C-h v`) now says when an option needs `setopt`.
- Settings changed in an enabled theme apply immediately.
- PGTK/GTK: toolkit widgets follow the system dark/light theme; the
  `toolkit-theme` variable and `toolkit-theme-set-functions` hook let a
  config follow it too.
- `package-refresh-contents` is asynchronous when called interactively.
- ElDoc and Elisp semantic-highlighting improvements;
  `lisp-indent-local-overrides` file-local.
