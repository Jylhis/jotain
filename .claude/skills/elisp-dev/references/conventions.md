# Coding, naming, and documentation conventions

Source: GNU Elisp Reference Manual appendix D (Tips and Conventions),
bbatsov/emacs-lisp-style-guide, checkdoc. Repo rules (CLAUDE.md/AGENTS.md)
override where they conflict.

## Symbol naming

- **Every global symbol** (function, variable, face, constant) starts with
  the library prefix: `foo-thing`, never bare `thing`. In this repo the
  prefix is the module concern (`init-...`) for module-internal helpers.
- **Private symbols use a double hyphen**: `foo--internal` — not public API,
  may change without notice. Reach for it liberally for helpers.
- Predicates: single word ends in `p` (`stringp`), multi-word ends in `-p`
  (`buffer-live-p`).
- Boolean variables: **do not** use `-p`; use a `-flag` suffix or a
  "Non-nil means …" docstring.
- A single function value → `-function`; a list of functions (abnormal
  hook) → `-functions`; a normal hook → `-hook`; one predicate function →
  `-predicate`.
- Face names must **not** end in `-face` (checkdoc-enforced).
- Prefer `file`/`file-name`/`directory` over `path` in names — GNU reserves
  "path" for search paths (`load-path`, `exec-path`).
- Prefix unused lexical args with `_` to silence the byte-compiler:
  `(lambda (x _y) x)`.
- `lisp-case` throughout; spaces not tabs (`indent-tabs-mode` nil); no
  closing parens on their own line; ≤80 columns; no trailing whitespace.
- Keep positional params ≤3–4; beyond that use `cl-defun` keyword args or a
  plist.

## Library / file headers

For a standalone library the first line is exactly:

```elisp
;;; foo.el --- One-line description  -*- lexical-binding: t; -*-
```

then copyright/license, then `;; Header: value` lines (`Author:`,
`Maintainer:`, `Version:`, `Package-Requires:` as
`((emacs "30.1") (compat "30.1"))`, `Keywords:` from `finder-known-keywords`
only, `URL:`), then `;;; Commentary:`, `;;; Code:`, the code, `(provide
'foo)`, and the mandatory footer `;;; foo.el ends here`.

This repo's `lisp/init-*.el` modules follow the abbreviated house form: the
`-*- lexical-binding: t; -*-` cookie on line 1 and `(provide 'init-<concern>)`
at the end — that is the required shape, don't add full FSF headers to them.

## Autoload cookies (`;;;###autoload`)

- Cookie on its own line before a definition → an autoload stub is scraped
  into the generated loaddefs. Recognized specially for `defun`, `defmacro`,
  `define-minor-mode`, `define-derived-mode`, `defcustom`, `defgroup`,
  `define-globalized-minor-mode`, and friends; any other form after the
  cookie is copied verbatim (runs at activation).
- **Autoload only user entry points**: interactive commands, `auto-mode-alist`
  additions. Never autoload internal functions, plain variables, or user
  options.
- **Never add an autoload cookie to silence a compiler warning** — use
  `(defvar foo)`, `declare-function`, or `require` instead.
- In this Nix/`use-package` repo, packages are on `load-path` already;
  autoload cookies matter mainly for the repo's own `lisp/` commands if any
  need deferring — most config wiring is done through `use-package` keywords
  instead.

## Comments

- `;` margin comment aligned right of code; `;;` aligned with code, describes
  following lines (also used to comment out code); `;;;` left-margin Outline
  headings (three = section, four = subsection). `M-;` does the right thing.
- Annotation keywords `TODO`/`FIXME`/`HACK`/`REVIEW` as `KEYWORD: text` on
  the line above the code.

## Docstrings

- **First line**: one or two complete sentences, self-contained (apropos
  shows only it), capital start, period end, ≤ ~74 chars. Functions answer
  "what does it do"; variables "what does the value mean".
- **Imperative present tense**: "Return the cons of A and B." — not
  "Returns…". Predicates: "Return t if …". Boolean vars: "Non-nil means …".
- **Arguments in CAPS** matching the arglist: "Evaluate FORM…".
- Quote symbols as `` `symbol' `` (renders as curly quotes); `t`/`nil` bare;
  don't quote non-symbol expressions.
- Key references: `\\[command]` not a hardcoded `C-x`; `\\<map>` /
  `\\{map}` for a mode's own bindings.
- Never indent continuation lines to align with source; verify with
  `M-x checkdoc`. Emacs 30 warns on control chars and (separately) overwide
  docstrings — both are byte-compile warnings, which `just compile` fails on.

## Etiquette that catches config bugs

- In Lisp use `forward-line`, never `next-line`/`previous-line`.
- Don't call mark-moving/interactive helpers (`beginning-of-buffer`,
  `replace-string`, `replace-regexp`, `insert-file`) from code — use their
  programmatic equivalents (`goto-char (point-min)`, `re-search-forward`+
  `replace-match`, `insert-file-contents`).
- Signal errors with `error`/`signal` (message: capital first letter, **no**
  trailing period); never report failures via `message`/`beep`.
- `#'fn` for function references (compiler checks definedness), `'sym` for
  data symbols; never `'(lambda …)` or a bare `(lambda …)` where `#'f`
  suffices.
- Prefer `when`/`unless` over one-armed `if`; `t` not `:else` in `cond`.
