# Modern Emacs Lisp libraries and idioms

Source: GNU Elisp Reference Manual, `seq.el`/`map.el`/`cl-lib`/`rx`/`pcase`
nodes, NEWS.30/NEWS.31. Prefer these over hand-rolled list surgery; they are
built in on Emacs 30/31.

## pcase

- `pcase EXP &rest CASES`. Patterns: `_` (any), `SYM` (bind), `'VAL` /
  self-quoting literal (equal), `` `(,a ,b) `` (destructure), `(pred FN)`,
  `(app FN PAT)`, `(guard EXPR)`, `(and …)`, `(or …)`, `(rx …)`,
  `(cl-type TYPE)`, `(map KEY …)`, `(seq PAT …)`.
- Family: `pcase-let`, `pcase-let*`, `pcase-dolist`, `pcase-lambda`,
  `pcase-exhaustive` (errors on no match), `pcase-defmacro`.
- Keep patterns readable — deeply clever `pcase` is a maintenance liability;
  for simple bind-and-test, `if-let*`/`when-let*` are clearer. Emacs 31 adds
  `cond*` as an official cond/pattern-matching hybrid.

## seq.el (generic over lists/vectors/strings)

`seq-map` `seq-filter` `seq-remove` `seq-reduce` `seq-find` `seq-some`
`seq-every-p` `seq-contains-p` `seq-count` `seq-sort` `seq-sort-by`
`seq-uniq` `seq-take` `seq-drop` `seq-take-while` `seq-drop-while`
`seq-subseq` `seq-mapn` `seq-do` `seq-map-indexed` `seq-group-by`
`seq-partition` `seq-difference`/`-intersection`/`-union` `seq-empty-p`
`seq-elt` (setf-able) `seq-let` `seq-keep` `seq-split`. Side-effect-free,
extensible via `cl-defgeneric`. Rule of thumb: `dolist` for side effects,
`seq-*` for value transformations, `seq-do` only with a named function.

## map.el (generic over alists/plists/hash-tables/arrays)

`map-elt` (setf-able) `map-put!` `map-delete` `map-nested-elt` `map-keys`
`map-values` `map-pairs` `map-length` `map-do` `map-filter` `map-some`
`map-contains-key` `map-merge` `map-merge-with` `map-into` `map-let`, and the
`(map …)` pcase pattern. Emacs 31 adds `hash-table-contains-p`.

## cl-lib

- Always `cl-lib`, never the removed `cl`. Highlights: `cl-defun`/
  `cl-defmacro` (keyword & optional args), `cl-loop`, `cl-case`,
  `cl-typecase`, `cl-destructuring-bind`, `cl-flet`/`cl-labels` (lexical
  local functions), `cl-letf` (temporarily rebind *places* incl. function
  cells — the standard test-double trick; `flet` is dead), `cl-defstruct`,
  `cl-defgeneric`/`cl-defmethod`, `cl-check-type`, `cl-assert`,
  `cl-with-gensyms`, `cl-once-only`.
- **Generalized variables**: `setf` works on `car`, `nth`, `plist-get`,
  `alist-get`, `map-elt`, `symbol-function`, `buffer-local-value`, …;
  `cl-incf`/`cl-decf`/`push`/`pop`/`cl-callf`/`cl-rotatef` on places;
  `gv-define-setter` for new ones. `(setf (alist-get k a) v)` (with
  `:remove` for deletion) is the idiomatic alist updater.
- Emacs 31 promotes `incf`/`decf`/`plusp`/`minusp`/`oddp`/`evenp` to core
  (un-prefixed).

## rx (structured regexps)

`(rx …)` compiles to a regexp string at compile time: `seq` `or` `group`
`group-n` `opt` `zero-or-more`/`*` `one-or-more`/`+` `repeat` `any` (as a
set) `not` `syntax` `bol`/`eol`/`bos`/`eos` `word-boundary` `literal`
`regexp` `eval`; `rx-define`/`rx-let` for reusable components; also usable as
a pcase pattern with group bindings. Use it for any nontrivial regexp;
`relint` checks hand-written ones. (Emacs 31 obsoletes the `any` *atom* for
"."; use `anychar`.)

## Binding, threading, and control-flow macros

- `if-let*` / `when-let*` / `and-let*` / `while-let` — bind-and-test chains.
  **Emacs 31 obsoletes `if-let`/`when-let`** and the single-binding
  `(if-let (sym val))` spelling; use the starred forms now.
- `thread-first` / `thread-last` — pipeline macros for genuinely linear
  transformations (don't force non-linear code into them).
- `named-let` — Scheme-style tail-recursive local loop (compiles tail calls
  to iteration; elisp has no general TCO).
- `ensure-list` — normalize x-or-list-of-x arguments.
- `handler-bind` (Emacs 30) — run an error handler *without* unwinding the
  stack (backtrace-preserving).
- `static-if` (30) / `static-when` / `static-unless` (31) — compile-time
  feature/version gating with no runtime cost.
- `with-memoization`, `benchmark-progn`, `while-no-input`.

## Strings, keymaps, sorting

- `string-empty-p` `string-join` `string-trim` `string-pad`
  `string-search` `string-replace` (prefer over
  `replace-regexp-in-string` for literal replacement) `string-chop-newline`.
- **`defvar-keymap`** + `keymap-set`/`keymap-global-set`/`keymap-lookup`
  (Emacs 29) replace `define-key`/`kbd`. `defvar-keymap` takes `:repeat`;
  Emacs 31 adds `:prefix`/`:continue`.
- Emacs 30 extended `sort` with `:key`/`:lessp`/`:reverse`/`:in-place`, and
  added `value<`, `merge-ordered-lists`, `take`/`ntake`, `drop`.

## Performance idioms (measure first)

- `memq`/`member`/`assq`/`assoc` beat manual loops; `buffer-local-value` is
  far cheaper than `with-current-buffer` for a single variable read; batch
  `insert` one concatenated string rather than many small inserts; hash
  tables for large keyed lookups (plists only for tiny ones). Use
  `defsubst`/`define-inline` only on measured hot paths. Profile with
  `M-x profiler-start` and `benchmark-run`, not intuition.
