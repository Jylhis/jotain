# Lexical binding and macros

Source: GNU Elisp Reference Manual ch. 12 (Variable Scoping) and ch. 14
(Macros).

## Lexical binding

- Selected by the buffer-local `lexical-binding`, set from the first-line
  `-*- lexical-binding: t; -*-` cookie. Every file in this repo has it; new
  files must. Emacs 30 *warns* when the cookie is missing; Emacs 31 warns at
  plain `load` time and makes `-x`/`--script` files lexical by default.
- Under lexical binding a variable reference must be textually inside its
  binding form; closures capture bindings with **indefinite extent**. Lookup
  order: lexical environment first, then the symbol's dynamic value cell.
- **`symbol-value`/`boundp`/`set`/`makunbound` see only the dynamic
  binding** — a lexical variable has no runtime tie to its symbol. Mixing
  `intern`/`symbol-value` tricks with lexical locals is the classic trap.
- **Special (dynamic) variables**: anything defined with `defvar` (with a
  value), `defconst`, or `defcustom` is *always* dynamically bound,
  everywhere, including by `let`. Test with `special-variable-p`. A special
  variable must **never** be used as a function/macro parameter name (the
  byte-compiler warns; behavior is unsupported).
- **`(defvar foo)` with no value** marks `foo` special only for the rest of
  the current file/scope — the idiom to declare a dynamic variable you bind
  locally but that is defined elsewhere, and to silence "reference to free
  variable" warnings without loading the defining file.
- `(eval FORM t)` evaluates in the lexical dialect; `(eval FORM)` uses
  dynamic — a subtle bug source when building forms at runtime.
- Converting a file: add the cookie, byte-compile, add a `defvar` for each
  "free variable" warning, delete or `_`-prefix each "unused lexical
  variable".

## When dynamic binding is right

Use dynamic variables only for genuine dynamic protocol (like
`case-fold-search`, `default-directory`, `inhibit-read-only`): always
`defvar`/`defcustom` them at top level with a docstring and prefix. If a
variable is only ever `let`-bound, use a value-less `defvar` so a stray
global read errors loudly.

## Macros

### Fundamentals

- `defmacro` receives its arguments **unevaluated**; its result form is then
  evaluated/compiled in place. Macros can't be `mapcar`'d and can't be
  interactive. Prefer backquote: `` `(if ,test ,then) ``.
- Macros must be **defined before they are used in compilation**. A
  `defmacro` earlier in the same file is available for the rest of that
  file's compilation; for macros from another file wrap the require in
  `(eval-when-compile (require 'foo))` (or a plain top-level `require`).

### The five classic macro hazards

1. **Wrong time** — expansion can happen at compile time, long before and
   possibly instead of run time; never assume run-time context while
   expanding.
2. **Evaluation count** — the expansion must evaluate each argument form
   **exactly once, in order**, unless multiple/zero evaluation is the
   documented point. Substituting an argument into a `while` condition
   re-evaluates it every iteration. Fix: bind it once in a `let` in the
   expansion.
3. **Hygiene / variable capture** — a plain interned local (`max`) in the
   expansion captures the caller's same-named variable. Fix: uninterned
   symbols via `(gensym)` / `(make-symbol "max")`, or `cl-with-gensyms` /
   `cl-once-only` (cl-lib) which package the "bind each arg once to a
   gensym" pattern.
4. **Eval during expansion** — never `eval` argument forms inside the macro
   body; the caller's context isn't available and results differ
   interpreted vs compiled. Substitute the expression into the expansion.
5. **Repeated expansion** — interpreted code re-expands on each call,
   compiled code once; avoid side effects in expansion, and never mutate
   (`setcar`) objects that appear as quoted constants — compiled code shares
   them across calls. (Emacs 30 warns on constant mutation; Emacs 31 makes
   it genuinely break.)

### `declare` forms

- `(declare (indent SPEC))` — indentation: `nil`, `defun`, integer N
  (first N args distinguished), or a custom indent function.
- `(declare (debug SPEC))` — Edebug spec; use `(debug t)` when all args are
  evaluated normally. Declare it for every non-trivial macro or Edebug can't
  step into calls.
- Emacs 30 adds function `declare` forms: `(ftype (function (int) int))` and
  `important-return-value t`.
- Style: write a macro only when a function won't do (macros don't compose,
  can't be passed as values); keep the real logic in a helper function and
  let the macro be thin sugar; write the intended call site first.

## `eval-when-compile` vs `eval-and-compile`

- `eval-when-compile` — compute at compile time only; the value is inlined.
  For compile-time constants and requiring macro libraries.
- `eval-and-compile` — run at both compile and load time. Use when a
  definition is needed *to compile the rest of the file* and also at run
  time (e.g. a function called by a macro's expansion).
