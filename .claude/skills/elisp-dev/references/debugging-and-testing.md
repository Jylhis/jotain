# Debugging, profiling, and ERT testing

Source: GNU Elisp Reference Manual (Debugging chapter, Profiling), ERT
manual, this repo's workflow (`just test`, `test/test-*.el`).

## Debugging

- **`M-x toggle-debug-on-error`** / `(setq debug-on-error t)` — enter the
  backtrace debugger on any error. `just debug` starts Emacs with
  `--debug-init` and `debug-on-error` for startup problems.
- `debug-on-entry` / `cancel-debug-on-entry` — break on entry to a function.
  `debug-on-variable-change` (add-variable-watcher) — break when a variable
  changes. `debug-on-signal` — catch even handled signals.
- **Edebug** — instrument a definition with `C-u C-M-x` in its body, then
  run it; step with `SPC`, inspect with `e`, set breakpoints with `b`.
  Macros need a correct `(declare (debug …))` spec for Edebug to step into
  their calls.
- `M-x trace-function` logs calls/returns to `*trace-output*`. For "what set
  this variable / why is this mode on" use `C-h v`, `C-h o`, and
  `find-variable`/`find-function` (`C-h f` then the source link).

## Profiling

- **`M-x profiler-start`** (`cpu`, `mem`, or both) → do the slow thing →
  `M-x profiler-report`; the tree shows where time/allocation goes. This is
  the right tool for "Emacs is slow/janky", including redisplay and
  fontification hotspots (look for `redisplay_internal`, `jit-lock`,
  font-lock functions).
- `benchmark-run` / `benchmark-progn` for microbenchmarks;
  `M-x emacs-init-time` and this repo's `just bench` (wraps `require` with
  timing advice via `bench/early-init.el`) for startup cost.
- `M-x memory-report` for a heap overview; `garbage-collect` and
  `gc-elapsed`/`gcs-done` for GC pressure (see the internals skill's
  `objects-and-gc.md`).

## ERT testing (this repo: `test/test-*.el`, run with `just test`)

- `(ert-deftest prefix-name () "Docstring." BODY)`. Test names are a single
  global namespace — **prefix every test with the module/package name**; the
  name should state the behavior, and the docstring's first line stands
  alone (shown on failure).
- Assertions: `(should FORM)`, `(should-not FORM)`,
  `(should-error FORM :type 'error-symbol)`. `should` records and *explains*
  its argument form on failure, so prefer `(should (equal actual expected))`
  over an opaque boolean — the explainer diffs the two.
- `skip-unless` for conditional skips; `:tags` for grouping;
  `:expected-result :failed` for known failures; `ert-info` adds context.
- **Environment hygiene**: each test must restore prior global state. Use
  `with-temp-buffer` for buffer work, `save-window-excursion` for windows,
  `let` for options (they're dynamic/special, so `let` restores them),
  `cl-letf` to stub functions
  (`(cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))) …)`),
  `unwind-protect` for temp files/processes.
  - Note: `cl-letf` on a **primitive** (subr) may not take effect under
    native compilation unless a trampoline exists — see the internals
    skill's `compilation.md`. Stub Lisp-level functions where possible.
- Factor side-effect-free cores out of interactive commands so the logic is
  testable without simulating the command loop. Keep fixtures as plain
  helper functions that take a body thunk.
- Run in batch (what CI does):
  `emacs -Q -batch -L lisp -l ert -l test/test-foo.el -f ert-run-tests-batch-and-exit`.
  `just test` wraps this; `test/` files load repo code with `-L lisp`.
- Lint gauntlet before considering Elisp done: `just check-elisp` (parens),
  `just compile` (byte-compile, warnings-as-errors), `M-x checkdoc`,
  and `relint` for regexps. Emacs 30's `package-isolate` runs a subprocess
  with only named packages for clean-environment reproduction.
