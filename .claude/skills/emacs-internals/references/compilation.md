# Byte compilation and native compilation

Sources: GNU Elisp Reference Manual (Emacs 30.2) Byte Compilation & Native
Compilation chapters; `src/comp.c`, `lisp/emacs-lisp/comp*.el`; Corallo et
al., "Bringing GNU Emacs to Native Code" (ELS'20). This repo runs Emacs 30
with native-comp on by default.

## Byte compilation

- Byte compilation turns Lisp into a portable bytecode object executed by
  the C interpreter loop in `bytecode.c`. `.elc` files hold these objects.
  Byte-code / interpreted functions are now `closure` objects (Emacs 30
  reworked the representation; the old "Byte-Code Objects" manual node is
  now "Closure Function Objects").
- **`eval-when-compile`** — evaluate at compile time only; result is
  substituted (use for compile-time constants and to `require` macros).
- **`eval-and-compile`** — evaluate at *both* compile and load time (use
  when a macro/function is needed to compile the rest of the file *and* at
  run time).
- Macros must be defined before their first *use* in compilation:
  `(eval-when-compile (require 'cl-lib))` before macro-heavy code. A plain
  top-level `require` is also run by the compiler and additionally silences
  undefined-function/variable warnings.
- Silence warnings correctly, never with a bogus autoload cookie:
  `(defvar foo)` for a variable defined elsewhere, `declare-function` for a
  function, `(with-suppressed-warnings ...)` for a specific known-safe call.
- `disassemble` shows the bytecode; `byte-compile-error-on-warn` turns
  warnings into hard errors — this repo's `just compile` uses it, so
  warning-clean code is mandatory.

## Native compilation (gccemacs)

### Pipeline

`elisp → (byte-compiler front end) → LAP → LIMPLE (SSA CFG IR) →
libgccjit IR → GCC optimizer → .eln shared object`

- Only lexically-scoped code is natively compiled; dynamic-scope files fall
  back to bytecode. `.eln` output is persistent and reloadable — not a JIT
  in the transient sense; expensive optimization amortizes across sessions.
- Native functions register as subrs (`subrp` is t); distinguish them with
  **`native-comp-function-p`** (Emacs 30 name; was `subr-native-elisp-p`).
- The `comp-passes` pipeline (Emacs 30): spill-lap, limplify, fwprop (×3),
  call-optim, ipa-pure, add-cstrs, tco, remove-type-hints, sanitizer,
  compute-function-types, final. Only `final` invokes GCC (in a subprocess).

### Speed levels (`native-comp-speed`, default 2)

| level | meaning |
|-------|---------|
| −1 | keep bytecode, no native code (still writes a bytecode `.eln`) |
| 0 | native, no optimization (`-O0`) |
| 1 | light optimizations, advanced frame layout |
| 2 | **default** — full optimization, semantics-preserving |
| 3 | maximum — **may change semantics**, use only when needed |

- Per-function override: `(declare (speed N))`; also `(declare (safety N))`.
- **Why 3 is dangerous** (and 2 is the default):
  - Intra-compilation-unit calls become direct/inlined, so **redefining or
    advising a function defined in the same file won't affect callers within
    that file** — they keep calling the old code.
  - Type hints (`comp-hint-fixnum`/`comp-hint-cons`) are *trusted* rather
    than checked; a wrong hint is undefined behavior / crash.
- `compilation-safety` (default 1): 0 lets mis-declared function types
  produce crashing code; 1 stays memory-safe. "Safe ≠ correct."

### Deferred / async (JIT) compilation

- `native-comp-jit-compilation` (default t): loading a `.elc` with no
  matching `.eln` spawns a batch subprocess to compile it, then swaps the
  native definitions in. Disable per-file with
  `native-comp-jit-compilation-deny-list` (regexps).
- Async subprocesses start from a **pristine environment**, so a missing
  `require` that was masked in your live session becomes an async-only
  warning/error — the most common native-comp failure mode. Control noise
  with `native-comp-async-report-warnings-errors` (t / `silent`).
- `native-comp-async-jobs-number` (0 = half the CPUs). Log buffers:
  `*Async-native-compile-log*`, `*Native-compile-Log*`.

### eln-cache layout and invalidation

- Search/write path: `native-comp-eln-load-path` (the `.eln` analogue of
  `load-path`; async output goes to the first writable entry). This repo
  redirects it to `var/eln-cache/` in `early-init.el` via
  `startup-redirect-eln-cache`.
- Per-ABI subdirectory `emacs-version "-" comp-abi-hash` (e.g.
  `30.1-abcdef12/`). `comp-abi-hash` is an 8-char MD5 over the Emacs
  version, build configuration, and **every preloaded primitive's
  name+arity** — so a different Emacs build, or any primitive signature
  change, silently invalidates the whole cache directory (old dirs are just
  never consulted). This is why a Nix Emacs rebuild recompiles everything.
- File name: `basename-<pathhash>-<contenthash>.eln` (both 8-char MD5, of
  the resolved absolute path and of the source contents). The content hash
  means editing a source invalidates its `.eln` and lets `dlopen` load the
  new one; for cache GC, all but the newest `basename-pathhash-*` are safe
  to delete.

### Trampolines and primitive redefinition

- At speed ≥ 2 native code calls primitives *directly* through a function
  pointer table, bypassing symbol function cells. So `fset`/advice on a
  **primitive** would be invisible to native callers.
- Fix: Emacs installs a natively-compiled **trampoline** shim in that
  primitive's table slot doing a normal symbol-based `funcall`. Trampolines
  are cached in the eln cache and reused across sessions.
- `native-comp-enable-subr-trampolines` (default t): t generates on demand;
  nil means **advice/redefinition of primitives is silently ignored** by
  native code unless the trampoline already exists; a string names a
  directory (session-only if unusual). `native-comp-never-optimize-functions`
  defaults to `(eval)`.
- Practical fallout: mocking/spying frameworks (buttercup, `cl-letf` on a
  primitive) behave differently under native-comp; sandboxed builds (Nix)
  may need trampolines pre-generated. When a test that stubs a *primitive*
  passes interpreted but fails compiled, suspect trampolines.

### Renames to remember

`subr-native-elisp-p` → `native-comp-function-p`;
`native-comp-deferred-compilation-deny-list` →
`native-comp-jit-compilation-deny-list`; `comp-speed`/`comp-debug` →
`native-comp-speed`/`native-comp-debug`; `comp.el` split into
`comp.el`+`comp-common.el`+`comp-run.el`+`comp-cstr.el` (Emacs 30).
