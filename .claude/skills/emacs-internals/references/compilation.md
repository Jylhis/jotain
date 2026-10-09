# Byte compilation and native compilation

Sources: GNU Elisp Reference Manual (Emacs 31.1) Byte Compilation & Native
Compilation chapters; `src/comp.c`, `lisp/emacs-lisp/comp*.el`; Corallo et
al., "Bringing GNU Emacs to Native Code" (ELS'20). This repo's default
build is Emacs 31.1 (emacs-overlay `unstable` variant) with
native-comp on by default; Emacs 30 remains the `mainline` escape hatch.

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
  warnings into hard errors — this repo's `elisp-compile` flake check
  (run via `just check`) uses it, so warning-clean code is mandatory.
- 31: `byte-compile-cond-use-jump-table` is obsolete; don't set it.

## Native compilation (gccemacs)

### Pipeline

`elisp → (byte-compiler front end) → LAP → LIMPLE (SSA CFG IR) →
libgccjit IR → GCC optimizer → .eln shared object`

- Needs a libgccjit-enabled build (the configure default since 30.1 when
  libgccjit works) **and** GCC + Binutils at run time to produce new
  `.eln`s. `native-comp-available-p` checks only the first part.
- Dynamic-binding functions are native-compiled too, but most middle-end
  optimizations (fwprop, call-optim, add-cstrs, tco) only run on
  lexically-scoped functions. `.eln` output is persistent and reloadable —
  not a JIT in the transient sense; expensive optimization amortizes
  across sessions.
- Native functions register as subrs (`subrp` is t); distinguish them with
  **`native-comp-function-p`** (Emacs 30 name; was `subr-native-elisp-p`)
  and C primitives with `primitive-function-p` (30). `cl-type-of` returns
  `native-comp-function`.
- `comp-passes` (31.1): `comp--spill-lap`, `limplify`, `fwprop`,
  `call-optim`, `ipa-pure`, `add-cstrs`, `fwprop`, `type-check-optim`,
  `tco`, `fwprop`, `remove-type-hints`, `sanitizer`,
  `compute-function-types`, `final` (all `comp--`-prefixed). Only `final`
  invokes GCC (in a subprocess).

### Speed levels (`native-comp-speed`, default 2)

| level | meaning |
|-------|---------|
| −1 | no native code: byte-compile only (still writes a bytecode `.eln`) |
| 0 | native, `-O0` |
| 1 | `-O1` |
| 2 | **default**, `-O2`: optimizations that preserve semantics |
| 3 | `-O3` plus optimizations that **may change semantics** |

- Per-function override: `(declare (speed N))`, including `(speed -1)` to
  keep one function as bytecode; also `(declare (safety N))`.
- What changes between 2 and 3 (`comp.el`):
  - At 2, direct intra-compilation-unit calls are used only for anonymous
    lambdas (which can't be redefined). At 3 they also apply to **named
    functions unique in the file**, so **redefining or advising such a
    function won't affect callers within that file**.
  - 3 also enables `ipa-pure` purity inference and self tail-call
    elimination, and drops the GC/quit check from loop latches, so a tight
    loop can't be interrupted with `C-g`.
  - Type hints (`comp-hint-fixnum`/`comp-hint-cons`) are already
    *trusted, not checked* from speed 2 up; a wrong hint is undefined
    behavior.
- `compilation-safety` (30, default 1): 0 lets mis-declared function types
  produce crashing code; 1 stays memory-safe. "Safe ≠ correct." The 30
  `(declare (ftype (function (ARG-TYPES…) RET-TYPE)))` form is exploited
  only at safety 0, where a wrong declaration can crash Emacs.

### Deferred / async (JIT) compilation

- `native-comp-jit-compilation` (default t): loading a `.elc` with no
  matching `.eln` spawns a batch subprocess to compile it, then swaps the
  native definitions in. Disable per-file with
  `native-comp-jit-compilation-deny-list` (regexps). It does not stop
  trampoline compilation, which `native-comp-enable-subr-trampolines`
  controls.
- Async subprocesses start from a **pristine environment**, so a missing
  `require` that was masked in your live session becomes an async-only
  warning/error — the most common native-comp failure mode.
  `native-comp-async-report-warnings-errors`: t (report and pop up),
  `silent` (log only), nil (drop). 30 added
  `native-comp-async-warnings-errors-kind` (default `important`: errors and
  important warnings only), and NEWS.30 recommends leaving reporting on
  now that the noise is filtered. This repo sets it to nil in
  `early-init.el`.
- `native-comp-async-jobs-number` (0 = half the CPUs; this repo caps it at
  3). Log buffers: `*Async-native-compile-log*`, `*Native-compile-Log*`.
  `native-comp-async-query-on-exit` (default nil) asks before killing
  running jobs.
- 31: `native-comp-async-on-battery-power` (default t). nil starts no
  *new* async jobs on battery (relies on `battery-status-function`); jobs
  skipped that way are not retried until restart. This repo sets nil.
- 31: `native-compile-directory` native-compiles every `.el` under a
  directory, skipping up-to-date ones.

### eln-cache layout and invalidation

- Emacs finds the `.elc` through `load-path`, then looks for a matching
  `.eln` in `native-comp-eln-load-path` (the `.eln` analogue of
  `load-path`; async output goes to the first writable entry). The swap
  needs the `.el`/`.el.gz` source to be findable, since the file hash
  comes from it. Relative entries resolve against `invocation-directory`;
  the last entry is the system eln dir.
- This repo redirects the writable head to `var/eln-cache/` in
  `early-init.el` via `startup-redirect-eln-cache` (which `setcar`s the
  list), and *appends* `JOTAIN_ELN_PATH`, the store-resident AOT `.eln`s
  for this config built by `nix/config-compiled.nix` when
  `services.jotain.nativeCompile.enable` is on. Append, never prepend: the
  head must stay the writable cache.
- Per-ABI subdirectory `comp-native-version-dir`, `emacs-version "-"
  comp-abi-hash` (e.g. `31.1-305e932a/`). `comp-abi-hash` is an 8-char
  hash over the Emacs version, build configuration, and **every preloaded
  primitive's name+arity**, so a different Emacs build or any primitive
  signature change silently invalidates the whole cache directory (old
  dirs are just never consulted). This is why a Nix Emacs rebuild
  recompiles everything. `native-compile-prune-cache` (30) deletes
  incompatible ABI dirs (never the system dir).
- File name: `basename-<pathhash>-<contenthash>.eln` (both 8-char MD5, of
  the resolved absolute path and of the source contents). The content hash
  means editing a source invalidates its `.eln` and lets `dlopen` load the
  new one; for cache GC, all but the newest `basename-pathhash-*` are safe
  to delete.

### Trampolines and primitive redefinition

- At speed ≥ 2 native code calls primitives *directly* through a function
  pointer table, bypassing symbol function cells. So `fset`/advice on a
  **primitive** would be invisible to native callers. (C callers never see
  advice on a primitive, native-comp or not.)
- Fix: Emacs installs a natively-compiled **trampoline** shim in that
  primitive's table slot doing a normal symbol-based `funcall`. Trampolines
  are cached in the eln cache and reused across sessions.
- `native-comp-enable-subr-trampolines` (default t): t generates on demand;
  nil means **advice/redefinition of primitives is silently ignored** by
  native code unless the trampoline already exists; a string names the
  directory to write them to (relative to `invocation-directory`, no
  version subdir). Trampolines written there, or to the
  `temporary-file-directory` fallback, are session-only because Emacs
  never looks them up again. `native-comp-never-optimize-functions`
  (default `(eval)`) lists primitives always called via `funcall`, which
  therefore need no trampoline.
- Practical fallout: mocking/spying frameworks (buttercup, `cl-letf` on a
  primitive) behave differently under native-comp; sandboxed builds (Nix)
  may need trampolines pre-generated. When a test that stubs a *primitive*
  passes interpreted but fails compiled, suspect trampolines.

### Renames to remember

`subr-native-elisp-p` → `native-comp-function-p`;
`native-comp-deferred-compilation-deny-list` →
`native-comp-jit-compilation-deny-list`; `comp-speed`/`comp-debug` →
`native-comp-speed`/`native-comp-debug`; 30 split `comp-common.el` and
`comp-run.el` out of `comp.el` (`comp-cstr.el` dates from 28); 31
prefixes the internal passes `comp--`.
