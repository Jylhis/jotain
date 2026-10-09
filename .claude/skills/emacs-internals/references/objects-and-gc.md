# Object representation and garbage collection

Source: GNU Elisp Reference Manual (Emacs 31.1), appendix E (Internals) and
ch. 2–3. Version notes flagged inline. The GC description covers the classic
in-tree collector, the only one in released Emacs 30 and 31.1; the MPS/igc
collector (see end) changes most of it.

## Tagged pointers (`Lisp_Object`)

- `Lisp_Object` is a tagged pointer. Every value is one of: integer, symbol,
  string, cons cell, float, or "vectorlike". The tag lives in a 3-bit
  bitfield (`enum Lisp_Type`); the remaining bits are the value.
- Fixnums are *immediate* (stored in the bits); everything else is a heap
  pointer.
- Width is normally pointer-width. The `--with-wide-int` build makes
  `Lisp_Object` 64-bit on 32-bit platforms to widen the fixnum range.
- Core structs in `src/lisp.h`: `struct Lisp_Cons`, `struct Lisp_String`,
  `struct Lisp_Vector`, `struct Lisp_Symbol`, `struct Lisp_Float`.
- Tag space is scarce, so everything else (buffers, windows, frames,
  processes, markers, overlays, hash tables, byte-code objects…) is a
  **pseudovector**: a `Lisp_Vectorlike` discriminated by `enum pvec_type`
  stored in the `union vectorlike_header` `size` field, which also encodes
  the count of `Lisp_Object` slots and trailing non-Lisp data size.

## Integers: fixnums and bignums

- Two kinds since Emacs 27: fixnums (immediate) and bignums (arbitrary
  precision, heap-allocated).
- Fixnum range is machine-dependent; guaranteed at least 30 bits
  (−2^29 … 2^29−1). Typical: 2^61−1 max on 64-bit. See
  `most-positive-fixnum` / `most-negative-fixnum`.
- An integer in fixnum range is *always* a fixnum; a bignum is never
  numerically equal to a fixnum. Consequence: **`eq` on integers is not
  reliable** — use `eql` or `=`.
- `integer-width` bounds bignum creation (values ≥ 2^n signal `range-error`).
- Characters are just integers in `0 … (max-char)`.

## Emacs 30 type-system additions

- Evaluating a `lambda` now yields an `interpreted-function` object, not a
  `(closure …)` list. `closure` is the parent type of both
  `interpreted-function` and `byte-code-function` (`closurep`,
  `interpreted-function-p`). Don't `car`/`cdr` into function objects.
  `cl-type-of`, type descriptors, and a documented type hierarchy were
  added.
- Objects are self-typing; each primitive type has a predicate. Use
  `cl-type-of` for the most specific type name.

## Garbage collection (classic mark-and-sweep)

### Allocation model

- Objects are segregated by type into blocks: small strings pack into 8 KB
  blocks, small vectors into 4 KB blocks; conses/symbols/markers get
  type-specific blocks with per-type free lists. Large vectors, long
  strings, and buffers are allocated individually.
- Emacs does **not** GC when a free list empties — it asks the OS for more
  memory and GCs only after `gc-cons-threshold` bytes have been consed.

### Algorithm

- **Conservative stack-scanning mark & sweep.** Roots: all symbols (values +
  function slots) plus anything that *looks like* a Lisp pointer on the C
  stack. Conservative scanning may overestimate reachability, so one sweep
  is not guaranteed to collect every dead object.
- Sweep: conses/symbols/markers go back on free lists; live strings are
  **compacted** into fewer 8 KB blocks; vector-block free areas are
  coalesced and indexed by size.
- GC can move string data and buffer text — C code must never hold pointers
  into them across Lisp evaluation (this constrains primitives, not Lisp).

### Triggering and tuning

- `gc-cons-threshold` — bytes of Lisp allocation since the last GC that allow
  the next one. Default `GC_DEFAULT_THRESHOLD`: **800,000 on 64-bit** and
  32-bit `--with-wide-int` (400,000 on plain 32-bit). Buffer contents don't
  count. Values below 1/10 of the default last only until the next GC.
- `gc-cons-percentage` — threshold as a fraction of the current heap.
  Default **0.1 interactive and while dumping, 1.0 under
  `--batch`/`--script`** (since 29).
- **Both criteria must be satisfied** for GC to run; the check is periodic
  and approximate, not per-allocation.
- The manual explicitly advises against keeping `gc-cons-threshold` large
  for prolonged periods: fewer but much longer pauses, higher memory
  pressure. The legitimate pattern is a *temporary* raise around a critical
  section — which is exactly what this repo does: `early-init.el` sets it to
  `most-positive-fixnum` (and `gc-cons-percentage` 0.6) during startup;
  `lisp/init-core.el` restores 16 MiB / 0.1 on `emacs-startup-hook`, pauses
  GC for the duration of minibuffer sessions, and runs an idle GC. Calling
  `garbage-collect` explicitly just before a latency-critical section
  guarantees no GC inside it (if it conses less than the threshold).
- `garbage-collect-maybe FACTOR` (28+) collects only when more than
  1/FACTOR of the threshold has been consed since the last GC: the cheaper
  choice for idle-timer GCs than an unconditional `garbage-collect`.
- Observability: `garbage-collect` (returns per-type `(NAME SIZE USED [FREE])`
  entries; the trailing `heap` entry needs malloc's `mallinfo`),
  `garbage-collect-heapsize` (31: the same list from the last GC, without
  collecting; exact only right after a GC), `gcs-done`, `gc-elapsed`,
  `garbage-collection-messages`, `post-gc-hook` (GC is inhibited while it runs
  — keep it cheap), `memory-report` (approximate `*Memory Report*` buffer),
  `memory-limit` (KiB of virtual memory), `memory-info` (reports the remote
  host when `default-directory` is remote), `memory-use-counts`, and the
  cumulative counters `cons-cells-consed`, `strings-consed`,
  `intervals-consed`, etc.

## Stack-allocated objects

- C-internal only: `AUTO_CONS`, `AUTO_STRING`, … allocate conses/strings
  with block lifetime on the C stack — never GC'd, never visible to Lisp.
  Stack strings are ASCII-only and often immutable. Debug misuse with
  `GC_CHECK_MARKED_OBJECTS`.

## Pure storage (historical) and the igc/MPS collector

- Pure storage: read-only, shared memory for preloaded-library data, filled
  only while `temacs` loads the preloaded files. Under pdump it was already
  nearly irrelevant; **Emacs 31.1 removed pure storage**: `purecopy` is an
  obsolete alias for `identity`, `pure-bytes-used` is always 0, and
  `purify-flag` survives only as a "preload phase" flag. The manual's Pure
  Storage node is gone. Treat `purecopy` in modern code as noise.
- `--disable-gc-mark-trace` (configure, 30+) drops the GC mark trace buffer
  for roughly 5% faster GC, at the cost of GC debugging aids.
- **igc/MPS**: the `feature/igc3` branch, not merged in 31.1 (packaged by
  emacs-overlay as `emacs-igc`; built here via `just build-igc`), replaces
  mark-sweep with Ravenbrook MPS — an incremental, mostly-copying,
  generational collector. Under igc, `gc-cons-threshold` semantics and
  `garbage-collect` output do not apply as written; GC pauses become small and
  incremental. Neither the 31.1 manual nor NEWS mentions igc/MPS (`(featurep
  'mps)` is nil on mainline builds).
