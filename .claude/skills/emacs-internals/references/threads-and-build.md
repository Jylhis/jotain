# Threads and the build/dump pipeline

Sources: GNU Elisp Reference Manual (Emacs 30.2) Threads chapter and
Building Emacs / Pure Storage nodes; emacs-devel on the igc/MPS branch.

## Threads (cooperative concurrency)

- Emacs threads are a **limited, mostly-cooperative** form of concurrency.
  All threads share one heap; effectively a **global lock (GIL-like)** —
  exactly one thread runs Lisp at a time. The manual states the
  cooperative-switching contract; "correct programs should not rely on
  cooperative threading."
- **Yield points** (the only places a switch can happen): explicit
  `thread-yield`; waiting for keyboard input or process output (e.g. inside
  `accept-process-output`); and the blocking ops `mutex-lock`,
  `condition-wait`, `thread-join`.
- **No preemption**: a CPU-bound Lisp loop that never hits a yield point
  starves every other thread and freezes the UI until it yields or blocks.
  GC stops all threads.
- **Per-thread state**: dynamic `let` bindings, the current buffer, and the
  match data are per-thread (the interpreter special-cases dynamic-binding
  unwinding — you cannot replicate it with `unwind-protect`). Global
  variable values and lexical closures are *shared*.

### API

- `make-thread FUNCTION &optional NAME` — starts with no let bindings;
  current buffer inherited from the creator. `current-thread`,
  `all-threads`, `main-thread`, `threadp`, `thread-live-p`, `thread-name`,
  `thread-join` (returns the function's value), `thread-yield`.
- `thread-signal THREAD ERROR-SYMBOL DATA` — deliver a signal into another
  thread (unblocks it if blocked); **signals to the main thread surface as
  a message, not a raised error**. `thread-last-error` is a single global
  slot overwritten by each abnormal exit.
- Mutexes are **recursive**: `make-mutex`, `mutex-lock`/`mutex-unlock`,
  and the safe wrapper `with-mutex`. Condition variables:
  `make-condition-variable`, `condition-wait` (releases the mutex while
  waiting; loop against a predicate — spurious wakeups happen),
  `condition-notify`.
- `list-threads` / the `*Threads*` buffer inspect live threads.
- Practical guidance: threads are for hiding latency (network, subprocess),
  not for parallel CPU work. For most config-level concurrency, prefer
  timers, process filters/sentinels, and `while-no-input` — they interleave
  with the command loop without the thread caveats.

## Building and dumping Emacs

- The build compiles the C core into a bootstrap executable **`temacs`**,
  which loads the preloaded Lisp libraries and then *dumps* its state so a
  normal startup doesn't re-load them.
- **Portable dumper (pdumper)** is the only supported method in Emacs 30:
  it writes a `.pdmp` file that the same executable maps at startup.
  `dump-emacs-portable` performs it; `pdumper-stats` reports whether the
  session was restored from a dump and the load time. The old **unexec**
  dumper is deprecated and compiled out by default (`dump-emacs`
  unavailable).
- A `.pdmp` is **not portable** — only the exact executable that produced
  it can load it. This matters for Nix: the Emacs binary and its dump are a
  matched pair in the same store path.
- **Delayed initialization**: preloaded code must defer install-path and
  environment computation to startup (dump time ≠ run time). Escape
  hatches: `custom-initialize-delay` as a defcustom `:initialize`, and
  `before-init-hook` / `after-pdump-load-hook`.
- **Pure storage** (`purecopy`, `purify-flag`, `pure-bytes-used`) was
  read-only shared memory for preloaded data. Already near-vestigial under
  pdump; **Emacs 31 (master) removed it entirely** — `purecopy` is a
  compatibility no-op. Ignore `purecopy` in modern code.

## The igc / MPS garbage-collector branch (status)

- **igc** replaces the classic stop-the-world mark-sweep collector with an
  **incremental, generational, moving** collector built on Ravenbrook's
  Memory Pool System (MPS). Build flag `./configure --with-mps`; packaged by
  emacs-overlay as `emacs-igc` (this repo's `just build-igc`).
- Authors: Gerd Möllmann, Pip Cet, Helmut Eller, with Eli Zaretskii and
  Stefan Kangas. History: `scratch/igc` (2024) → `feature/igc` →
  `feature/igc3` (2026).
- **Status: igc did NOT make Emacs 31** (feature freeze May 2026); work
  continues on `feature/igc3`, realistic target Emacs 32. Mainline 30/31
  keep the classic GC, so the `gc-cons-threshold` tuning in
  `objects-and-gc.md` applies to the builds this repo ships by default. Under
  igc, that tuning is largely irrelevant — GC pauses become small and
  incremental — but object pinning/moving assumptions interact with
  native-comp and the pdumper, which is why stabilization is slow.
- Nothing in the threads or compilation APIs changes on the igc branches;
  MPS addresses GC pause latency, not the single-threaded execution model.
