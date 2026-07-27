---
name: emacs-internals
description: Knowledge base on GNU Emacs internals (Emacs 30/31) — garbage collection, object representation, buffers and text, the redisplay engine, the command loop and keymaps, byte/native compilation, threads, and the build/dump pipeline. Use when explaining or debugging core Emacs behavior, tuning performance (GC, redisplay, startup), reasoning about native-comp/eln caches, or reviewing Elisp whose correctness depends on runtime semantics.
---

# Emacs internals knowledge base

Reference notes distilled from the GNU Emacs Lisp Reference Manual (Internals
appendix and related chapters), the `src/xdisp.c` design commentary, and the
native-compilation literature. Targets Emacs 30/31, which is what this repo
builds and runs (see `emacs.nix`).

## How to use this skill

Read only the reference file(s) relevant to the question — each is
self-contained:

| File | Covers |
|------|--------|
| `references/objects-and-gc.md` | Lisp_Object tagging, fixnums/bignums, symbols, strings, vectors; mark-and-sweep GC, `gc-cons-threshold`/`gc-cons-percentage`, stack allocation, `memory-report`, the MPS/igc branch |
| `references/buffers-and-text.md` | Gap buffer, markers, overlays (interval-tree since 29), text properties, buffer-local variables, character/byte positions |
| `references/redisplay.md` | Glyph matrices, desired vs. current matrix, the display iterator, redisplay optimizations and what defeats them, jit-lock/fontification, forcing redisplay |
| `references/command-loop-and-keymaps.md` | `read-key-sequence` → key lookup → command execution, `this-command`/`last-command`, pre/post-command hooks, event representation, keymap format and the active-keymap precedence order, quitting |
| `references/compilation.md` | Byte-code objects and `.elc` semantics, `eval-when-compile` vs `eval-and-compile`; native-comp pipeline (LAP → LIMPLE → libgccjit → `.eln`), speed levels, trampolines, eln-cache layout and invalidation |
| `references/threads-and-build.md` | Cooperative threads (global lock, yield points, mutexes/condvars); building and dumping (`temacs`, portable dumper `.pdmp`, purespace removal) |

## Repo-specific anchors

- GC startup tuning lives in `early-init.el` (`gc-cons-threshold` set to
  `most-positive-fixnum` during init) — see `references/objects-and-gc.md`
  for why and what to restore afterwards.
- Native-comp eln cache is redirected to `var/eln-cache/` in `early-init.el`;
  `just clean` removes it. The `igc` build variant (`just build-igc`) uses the
  MPS garbage collector.
- Startup/benchmarking: `just bench` wraps `require` with timing advice
  (`bench/early-init.el`).

When a claim here conflicts with the actual manual, trust the manual: use
`C-h S` (`info-lookup-symbol`) or the online GNU Elisp Reference Manual, and
fix the reference file in the same change.
