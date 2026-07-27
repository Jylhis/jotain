# Emacs & Emacs Lisp knowledge base

Curated, source-cited reference material for working on this repo's Emacs 30/31
configuration and its Nix build layer. It exists so that answers about Emacs
internals and Elisp practice come from the canonical manuals — not from memory.

## Where things live

The substance is packaged as two **skills** (auto-discoverable, loaded on
demand) plus this index:

- **`.claude/skills/emacs-internals/`** — how Emacs works underneath:
  garbage collection & object representation, the gap buffer / markers /
  overlays, the redisplay engine, the command loop & keymaps, byte & native
  compilation, threads, and the build/dump pipeline. Reach for it when
  explaining or debugging core behavior, tuning performance (GC, redisplay,
  startup), or reasoning about native-comp / eln caches.

- **`.claude/skills/elisp-dev/`** — how to write good Elisp here: naming and
  file conventions, lexical binding, macro hygiene, hooks vs. advice,
  `defcustom`/`setopt`, `use-package` patterns, modern libraries
  (pcase/seq/map/cl-lib/rx), debugging & ERT testing, and an Emacs 30/31
  changes digest. Reach for it when writing or reviewing any Elisp.

Each skill has a `SKILL.md` entry point with a table routing to
self-contained `references/*.md` files. Read only the reference you need.

## Primary sources

Everything traces back to these — cite them when correcting the notes:

- **GNU Emacs Lisp Reference Manual** (Emacs 30.2): the Internals appendix
  (E), plus the Variables, Macros, Customization, Loading, Modes, Display,
  Command Loop, Keymaps, Byte/Native Compilation, and Threads chapters.
  Read in Emacs with `C-h i m Elisp RET`, or `C-h S` (`info-lookup-symbol`)
  on any symbol.
- **GNU Emacs Manual** (`C-h r`) for user-facing behavior.
- **Source commentary**: `src/xdisp.c` (redisplay design), `src/comp.c` +
  `lisp/emacs-lisp/comp*.el` (native compilation), `src/buffer.h`.
- **`etc/NEWS.30` / `etc/NEWS.31`** for version-specific changes.
- bbatsov/emacs-lisp-style-guide and the use-package manual for style.

## How to keep it honest

These notes target Emacs 30/31 and carry version flags (e.g. igc/MPS is not
in mainline 30/31; pure space is gone in 31). When a note conflicts with the
installed Emacs, **trust the running Emacs and its manual**: verify with
`C-h f`/`C-h v`/`C-h S`, then fix the reference file in the same change so
the knowledge base doesn't drift.
