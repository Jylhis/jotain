# Implementation State: 003-bleeding-edge-ide

**Last updated**: 2026-03-05
**Branch**: `003-bleeding-edge-ide`

## Status: Complete

All 26 tasks from `specs/003-bleeding-edge-ide/tasks.md` implemented.
Beads epic: `jotain-3xw` — closed.

## Completed Tasks

| Task | Status | What was done |
|------|--------|---------------|
| T001 | Done | Added `combobulate` trivialBuild to `nix/overlays/default.nix` |
| T002 | Done | Added `"minuet-ai" = "minuet"` to `packageNameMap` in `nix/lib/dependencies.nix` |
| T003 | Done | Added `pkgs.emacs-lsp-booster` and `pkgs.sqlite` to `cliTools` in `nix/lib/runtime-deps.nix` |
| T004 | Done | Removed `kind-icon` use-package block from `elisp/completion.el` |
| T005 | Done | Added `use-package gptel` with Anthropic/Gemini/Ollama backends, `C-c RET`/`C-c M-RET` bindings |
| T006 | Done | Added AI tool hierarchy comment documenting keybinding layout |
| T007 | Done | Replaced `direnv` with `envrc` for buffer-local env isolation |
| T008 | Done | Added `use-package inheritenv` after envrc |
| T009 | Done | Added `use-package apheleia` with `apheleia-global-mode` |
| T010 | Done | Added `use-package eglot-booster` after eglot |
| T011 | Done | Uncommented `flymake-show-diagnostics-at-end-of-line` |
| T012 | Done | Expanded inlay hints to ts-mode variants |
| T013 | Done | Added `use-package combobulate` with all tree-sitter mode hooks |
| T014 | Done | Removed `:disabled` from `treesit-fold` |
| T024 | Done | Added `apheleia-mode` as safe local variable |
| T025 | Done | Added dir-locals safety comment for `eglot-inlay-hints-mode` |
| T015 | Done | Added `use-package doom-modeline` |
| T016 | Done | Added `use-package indent-bars` with treesit support |
| T017 | Done | Added `use-package pulsar` with navigation pulse functions |
| T018 | Done | Added `use-package forge` with `emacsql-sqlite-builtin` |
| T019 | Done | Added `use-package git-timemachine` + `jotain-git-prefix-map` (`C-c g s`/`C-c g t`) |
| T020 | Done | Added `use-package minuet` with Claude backend, toggle `C-c a s` |
| T021 | Done | Added `use-package mcp` after gptel, manual activation only |
| T022 | Done | Created `tests/test-ai.el` with smoke tests for all new packages |
| T023 | Done | `just test-smoke` and `just test-fast` pass (29/29 tests) |
| T026 | Done | Audit passed — all commands have docstrings, descriptive M-x names |

## Test Results

- **Smoke tests**: All passed
- **Fast tests**: 29/29 passed (2.05s)
- **Startup time**: 0.017s (well under 3s SC-011 target)

## Key Design Decisions

- **gptel keybindings**: `C-c RET` (send), `C-c M-RET` (menu). claude-code-ide uses `C-c C-'`. No conflict.
- **minuet use-package name**: Use `minuet` (the elisp feature name), NOT `minuet-ai`.
- **envrc vs direnv**: `direnv` CLI stays in runtime-deps.nix. Only Emacs integration changed.
- **forge connector**: `emacsql-sqlite-builtin` for Emacs 30 built-in SQLite.
- **git keymap**: `C-c g` prefix via `defvar-keymap`. `C-c g s` → magit-status, `C-c g t` → git-timemachine.
- **combobulate rev**: `38773810` (2026-01-26).
- **treesit-fold**: Enabled by removing `:disabled`.
- **doom-modeline github**: Set to `nil` — forge handles GitHub.
