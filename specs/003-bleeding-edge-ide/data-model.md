# Data Model: Bleeding Edge IDE

**Date**: 2026-03-04
**Feature**: 003-bleeding-edge-ide

## Overview

This feature is a configuration project — no persistent data storage, no databases, no user data. The "data model" describes the module dependency graph and configuration data flow between elisp modules and Nix build files.

## Module Dependency Graph

```text
init.el (load orchestrator)
  │
  ├── ai.el ──────────────── NEW packages: gptel (P1), minuet-ai (P3), mcp.el (P3)
  │   ├── depends on: (none — self-contained AI module)
  │   └── provides: AI chat, rewrite, context mgmt; optional ghost text + MCP
  │
  ├── programming.el ─────── MODIFIED: +apheleia, +eglot-booster, +combobulate,
  │   │                                  +envrc (replaces direnv), +inheritenv,
  │   │                                  treesit-fold enabled
  │   ├── depends on: platform.el (platform guards for eat/vterm)
  │   └── provides: LSP, formatting, structural editing, env isolation, terminal
  │
  ├── ui.el ──────────────── MODIFIED: +doom-modeline, +indent-bars, +pulsar
  │   ├── depends on: (none)
  │   └── provides: modeline, indent visualization, visual feedback
  │
  ├── git.el ─────────────── MODIFIED: +forge, +git-timemachine
  │   ├── depends on: (none)
  │   └── provides: PR management, file history stepping
  │
  └── completion.el ──────── MODIFIED: fix duplicate icon formatters
      ├── depends on: (none)
      └── provides: corfu with single icon formatter
```

## Configuration Entities

### AI Backend (ai.el)

| Field | Type | Source | Notes |
|-------|------|--------|-------|
| provider | symbol | elisp config | One of: `anthropic`, `gemini`, `ollama` |
| api-key | string/fn | env var or auth-source | Resolved via `(or (getenv "KEY") (auth-source-pick ...))` |
| model | symbol | elisp config | Provider-specific model ID |
| stream | boolean | elisp config | Whether to stream responses |
| host | string | elisp config | For Ollama: `localhost:11434` |

### Formatter Association (programming.el via apheleia)

| Field | Type | Source | Notes |
|-------|------|--------|-------|
| major-mode | symbol | buffer | Determines formatter selection |
| formatter-cmd | list | apheleia-formatters alist | Command + args |
| mode-formatter | symbol | apheleia-mode-alist | Maps mode to formatter name |
| binary-path | string | $PATH (buffer-local via envrc) | Resolved per-project |

### Project Environment (programming.el via envrc)

| Field | Type | Source | Notes |
|-------|------|--------|-------|
| directory | string | buffer file path | Project root with `.envrc` |
| env-vars | alist | direnv CLI output | Includes PATH, tool locations |
| scope | buffer-local | envrc.el | Isolated per-buffer, not global |
| propagation | automatic | inheritenv.el | Extends to async subprocesses |

## Nix Build Data Flow

```text
elisp/*.el (use-package :ensure t declarations)
    │
    ▼
nix/lib/dependencies.nix
    ├── auto-extracts package names from use-package forms
    ├── packageNameMap: minuet-ai → minuet
    └── resolves to emacsPackages.* attributes
    │
    ▼
nix/lib/runtime-deps.nix
    ├── cliTools: +emacs-lsp-booster, +sqlite
    └── (direnv CLI remains — only Emacs package changes)
    │
    ▼
emacs.nix (symlinkJoin wraps everything into final derivation)
```

## State Transitions

### Ghost Text Mode (minuet-ai)

```
OFF (default) ──[user toggles mode]──► ACTIVE ──[user toggles mode]──► OFF
                                          │
                                     [typing pause]
                                          │
                                          ▼
                                    SUGGESTING ──[accept/dismiss]──► ACTIVE
                                          │
                                     [network error]
                                          │
                                          ▼
                                    ACTIVE (silent failure, no crash)
```

### Environment Resolution (envrc)

```
Buffer opened ──[envrc checks for .envrc]──► Found ──[direnv CLI evaluates]──► Buffer-local env set
                                              │                                      │
                                              ▼                                      ▼
                                         Not found ──► Global env (fallback)    LSP/formatters resolve
                                                                                from buffer-local PATH
```
