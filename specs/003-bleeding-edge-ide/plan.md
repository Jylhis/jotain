# Implementation Plan: Bleeding Edge IDE

**Branch**: `003-bleeding-edge-ide` | **Date**: 2026-03-04 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/003-bleeding-edge-ide/spec.md`

## Summary

Transform jotain from a solid modern Emacs configuration into a bleeding-edge AI-native IDE by adding: gptel-based AI integration (chat, inline rewrite, context management) as a lightweight complement to Claude Code, apheleia async formatting, eglot-booster LSP acceleration, combobulate structural editing, envrc buffer-local environment isolation, doom-modeline + indent-bars + pulsar UI polish, and forge + git-timemachine git workflow enhancements. Optional P3 enhancements include minuet-ai ghost text and mcp.el for MCP tool use. All packages installed via Nix; combobulate requires a custom derivation.

## Technical Context

**Language/Version**: Emacs Lisp (Emacs 30+, PGTK build), Nix (Flakes)
**Primary Dependencies**: gptel, apheleia, eglot-booster, combobulate, envrc, inheritenv, doom-modeline, indent-bars, pulsar, forge, git-timemachine
**Optional Dependencies (P3)**: minuet-ai, mcp.el
**Storage**: N/A (configuration files only)
**Testing**: ERT tests (`tests/`), NMT home-manager module tests (`nmt-tests/`), `just test-smoke` / `just test-fast`
**Target Platform**: Linux (primary), macOS, Android
**Project Type**: Emacs distribution / configuration framework
**Performance Goals**: Startup <= current + 500ms; LSP noticeably faster; ghost text < 500ms (when activated)
**Constraints**: Nix-only packages (Constitution I), modular architecture (Constitution II), no runtime MELPA/ELPA (Constitution I)
**Scale/Scope**: ~5 elisp modules modified, ~3 Nix files updated, 13 new packages + 2 runtime binaries

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Nix-Only Package Management | PASS | All 13 packages available via nixpkgs except combobulate (custom derivation needed). No runtime MELPA/ELPA. |
| II. Modular Elisp Architecture | PASS | New packages added to existing domain modules: ai.el, programming.el, ui.el, git.el. No new modules needed. |
| III. Reproducibility First | PASS | All runtime binaries (emacs-lsp-booster, sqlite) declared in runtime-deps.nix. |
| IV. Testing Discipline | PASS | Spec SC-007 requires passing existing test suites. New smoke tests for package loading. |
| V. Platform Portability | PASS | gptel/forge work cross-platform. vterm remains Linux-only (existing). Platform guards via platform.el. |
| VI. Performance by Design | PASS | Ghost text opt-in (no default API calls). Lazy loading via use-package :defer. SC-008 caps startup at +500ms. |
| VII. Simplicity & YAGNI | PASS | Every package addresses a concrete gap. Claude Code handles multi-file agentic editing; no duplicate tooling. |
| VIII. Discoverability | PASS | FR-024 requires docstrings and descriptive M-x names on all new commands. Combobulate uses transient menu (C-c o o). gptel uses gptel-menu transient. which-key built-in to Emacs 30. |
| IX. Built-in First | PASS | All third-party packages justified over built-in alternatives in research.md (pulsar vs pulse.el, doom-modeline vs mode-line-format). Eglot, flymake, treesit-fold are built-in. |
| Per-Project Configuration | PASS | FR-022/FR-023 require dir-locals support for apheleia-mode and eglot-inlay-hints-mode. envrc provides buffer-local env isolation. |

## Project Structure

### Documentation (this feature)

```text
specs/003-bleeding-edge-ide/
+-- plan.md              # This file
+-- spec.md              # Feature specification
+-- research.md          # Phase 0: package decisions and rationale
+-- data-model.md        # Phase 1: module structure and data flow
+-- checklists/
|   +-- requirements.md  # Spec quality checklist
+-- tasks.md             # Phase 2 output (created by /speckit.tasks)
```

### Source Code (repository root)

```text
elisp/
+-- ai.el                # MODIFY: gptel integration (Claude Code complement); minuet-ai and mcp.el as optional P3
+-- programming.el       # MODIFY: apheleia, eglot-booster, combobulate, envrc (replace direnv), inheritenv, treesit-fold enable
+-- ui.el                # MODIFY: doom-modeline, indent-bars, pulsar
+-- git.el               # MODIFY: forge, git-timemachine
+-- completion.el        # MODIFY: fix duplicate corfu icon formatters

nix/lib/
+-- dependencies.nix     # MODIFY: add packageNameMap entry for name mismatch (minuet-ai->minuet)
+-- runtime-deps.nix     # MODIFY: add emacs-lsp-booster, sqlite to cliTools

tests/
+-- test-ai.el           # NEW: smoke tests for AI package loading and configuration
+-- (existing tests)     # VERIFY: no regressions
```

**Structure Decision**: All changes fit within the existing modular architecture. No new elisp modules needed -- each change belongs to its domain module. The only structural additions are the combobulate custom Nix derivation and new test file.

## Complexity Tracking

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| Custom Nix derivation for combobulate | Not on MELPA/ELPA; only available from GitHub | Could skip combobulate, but it's the only mature AST-aware structural editor for tree-sitter; no alternative exists in nixpkgs |
