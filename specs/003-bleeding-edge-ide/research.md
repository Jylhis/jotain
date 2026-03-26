# Research: Bleeding Edge IDE

**Date**: 2026-03-04
**Feature**: 003-bleeding-edge-ide

## Workflow Context

**Claude Code is the primary AI coding tool** for this project. It provides multi-file agentic editing, repository-wide refactoring, and complex task execution from the terminal. Emacs AI packages serve complementary roles:

- **gptel**: Lightweight in-buffer AI for quick questions, region rewrites, and chat-with-context. Useful when the developer wants a fast answer without switching to the terminal.
- **minuet-ai** (P3, optional): Ghost-text completions during manual editing sessions when Claude Code isn't active. Off by default.
- **mcp.el** (P3, optional): MCP client for connecting LLMs to editor context and external tools. Strategic investment but requires explicit setup.

This framing means multi-file agentic editing packages (like aidermacs) are out of scope -- Claude Code already provides this capability with superior context awareness.

## Package Decisions

### AI Chat & LLM Backend: gptel

**Decision**: Use gptel v0.9.9.4 as the in-buffer AI complement to Claude Code.
**Rationale**: gptel supports 20+ backends (Anthropic, Gemini, Ollama, llama.cpp, Together.ai, Groq, DeepSeek, AWS Bedrock). It provides chat, inline rewrite (gptel-rewrite), and context management (gptel-add). It is the backend for minuet-ai (ghost text). Maintained by Karthik Chikmagalur; active development. As a lightweight in-buffer tool, it complements Claude Code's agentic capabilities without duplicating them.
**Alternatives considered**: ellama (Ollama-only, narrower scope), direct API clients (fragmented, no shared ecosystem).
**Nix**: `emacsPackages.gptel` -- available in nixpkgs.

### Ghost Text Completion: minuet-ai.el

**Decision**: Use minuet-ai.el for inline ghost-text suggestions (P3, optional enhancement).
**Rationale**: Only mature package providing ghost-text with Claude, Gemini, and Ollama. Supports chat-based completion (all providers) and FIM for compatible models (Codestral, DeepSeek). `minuet-auto-suggestion-mode` provides as-you-type suggestions with streaming. Submitted to GNU ELPA March 2025. Most useful during manual editing sessions when Claude Code isn't active.
**Alternatives considered**: copilot.el (locked to GitHub Copilot models, can't use Claude/Gemini/Ollama), gptel-autocomplete (experimental), codeium.el (proprietary models only).
**Nix**: `emacsPackages.minuet` -- name mismatch: use-package name `minuet-ai` maps to nixpkgs attr `minuet`. Requires manual entry in `packageNameMap`.

### Multi-File Agentic Editing: aidermacs

**Decision**: REMOVED -- Claude Code covers multi-file agentic editing.
**Rationale**: aidermacs wraps Aider CLI for multi-file AI editing with repository awareness. However, Claude Code provides superior multi-file agentic editing with deeper context awareness, and is the primary tool used for development on this project. Adding aidermacs would create a direct duplicate of Claude Code's core capability, violating YAGNI (Constitution VII). The developer would never prefer aidermacs over Claude Code for multi-file changes.
**Original consideration**: aidermacs v1.6, wrapping Aider CLI with ediff review, multiline prompts, automatic git commits, RepoMap.

### MCP Client: mcp.el

**Decision**: Use mcp.el for Model Context Protocol integration with gptel (P3, optional enhancement).
**Rationale**: Integrates with gptel via `gptel-mcp-connect`. Allows LLMs to access editor context and external tools through standardized protocol. The emerging "Emacs as agent runtime" paradigm makes this a strategic investment. Requires explicit connection -- no auto-connect.
**Alternatives considered**: No other Emacs MCP client library exists at maturity level.
**Nix**: `emacsPackages.mcp` -- available. Use-package name `mcp` matches nixpkgs attr.

### Async Formatting: apheleia

**Decision**: Use apheleia as the formatter dispatcher.
**Rationale**: Doom Emacs default. Runs formatters asynchronously after save using RCS patches (preserving cursor position). Automatically picks up formatters from `$PATH` -- works transparently with envrc buffer-local PATH. Supports 50+ formatters out of the box including nixfmt, rustfmt, goimports, prettier, shfmt, ruff, clang-format, black.
**Alternatives considered**: format-all (synchronous, blocking), eglot-format (language server dependent, no standalone formatter support), manual before-save-hook per mode (fragile).
**Nix**: `emacsPackages.apheleia` -- available.

### LSP Performance: eglot-booster

**Decision**: Use eglot-booster + emacs-lsp-booster binary for LSP acceleration.
**Rationale**: Transparent wrapper that intercepts LSP JSON and converts to Emacs bytecode before reaching the editor. Dramatically reduces GC overhead for large codebases. Drop-in enhancement requiring only `(eglot-booster-mode)`.
**Alternatives considered**: No alternative exists for this optimization. The Emacs 30 native JSON parser (8x faster) helps but eglot-booster provides additional gains.
**Nix**: `emacsPackages.eglot-booster` -- available. Runtime dependency: `pkgs.emacs-lsp-booster` (provides the binary).

### Structural Editing: combobulate

**Decision**: Use combobulate for AST-aware structural editing.
**Rationale**: Spiritual successor to paredit for non-Lisp languages. Provides AST-aware navigation, progressive region expansion, structural editing (splice, vanish, clone, drag nodes), bulk editing of matching nodes. Supports Python, TypeScript, TSX, JavaScript, JSX, HTML, CSS, YAML, JSON, Go, Rust, C, C++, TOML. By Mickey Petersen (Mastering Emacs). All commands via `C-c o o` transient menu.
**Alternatives considered**: No other package provides this level of tree-sitter structural editing. The built-in `treesit-thing-settings` provides basic navigation but not editing operations.
**Nix**: NOT in nixpkgs or MELPA. Requires custom Nix derivation using `trivialBuild` + `fetchFromGitHub` from `https://github.com/mickeynp/combobulate`. Add as overlay or inline in dependencies.nix.

### Environment Isolation: envrc + inheritenv

**Decision**: Replace `direnv` Emacs package with `envrc` + `inheritenv`.
**Rationale**: envrc sets environment variables buffer-locally (not global `process-environment`), so different buffers in different projects each have their own `$PATH`. This is critical for multi-project workflows with different LSP/formatter versions. inheritenv propagates buffer-local env to async subprocesses. The underlying `direnv` CLI and `.envrc` files are unchanged -- only the Emacs integration layer changes.
**Alternatives considered**: Keep direnv.el (but it mutates global state, breaks multi-project), buffer-env (less mature than envrc).
**Nix**: `emacsPackages.envrc` and `emacsPackages.inheritenv` -- both available.

### Modeline: doom-modeline

**Decision**: Use doom-modeline for the information-rich modeline.
**Rationale**: Shows git branch, file modification status, LSP status, flymake diagnostic counts, encoding, major mode. Works with doom-themes (already in jotain). Widely used, well-maintained, configurable.
**Alternatives considered**: Built-in `mode-line-format` (requires extensive custom elisp to replicate git branch, LSP status, and diagnostic count display — violates Principle VII by creating helpers for what doom-modeline provides out of the box), nano-modeline (minimal, lacks LSP/diagnostic indicators), mood-line (lightweight but fewer features), telephone-line (visual but complex), moody (interesting tab-like segments but less community adoption).
**Nix**: `emacsPackages.doom-modeline` -- available. Already has nerd-icons (dependency) in the project.

### Indent Guides: indent-bars

**Decision**: Use indent-bars for syntax-tree-aware indent visualization.
**Rationale**: Tree-sitter-powered indent guides that align to syntactic scope boundaries. Lightweight, integrates with treesit. Supports stipple-based rendering on GUI frames (smooth appearance).
**Alternatives considered**: highlight-indent-guides (regex-based, no tree-sitter awareness), built-in display-line-numbers (shows line numbers, not indentation structure).
**Nix**: `emacsPackages.indent-bars` -- available.

### Visual Pulse: pulsar

**Decision**: Use pulsar for visual feedback on navigation jumps.
**Rationale**: Pulses the current line briefly when jumping (goto-definition, search hit, bookmark, xref). By Protesilaos Stavrou (prolific Emacs maintainer). Minimal configuration, hooks into standard navigation commands automatically.
**Alternatives considered**: Built-in `pulse.el` (`pulse-momentary-highlight-one-line` — requires manual advice/hooks per navigation command; pulsar's `pulsar-global-mode` auto-hooks into dozens of commands without custom glue code, satisfying Principle VII), beacon (similar but less maintained), goggles (highlights edited regions, different purpose), nav-flash (older, less maintained).
**Nix**: `emacsPackages.pulsar` -- available.

### GitHub Integration: forge

**Decision**: Use forge for PR/issue management from Magit.
**Rationale**: By Jonas Bernoulli (Magit maintainer). Lists, creates, merges PRs; manages issues; adds labels and reviewers; checks out PR branches. GitHub support via GraphQL API is comprehensive. Integrates naturally into existing Magit workflow.
**Alternatives considered**: code-review (more limited scope), gh CLI via shell (loses Emacs integration).
**Nix**: `emacsPackages.forge` -- available. Runtime dependency: `pkgs.sqlite` (for emacsql-sqlite database).

### File History: git-timemachine

**Decision**: Use git-timemachine for stepping through file revisions.
**Rationale**: Simple, focused tool: invoke on any file, press n/p to step through git history, see content update in real time. Lightweight, well-maintained.
**Alternatives considered**: magit-log-buffer-file (shows log but doesn't replace buffer content), vc-annotate (blame-focused, not history stepping).
**Nix**: `emacsPackages.git-timemachine` -- available.

## Nix Packaging Summary

### Name Mismatches for dependencies.nix packageNameMap

| use-package name | nixpkgs attr | Action |
|-----------------|--------------|--------|
| `minuet-ai` | `minuet` | Add to packageNameMap |
| `combobulate` | (not in nixpkgs) | Custom derivation required |

### Runtime Binaries for runtime-deps.nix cliTools

| Binary | nixpkgs attr | Purpose |
|--------|-------------|---------|
| `emacs-lsp-booster` | `pkgs.emacs-lsp-booster` | eglot-booster companion binary |
| `sqlite` | `pkgs.sqlite` | forge emacsql-sqlite database |

### Combobulate Custom Derivation Strategy

Since combobulate is not on MELPA/ELPA and therefore not in the emacs-overlay, it needs a custom Emacs package derivation. Approach:

1. Add a Nix overlay that builds combobulate using `emacsPackages.trivialBuild` or `emacsPackages.melpaBuild` with source fetched via `fetchFromGitHub` from `mickeynp/combobulate`
2. Map `combobulate` in `packageNameMap` to the custom derivation name
3. The use-package declaration uses `:ensure t` as normal; Nix handles the rest

This follows the existing pattern in the project where all packages come through Nix, just with a custom source instead of MELPA.

## Activation Model (from Clarifications)

- **AI Chat (gptel)**: Always available on-demand. Lazy-loaded via use-package :defer. No startup cost. No automatic API calls. Complements Claude Code for quick in-buffer queries.
- **Ghost Text (minuet-ai)**: P3 optional enhancement. Requires explicit activation via `minuet-auto-suggestion-mode` toggle. Off by default.
- **MCP (mcp.el)**: P3 optional enhancement. Requires explicit connection via `gptel-mcp-connect`. No auto-connection.

## Credential Resolution (from Clarifications)

All AI backends resolve credentials in this order:
1. Environment variables (e.g., `ANTHROPIC_API_KEY`, `GEMINI_API_KEY`) -- checked first
2. auth-source (`~/.authinfo.gpg`) -- fallback for GPG-encrypted storage
3. If neither found: clear error message naming the expected variable and auth-source entry

This works naturally with gptel's built-in `:key` parameter which accepts both string values and functions. Use `(lambda () (or (getenv "ANTHROPIC_API_KEY") (auth-source-pick-first-password ...)))`.
