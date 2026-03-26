# Tasks: Bleeding Edge IDE
<!-- bd:epic:jotain-3xw -->

**Input**: Design documents from `/specs/003-bleeding-edge-ide/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md

**Tests**: Smoke tests for new package loading are included per plan.md (`tests/test-ai.el`).

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story. Phases are ordered by priority (P1 first, then P2 stories, then P3 optional enhancements).

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Phase 1: Setup (Nix Infrastructure)

**Purpose**: Add all new packages and runtime binaries to the Nix build system so use-package declarations resolve correctly.

- [ ] T001 Add combobulate custom derivation to `nix/overlays/default.nix` using `trivialBuild` + `fetchFromGitHub` from `mickeynp/combobulate`, following the existing `claude-code-ide` pattern <!-- bd:jotain-3xw.1 -->
- [ ] T002 [P] Add `minuet-ai` to `packageNameMap` in `nix/lib/dependencies.nix` mapping to nixpkgs attr `minuet` <!-- bd:jotain-3xw.2 -->
- [ ] T003 [P] Add `emacs-lsp-booster` and `sqlite` to `cliTools` in `nix/lib/runtime-deps.nix` with comments referencing which elisp packages use them <!-- bd:jotain-3xw.3 -->

**Checkpoint**: `nix flake check` passes. All new package names resolve to valid Nix derivations.

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Fix existing bug that must be resolved before UI story work begins.

**Warning**: No user story work should begin until this phase is complete.

- [ ] T004 Remove `kind-icon` use-package block from `elisp/completion.el` to resolve duplicate `corfu-margin-formatters` (FR-021); nerd-icons-corfu in `elisp/ui.el` is the canonical icon formatter <!-- bd:jotain-3xw.4 -->

**Checkpoint**: Corfu completion popup shows exactly one set of icons per candidate.

---

## Phase 3: User Story 1 -- AI-Assisted Editing (Priority: P1)

**Goal**: Developer can chat with LLMs, rewrite regions, and manage context -- all through a unified interface supporting Anthropic, Gemini, and Ollama. gptel complements Claude Code as a lightweight in-buffer AI tool.

**Independent Test**: Open any source file, invoke AI chat (`C-c RET`), select a region and invoke rewrite, verify gptel and claude-code-ide coexist without keybinding conflicts.

### Implementation for User Story 1

- [ ] T005 [US1] Configure gptel as lightweight in-buffer AI complement to Claude Code in `elisp/ai.el`: add `use-package gptel` with `:ensure t :defer t`, credential resolution via `(or (getenv "ANTHROPIC_API_KEY") (auth-source-pick-first-password ...))`, configure `gptel-model`, `gptel-stream`, `gptel-directives`, register additional backends with `gptel-make-gemini` and `gptel-make-ollama`, bind `C-c RET` to `gptel-send` and `C-c M-RET` to `gptel-menu` <!-- bd:jotain-3xw.5 -->
- [ ] T006 [US1] Design AI tool hierarchy and verify claude-code-ide compatibility with gptel in `elisp/ai.el`: ensure keybinding prefixes don't conflict (claude-code-ide uses its own prefix, gptel uses `C-c RET` / `C-c M-RET`), document the tool separation in comments <!-- bd:jotain-3xw.6 -->

**Checkpoint**: AI chat returns streaming response. gptel and claude-code-ide coexist with distinct keybindings.

---

## Phase 4: User Story 5 -- Per-Project Environment Isolation (Priority: P2)

**Goal**: Each buffer resolves tools (LSP servers, formatters) from its project's development environment. Switching between projects seamlessly switches tool resolution without global state mutation.

**Independent Test**: Open files from two projects with different tool versions in the same Emacs session, verify each buffer's `$PATH` and tool resolution is project-specific.

### Implementation for User Story 5

- [ ] T007 [US5] Replace direnv with envrc for buffer-local environment isolation in `elisp/programming.el`: remove the existing `direnv` use-package block, add `use-package envrc` with `:ensure t :demand t`, enable `envrc-global-mode`, suppress warnings similar to existing direnv config <!-- bd:jotain-3xw.7 -->
- [ ] T008 [US5] Add inheritenv for async subprocess environment propagation in `elisp/programming.el`: add `use-package inheritenv` with `:ensure t :demand t` after envrc -- this ensures compilation, formatting (apheleia), and linting subprocesses inherit buffer-local environment variables <!-- bd:jotain-3xw.8 -->

**Checkpoint**: Open a file in a project with `.envrc`, verify `(getenv "PATH")` in that buffer includes project-specific tools. Open a file outside any project, verify global fallback works.

---

## Phase 5: User Story 2 -- Automatic Code Formatting on Save (Priority: P2)

**Goal**: Files are automatically formatted on save using language-appropriate formatters, asynchronously, without cursor displacement. Formatters resolve from project environment when available.

**Independent Test**: Open a Go file with incorrect indentation, save it, confirm reformatting without cursor jump. Open a file type with no formatter, save normally without errors.

### Implementation for User Story 2

- [ ] T009 [US2] Configure apheleia async format-on-save in `elisp/programming.el`: add `use-package apheleia` with `:ensure t`, enable `apheleia-global-mode`, verify default formatter associations for Go (goimports), Nix (nixfmt), Rust (rustfmt), Python (ruff), JavaScript/TypeScript (prettier), Shell (shfmt), C/C++ (clang-format) -- apheleia resolves binaries from buffer-local `$PATH` which integrates with envrc (US5) <!-- bd:jotain-3xw.9 -->

**Checkpoint**: Save a `.go` file and observe async reformatting. Save an unknown file type without errors.

---

## Phase 6: User Story 3 -- Enhanced LSP Performance and Features (Priority: P2)

**Goal**: LSP interactions feel noticeably faster through protocol-level optimization. Inlay hints show type information inline. Diagnostic messages appear at line ends.

**Independent Test**: Open a large Go or TypeScript project, trigger completions and hover, verify responsiveness. Check inlay hints render. Check end-of-line diagnostics.

### Implementation for User Story 3

- [ ] T010 [US3] Configure eglot-booster for LSP acceleration in `elisp/programming.el`: add `use-package eglot-booster` with `:ensure t :after eglot`, enable `eglot-booster-mode` -- requires `emacs-lsp-booster` binary from runtime-deps (T003) <!-- bd:jotain-3xw.10 -->
- [ ] T011 [US3] Enable end-of-line diagnostic display in `elisp/programming.el`: uncomment and enable `(flymake-show-diagnostics-at-end-of-line t)` in the flymake `use-package` `:custom` block (remove the `; FIXME` comment) <!-- bd:jotain-3xw.11 -->
- [ ] T012 [US3] Expand inlay hints to tree-sitter mode variants in `elisp/programming.el`: update the `eglot-inlay-hints-mode` hook to include `go-ts-mode`, `rust-ts-mode`, `typescript-ts-mode`, `python-ts-mode` alongside existing base modes <!-- bd:jotain-3xw.12 -->

**Checkpoint**: LSP completions feel faster in a project with many files. Diagnostic summaries appear at end of affected lines. Inlay hints render in Go/Rust/TypeScript.

---

## Phase 7: User Story 4 -- Structural Code Editing (Priority: P2)

**Goal**: Developer navigates and manipulates code using AST structure -- drag arguments, splice expressions, expand selections along tree-sitter node boundaries.

**Independent Test**: Open a Python or TypeScript file, place cursor on a function argument, invoke drag-left/right, invoke progressive selection expansion.

### Implementation for User Story 4

- [ ] T013 [US4] Configure combobulate for AST-aware structural editing in `elisp/programming.el`: add `use-package combobulate` with `:ensure t`, hook into tree-sitter modes (`python-ts-mode`, `typescript-ts-mode`, `tsx-ts-mode`, `js-ts-mode`, `json-ts-mode`, `yaml-ts-mode`, `html-ts-mode`, `css-ts-mode`, `go-ts-mode`, `rust-ts-mode`, `c-ts-mode`, `c++-ts-mode`), configure `C-c o o` for the combobulate transient menu <!-- bd:jotain-3xw.13 -->
- [ ] T014 [US4] Enable treesit-fold for code folding in `elisp/programming.el`: remove `:disabled` from the existing `treesit-fold` use-package block, verify `global-treesit-fold-indicators-mode` activates on `after-init` with fringe indicators <!-- bd:jotain-3xw.14 -->

**Checkpoint**: `C-c o o` opens combobulate transient menu in a tree-sitter buffer. Drag-left/right swaps function arguments. Code blocks can be folded/unfolded with fringe indicators.

---

## Phase 7b: Per-Project Configuration (Constitution Compliance)

**Purpose**: Ensure new features support `.dir-locals.el` configuration per constitution Per-Project Configuration constraint.

- [ ] T024 [US2/US5] Declare `apheleia-mode` as a safe local variable in `elisp/programming.el` so projects can set `((prog-mode . ((apheleia-mode . nil))))` in `.dir-locals.el` to disable format-on-save per-project (FR-022) <!-- bd:jotain-3xw.15 -->
- [ ] T025 [US3] Verify `eglot-inlay-hints-mode` is configurable via `.dir-locals.el` in `elisp/programming.el` — mode is already buffer-local; ensure no global override prevents dir-locals from taking effect (FR-023) <!-- bd:jotain-3xw.16 -->

**Checkpoint**: A project with `((prog-mode . ((apheleia-mode . nil))))` in `.dir-locals.el` does not format on save. A project with `((go-ts-mode . ((eglot-inlay-hints-mode . nil))))` does not show inlay hints.

---

## Phase 8: User Story 6 -- Modern UI Polish (Priority: P2)

**Goal**: Rich modeline with git/LSP/diagnostic info, syntax-aware indent guides, and visual pulse on navigation jumps.

**Independent Test**: Open a file in a git repo, verify modeline shows branch name and diagnostic counts. Open nested code, verify indent guides render. Jump to a definition, observe visual pulse.

### Implementation for User Story 6

- [ ] T015 [US6] Configure doom-modeline as the information-rich modeline in `elisp/ui.el`: add `use-package doom-modeline` with `:ensure t :demand t`, enable `doom-modeline-mode`, configure to show git branch, LSP status, flymake diagnostic counts, set `doom-modeline-height` and `doom-modeline-bar-width` for PGTK rendering <!-- bd:jotain-3xw.17 -->
- [ ] T016 [US6] Configure indent-bars for syntax-tree-aware indent guides in `elisp/ui.el`: add `use-package indent-bars` with `:ensure t`, hook to `prog-mode`, enable tree-sitter integration via `indent-bars-treesit-support` and `indent-bars-treesit-scope` <!-- bd:jotain-3xw.18 -->
- [ ] T017 [US6] Configure pulsar for visual feedback on navigation jumps in `elisp/ui.el`: add `use-package pulsar` with `:ensure t :demand t`, enable `pulsar-global-mode`, configure `pulsar-pulse-functions` to include `xref-find-definitions`, `consult-line`, `imenu`, `bookmark-jump`, `recenter-top-bottom` <!-- bd:jotain-3xw.19 -->

**Checkpoint**: Modeline shows git branch + diagnostic counts. Indent guides visible in Python/TypeScript. Definition jump produces visible pulse.

---

## Phase 9: User Story 7 -- Enhanced Git Workflow (Priority: P2)

**Goal**: Developer manages PRs and issues from Magit, browses file history by stepping through revisions, and diffs render with enhanced clarity.

**Independent Test**: Open magit-status for a GitHub repo, navigate to PR section. Invoke git-timemachine on a file, step through revisions with n/p.

### Implementation for User Story 7

- [ ] T018 [US7] Configure forge for GitHub/GitLab PR and issue management in `elisp/git.el`: add `use-package forge` with `:ensure t :after magit`, configure `forge-database-connector` for emacsql-sqlite, document auth-source setup for GitHub token (`machine api.github.com login <user>^forge`) <!-- bd:jotain-3xw.20 -->
- [ ] T019 [US7] Configure git-timemachine for file history browsing in `elisp/git.el`: add `use-package git-timemachine` with `:ensure t :defer t`, bind `C-c g t` to `git-timemachine` <!-- bd:jotain-3xw.21 -->

**Checkpoint**: `M-x forge-pull` fetches PRs for a GitHub repo. `C-c g t` enters timemachine mode; n/p step through revisions.

---

## Phase 10: User Story 8 -- Optional In-Editor AI Enhancements (Priority: P3)

**Goal**: Ghost-text inline completions and MCP tool connections available as optional enhancements for manual editing sessions when Claude Code isn't active.

**Independent Test**: Toggle ghost-text mode and verify suggestions appear. Connect an MCP server and verify tool access.

### Implementation for User Story 8

- [ ] T020 [US8] Configure minuet-ai ghost text completion in `elisp/ai.el`: add `use-package minuet-ai` with `:ensure t :defer t`, set `minuet-provider` to use gptel backend, configure `minuet-auto-suggestion-debounce` for < 500ms response, bind a key to toggle `minuet-auto-suggestion-mode` (off by default per clarification) <!-- bd:jotain-3xw.22 -->
- [ ] T021 [US8] Configure mcp.el for MCP tool use in `elisp/ai.el`: add `use-package mcp` with `:ensure t :defer t :after gptel`, configure `mcp-server-command` defaults, document how to activate via `gptel-mcp-connect` (explicit activation only, no auto-connect per clarification) <!-- bd:jotain-3xw.23 -->

**Checkpoint**: Ghost text appears on explicit mode toggle. MCP can be connected manually.

---

## Phase 11: Polish & Cross-Cutting Concerns

**Purpose**: Verify all new packages load correctly, no regressions, startup time within budget.

- [ ] T022 Create smoke tests for new package loading in `tests/test-ai.el`: add ERT tests tagged `smoke` that verify `(require 'gptel)`, `(require 'apheleia)`, `(require 'eglot-booster)`, `(require 'combobulate)`, `(require 'envrc)`, `(require 'doom-modeline)`, `(require 'indent-bars)`, `(require 'pulsar)`, `(require 'forge)`, `(require 'git-timemachine)` all succeed; optionally test `(require 'minuet)` and `(require 'mcp)` (US8 packages -- present in Nix but off by default); register in `tests/test-all.el` <!-- bd:jotain-3xw.24 -->
- [ ] T023 Run `just test-smoke` and `just test-fast` to verify no regressions (SC-007), and measure startup time delta to confirm < 500ms increase (SC-008) <!-- bd:jotain-3xw.25 -->
- [ ] T026 Audit all new custom interactive commands across ai.el, programming.el, ui.el, git.el for docstrings and descriptive M-x names (FR-024, Principle VIII compliance) <!-- bd:jotain-3xw.26 -->

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies -- can start immediately
- **Foundational (Phase 2)**: Independent of Setup -- can run in parallel with Phase 1
- **User Stories (Phases 3--10)**: All depend on Phase 1 (Nix packages must resolve)
  - US1 (Phase 3) depends only on Phase 1
  - US5 (Phase 4) depends on Phase 1
  - US2 (Phase 5) depends on Phase 1; benefits from US5 (envrc) for project-local formatter resolution
  - US3 (Phase 6) depends on Phase 1 (specifically T003 for emacs-lsp-booster)
  - US4 (Phase 7) depends on Phase 1 (specifically T001 for combobulate derivation)
  - US6 (Phase 8) depends on Phase 1 and Phase 2 (T004 removes kind-icon before UI work)
  - US7 (Phase 9) depends on Phase 1 (specifically T003 for sqlite)
  - US8 (Phase 10) depends on Phase 1 and Phase 3 (gptel must be configured before minuet-ai/mcp.el)
- **Polish (Phase 11)**: Depends on all user story phases being complete

### User Story Independence

- **US1** (ai.el): Fully independent -- no cross-story dependencies
- **US2** (programming.el): Independent -- but benefits from US5 (envrc) for project-local formatter resolution
- **US3** (programming.el): Independent -- eglot-booster works regardless of other changes
- **US4** (programming.el): Independent -- combobulate only needs tree-sitter (already configured)
- **US5** (programming.el): Independent -- envrc replaces direnv cleanly
- **US6** (ui.el): Independent -- doom-modeline, indent-bars, pulsar have no cross-story dependencies
- **US7** (git.el): Independent -- forge and git-timemachine extend existing magit
- **US8** (ai.el): Depends on US1 -- gptel must be configured before minuet-ai and mcp.el

### File Conflict Map

Stories modifying the same file should be implemented sequentially:

| File | Stories |
|------|---------|
| `elisp/ai.el` | US1, US8 (sequential: US1 first) |
| `elisp/programming.el` | US2, US3, US4, US5 |
| `elisp/ui.el` | US6 only |
| `elisp/git.el` | US7 only |
| `elisp/completion.el` | Foundational (T004) only |

**Recommended sequence for programming.el stories**: US5 (envrc) -> US2 (apheleia) -> US3 (eglot-booster) -> US4 (combobulate/treesit-fold)

### Within Each User Story

- Configuration blocks before integration
- Core package before dependent packages (e.g., gptel before minuet-ai)
- Each story complete before moving to next priority

### Parallel Opportunities

After Phase 1 completes, the following can run in parallel (different files):

```
Stream A: US1 then US8 (ai.el -- sequential within stream)
Stream B: US5 -> US2 -> US3 -> US4 (programming.el -- sequential within stream)
Stream C: US6 (ui.el)
Stream D: US7 (git.el)
```

---

## Parallel Example: After Phase 1

```text
# These four streams can execute concurrently:

Stream A (ai.el):     T005 -> T006 .......... T020 -> T021
Stream B (prog.el):   T007 -> T008 -> T009 -> T010 -> T011 -> T012 -> T013 -> T014
Stream C (ui.el):     T015 -> T016 -> T017
Stream D (git.el):    T018 -> T019

# After all streams complete:
Final:                T022 -> T023
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001--T003)
2. Complete Phase 2: Foundational (T004)
3. Complete Phase 3: User Story 1 -- AI-Assisted Editing (T005--T006)
4. **STOP and VALIDATE**: Test AI features independently
5. This alone delivers the highest-value capability identified in the research

### Incremental Delivery

1. Setup + Foundational -> Nix infrastructure ready
2. US1 (AI) -> Test independently -> **MVP delivered**
3. US5 (envrc) -> US2 (apheleia) -> Test formatting with project environments
4. US3 (eglot-booster) -> Test LSP performance improvement
5. US4 (combobulate) -> Test structural editing
6. US6 (UI polish) -> Test modeline + indent guides + pulsar
7. US7 (Git workflow) -> Test forge + timemachine
8. US8 (Optional AI) -> Test ghost text + MCP (if desired)
9. Polish -> Run full test suite, verify startup time

### Single-Developer Strategy

Recommended execution order for one developer working sequentially:

1. T001 -> T002 -> T003 (Setup -- ~30 min)
2. T004 (Foundational -- ~10 min)
3. T005 -> T006 (US1 AI -- gptel + tool hierarchy)
4. T007 -> T008 (US5 envrc -- enables better US2)
5. T009 (US2 apheleia -- benefits from envrc)
6. T010 -> T011 -> T012 (US3 eglot-booster)
7. T013 -> T014 (US4 combobulate)
8. T015 -> T016 -> T017 (US6 UI polish)
9. T018 -> T019 (US7 git workflow)
10. T020 -> T021 (US8 optional AI enhancements)
11. T022 -> T023 (Polish -- test everything)

---

## Notes

- All use-package blocks use `:ensure t` to trigger Nix auto-extraction via `dependencies.nix`
- Packages with `:defer t` have zero startup cost until first invocation
- The direnv CLI binary stays in runtime-deps.nix -- only the Emacs integration (direnv->envrc) changes
- combobulate is the only package requiring a custom Nix derivation (not in nixpkgs/MELPA)
- Ghost text and MCP are P3 optional enhancements (US8) -- never auto-enabled
- Claude Code is the primary agentic tool; gptel is the lightweight in-buffer complement
