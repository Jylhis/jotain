# Feature Specification: Bleeding Edge IDE

**Feature Branch**: `003-bleeding-edge-ide`
**Created**: 2026-03-04
**Status**: Draft
**Input**: User description: "Based on 2026 research, bring jotain Emacs configuration to the bleeding edge with AI agents, modern completion, enhanced LSP, structural editing, and polished developer experience"

**Workflow Context**: Claude Code is the primary AI coding tool for this project. Emacs AI packages (gptel, minuet-ai, mcp.el) serve as lightweight in-editor complements for quick queries and region rewrites — not as replacements for Claude Code's multi-file agentic capabilities.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - AI-Assisted Editing (Priority: P1)

A developer opens a source file and wants lightweight AI assistance without leaving the buffer. They can chat with an LLM about the current file or project, rewrite selected regions with natural language instructions, and add context from files, buffers, or directories. These in-buffer AI features complement Claude Code (the primary agentic tool) by handling quick questions and small edits that don't warrant switching to the terminal. All AI features work with multiple model providers (Anthropic Claude, Google Gemini, local Ollama models) through a unified interface.

**Why this priority**: While Claude Code handles multi-file agentic editing, there is no lightweight in-buffer AI for quick questions, region rewrites, or chat-with-context. gptel fills this gap as a fast, provider-agnostic complement. The current ai.el module is essentially a stub with only claude-code-ide configured.

**Independent Test**: Can be fully tested by opening any source file, invoking AI chat, performing an inline rewrite on a selected region, and verifying gptel and claude-code-ide coexist without keybinding conflicts.

**Acceptance Scenarios**:

1. **Given** a developer is editing a source file, **When** they invoke the AI chat command, **Then** they can converse with an LLM about the code with the current buffer automatically included as context
2. **Given** a developer selects a code region and invokes rewrite, **When** they type a natural language instruction, **Then** the LLM rewrites the region and presents a diff for review before applying
3. **Given** no API keys are configured in environment variables or auth-source, **When** a developer invokes any AI feature, **Then** they receive a clear message about which credentials are needed and both supported storage methods
4. **Given** claude-code-ide and gptel are both loaded, **When** the developer checks keybindings, **Then** there are no conflicts — each tool has a distinct keybinding prefix

---

### User Story 2 - Automatic Code Formatting on Save (Priority: P2)

A developer saves a file and the appropriate language-specific formatter runs automatically, asynchronously, without blocking the editor or moving the cursor. Different projects can use different formatters for the same language, resolved through the project's environment. The formatting happens transparently — the developer simply saves and the file is properly formatted.

**Why this priority**: The current configuration has no formatter-on-save infrastructure. Asynchronous formatting is a foundational IDE capability that eliminates an entire class of code review friction.

**Independent Test**: Can be tested by opening files in different languages, making formatting-violating edits, saving, and confirming the file is reformatted without cursor displacement.

**Acceptance Scenarios**:

1. **Given** a developer edits a Go file with incorrect indentation, **When** they save, **Then** the file is reformatted according to Go standards without cursor movement or editor blocking
2. **Given** a developer works in a project with a specific Nix formatter preference, **When** they save a `.nix` file, **Then** the project-local formatter is used (not the global default)
3. **Given** a developer saves a file type with no formatter configured, **When** the save completes, **Then** no error occurs and the file saves normally
4. **Given** the formatter binary is not available in PATH, **When** the developer saves, **Then** the file saves without formatting and a non-intrusive warning is shown

---

### User Story 3 - Enhanced LSP Performance and Features (Priority: P2)

A developer working in a large codebase experiences fast, responsive LSP interactions. Language server responses arrive noticeably faster through protocol-level optimization. Inlay hints show type information inline. The developer can see diagnostic information at the end of affected lines for quick scanning.

**Why this priority**: LSP is the backbone of code intelligence. Performance optimization and richer display (inlay hints, end-of-line diagnostics) are low-effort, high-impact improvements that benefit every language.

**Independent Test**: Can be tested by opening a large project, verifying completions feel responsive, and confirming inlay hints render correctly.

**Acceptance Scenarios**:

1. **Given** a developer opens a large project, **When** they trigger completions or hover, **Then** responses arrive noticeably faster than the current unoptimized baseline
2. **Given** a developer enables inlay hints for a supported language, **When** they view a function, **Then** parameter names and inferred types appear inline without obscuring the code
3. **Given** a file has diagnostic errors, **When** the developer scans the buffer, **Then** truncated diagnostic messages appear at the end of affected lines for quick identification

---

### User Story 4 - Structural Code Editing (Priority: P2)

A developer navigates and manipulates code using its syntactic structure rather than text. They can move function arguments, swap sibling nodes, splice or wrap expressions, and progressively expand selections along AST boundaries. These operations work across all tree-sitter-supported languages with consistent keybindings.

**Why this priority**: Structural editing complements code review of Claude Code output — when reviewing AI-generated changes, AST-aware navigation and manipulation make it fast to reorganize, validate, and adjust code structurally rather than textually. The tree-sitter infrastructure is already in place with all grammars installed.

**Independent Test**: Can be tested by opening a Python/TypeScript file and performing node-level operations (drag argument left, splice list, expand selection to enclosing defun).

**Acceptance Scenarios**:

1. **Given** a developer's cursor is on a function argument, **When** they invoke "drag left/right", **Then** the argument swaps position with its sibling while preserving syntax
2. **Given** a developer selects a nested expression, **When** they invoke "splice", **Then** the expression replaces its parent node
3. **Given** a developer invokes progressive selection expansion, **When** they repeat the command, **Then** the selection grows to encompass increasingly larger syntactic units (expression -> statement -> block -> function)
4. **Given** a developer is in a tree-sitter mode with folding definitions, **When** they invoke fold, **Then** the code block collapses with a fringe indicator showing it can be expanded

---

### User Story 5 - Per-Project Environment Isolation (Priority: P2)

A developer working across multiple projects simultaneously has each buffer automatically resolve its language servers, formatters, and tools from the project's own development environment. Switching between a Go project and a Rust project in the same Emacs session seamlessly switches all tool resolution without manual intervention or global state mutation.

**Why this priority**: Environment isolation is foundational for multi-project work. The current direnv integration mutates global process environment, which breaks multi-project workflows. Buffer-local environment variables ensure correct tool resolution per-buffer, which is essential for the Nix devShell model. This also enables correct formatter resolution for US2.

**Independent Test**: Can be tested by opening files from two different projects with different tool versions and verifying each buffer's LSP and formatter resolve to the correct project-specific binary.

**Acceptance Scenarios**:

1. **Given** two open projects with different tool versions, **When** the developer switches between buffers, **Then** each buffer uses its project's tool versions (not global defaults)
2. **Given** a project has a devShell with specific tools, **When** a file from that project is opened, **Then** async subprocesses (compilation, formatting, linting) inherit the project's environment
3. **Given** a project directory has no `.envrc`, **When** a file is opened, **Then** the system falls back to global tool resolution without errors

---

### User Story 6 - Modern UI Polish (Priority: P2)

A developer's editing experience includes a rich, informative modeline showing git branch, LSP status, and diagnostic counts. Code structure is visualized through indent guides tied to the syntax tree. Visual feedback accompanies navigation jumps and editing operations, making it easy to track cursor position after large movements.

**Why this priority**: UI state awareness is critical during code review of Claude Code changes. A good modeline shows git branch, modification status, and diagnostic counts at a glance — essential context when reviewing diffs and verifying AI-generated code. Indent guides aid comprehension of deeply nested code. Visual feedback on jumps prevents disorientation.

**Independent Test**: Can be tested by opening a file and verifying the modeline shows project name, git branch, and diagnostic counts; indent guides render in nested code; and jumping to a definition produces a brief visual pulse at the landing point.

**Acceptance Scenarios**:

1. **Given** a developer is editing in a git repository, **When** they look at the modeline, **Then** they see the current git branch, file modification status, and active diagnostic error/warning counts
2. **Given** a developer opens a deeply nested code file, **When** they view the buffer, **Then** vertical indent guides aligned to syntactic scope boundaries are visible
3. **Given** a developer jumps to a definition in another file, **When** the cursor lands, **Then** a brief visual pulse highlights the target line

---

### User Story 7 - Enhanced Git Workflow (Priority: P2)

A developer manages pull requests, issues, and code review directly from within the Git interface. They can browse file history by stepping through past versions, and diffs render with enhanced visual clarity. The full code review cycle — from browsing PRs to leaving comments — happens without leaving the editor.

**Why this priority**: Git is the primary review interface for Claude Code changes. Every agentic coding session produces commits that need review, diff inspection, and potential PR management. Forge and git-timemachine complete the review loop: inspect history, browse PRs, and manage issues without leaving Emacs.

**Independent Test**: Can be tested by listing pull requests from a GitHub repository, stepping through a file's git history, and verifying diffs render with enhanced formatting.

**Acceptance Scenarios**:

1. **Given** a developer opens the Git status buffer for a GitHub-hosted repo, **When** they navigate to the pull request section, **Then** they see a list of open PRs with titles, authors, and review status
2. **Given** a developer wants to see how a file changed over time, **When** they invoke history browsing on a file, **Then** they can step forward and backward through git revisions of that file with the content updating in real time
3. **Given** a developer views a diff, **When** the diff renders, **Then** it uses syntax-aware formatting with word-level change highlighting

---

### User Story 8 - Optional In-Editor AI Enhancements (Priority: P3)

A developer doing manual editing (when Claude Code isn't active) can optionally enable ghost-text inline completions and MCP tool connections for richer AI context. Ghost text provides as-you-type suggestions from multiple providers during manual coding sessions. MCP allows LLMs to access editor context and external tools through a standardized protocol. Both features are off by default and require explicit activation.

**Why this priority**: These are optional enhancements for manual editing sessions. Ghost text overlaps with Claude Code's capabilities and is most useful when coding without an agentic tool. MCP is a strategic investment in the "Emacs as agent runtime" paradigm but requires explicit connection setup.

**Independent Test**: Can be tested by toggling ghost-text mode and verifying suggestions appear, and by connecting an MCP server and verifying tool access from gptel.

**Acceptance Scenarios**:

1. **Given** a developer has activated ghost-text mode, **When** they pause briefly while typing, **Then** inline ghost-text suggestions appear that can be accepted with a single keystroke
2. **Given** a developer invokes MCP connection, **When** a server is configured, **Then** the LLM can access editor context and external tools through the MCP protocol
3. **Given** ghost-text mode is not explicitly activated, **When** the developer types normally, **Then** no ghost-text suggestions appear and no API calls are made

---

### Edge Cases

- What happens when an AI model provider is unreachable (network failure, rate limit)?
  - Ghost text stops suggesting; chat shows a clear error; no editor crash or hang
- What happens when a formatter produces invalid output?
  - The original file content is preserved; an error is logged; the developer is notified
- What happens when tree-sitter grammars are missing for a language?
  - Structural editing commands gracefully degrade to text-based equivalents
- What happens when envrc encounters a broken `.envrc` file?
  - The buffer shows a warning but remains functional with global environment
- What happens when gptel and claude-code-ide have conflicting keybindings?
  - Each tool uses a distinct keybinding prefix; conflicts are resolved at configuration time

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST provide a unified AI chat interface that supports multiple LLM providers (at minimum: Anthropic Claude, Google Gemini, local Ollama instances) with Anthropic Claude as the default provider
- **FR-002**: System MUST provide region-based AI rewrite with diff preview before applying changes
- **FR-003**: System MUST provide AI context inclusion from files, buffers, directories, and regions through a consistent interface
- **FR-004**: System MUST ensure gptel and claude-code-ide coexist with clear keybinding separation and no conflicts
- **FR-005**: System MUST provide inline ghost-text code completions (P3/US8) that require explicit activation (per-session or per-buffer mode toggle), dismissible with a single keystroke when active
- **FR-006**: System MUST provide MCP (Model Context Protocol) client capability (P3/US8), requiring explicit connection activation, allowing LLMs to access editor context and external tools
- **FR-007**: System MUST automatically format files on save using language-appropriate formatters, asynchronously and without cursor displacement. Formatters MUST match the project's treefmt configuration so on-save and `just format` produce identical results
- **FR-008**: System MUST resolve formatters from the project's development environment when available, falling back to globally installed formatters
- **FR-009**: System MUST accelerate LSP protocol handling through protocol-level optimization of server communication
- **FR-010**: System MUST display inlay hints (parameter names, inferred types) by default in supported tree-sitter modes (Go, Rust, TypeScript, Python), with per-buffer toggle available
- **FR-011**: System MUST provide AST-aware structural editing operations (drag, splice, clone, wrap, expand-selection) for all tree-sitter-supported languages
- **FR-012**: System MUST enable code folding via tree-sitter syntax nodes with fringe indicators
- **FR-013**: System MUST isolate per-project environment variables at the buffer level, not globally
- **FR-014**: System MUST propagate buffer-local environment variables to async subprocesses (compilation, formatting, linting)
- **FR-015**: System MUST display a rich modeline with git branch, LSP status, and diagnostic counts
- **FR-016**: System MUST render syntax-tree-aware indent guides in programming modes
- **FR-017**: System MUST provide visual pulse feedback on navigation jumps
- **FR-018**: System MUST support GitHub/GitLab pull request and issue management from within the Git interface
- **FR-019**: System MUST provide file-level git history browsing with revision stepping
- **FR-020**: All new capabilities MUST be installable via Nix with required runtime binaries declared in the Nix build system
- **FR-021**: System MUST resolve the duplicate icon formatter issue in the current corfu completion popup
- **FR-022**: Format-on-save (apheleia-mode) MUST be configurable per-project via `.dir-locals.el`, allowing projects to disable or enable formatting independently
- **FR-023**: Inlay hints (eglot-inlay-hints-mode) MUST be configurable per-project via `.dir-locals.el`, allowing projects to disable hints independently of the global default
- **FR-024**: All new custom interactive commands MUST have docstrings and descriptive names suitable for `M-x` discovery via the completion stack (Principle VIII compliance)

### Key Entities

- **AI Backend**: A configured LLM provider with authentication credentials (resolved first from environment variables, then from auth-source), model selection, and streaming preferences
- **Formatter**: A language-specific code formatting tool resolved from project or global environment, invoked asynchronously on save
- **Project Environment**: A set of buffer-local environment variables derived from a project's development shell, providing tool resolution per-buffer
- **Syntax Node**: A tree-sitter AST element that structural editing commands operate on (argument, expression, statement, function, class)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developer can send an AI chat message and receive a streaming response within 3 seconds of invocation (given network connectivity)
- **SC-002**: Ghost-text inline suggestions appear within 500ms of typing pause without blocking editor input (when explicitly activated)
- **SC-003**: File formatting on save completes without visible cursor jump or editor freeze, even for files over 1000 lines
- **SC-004**: LSP completions and hover information feel noticeably faster in large projects compared to the current unoptimized baseline
- **SC-005**: Structural editing operations (drag, splice, expand) execute instantly on files up to 5000 lines
- **SC-006**: Switching between buffers in different projects resolves the correct project-specific tools without perceptible delay
- **SC-007**: All new features pass the existing `just test-smoke` and `just test-fast` test suites without regression
- **SC-008**: Emacs startup time does not increase by more than 500ms after all new packages are loaded
- **SC-009**: The git interface shows pull request listings within 5 seconds of invocation for repositories with up to 100 open PRs
- **SC-010**: At least 10 languages have fully working format-on-save with language-appropriate formatters

## Clarifications

### Session 2026-03-04

- Q: Should AI features (ghost text, MCP, chat) be enabled by default or require opt-in? -> A: Chat is always available on-demand; ghost text and MCP require explicit activation per-session or per-buffer.
- Q: How should AI API credentials be stored and accessed? -> A: Environment variables as primary (e.g., `ANTHROPIC_API_KEY`), auth-source (`~/.authinfo.gpg`) as fallback for users who prefer GPG-encrypted storage.
- Q: Should direnv->envrc migration be a clean replacement or parallel support? -> A: Clean replacement -- remove direnv package, add envrc; same `.envrc` files work transparently.
- Q: What is the primary AI coding tool? -> A: Claude Code is the primary agentic tool for multi-file editing and complex tasks. gptel provides lightweight in-buffer AI for quick queries and region rewrites that don't warrant switching to the terminal.
- Q: Which AI provider/model should be the gptel default when multiple are configured? -> A: Anthropic Claude (claude-sonnet-4-20250514). API keys are guaranteed present since Claude Code is the primary tool; minimizes credential setup friction. Other providers (Gemini, Ollama) available as switchable alternatives.
- Q: Should forge support GitHub only or also GitLab? -> A: Both — GitHub and GitLab, user-configurable. GitHub is the personal default; GitLab needed for work. Forge supports both via its backend system.
- Q: Should apheleia format Emacs Lisp files on save? -> A: Yes — enable apheleia for elisp, but ensure it integrates with the existing treefmt configuration used by `just format`. Apheleia should use the same formatters treefmt would invoke, so on-save and batch formatting produce identical results.
- Q: Should inlay hints be on by default or require opt-in? -> A: On by default in all supported tree-sitter modes (Go, Rust, TypeScript, Python). User can toggle off per-buffer.
- Q: Should pulsar be used for visual pulse, or the built-in pulse.el? -> A: Use pulsar. Built-in pulse.el requires manual advice/hooks per navigation command; pulsar's global-mode auto-hooks into dozens of commands (xref, consult, imenu, bookmark) without custom glue code. Justified per Principle IX: pulsar provides clear value over the built-in.
- Q: Should new features (format-on-save, inlay hints) support per-project configuration via .dir-locals.el? -> A: Yes. Add FRs requiring .dir-locals.el support for format-on-save toggle (apheleia-mode) and inlay hints toggle (eglot-inlay-hints-mode). Both are buffer-local minor modes natively. Required by Constitution Per-Project Configuration constraint.
- Q: Should the spec add an FR for Principle VIII discoverability (docstrings, M-x names)? -> A: Yes. Add one cross-cutting FR requiring all new custom interactive commands to have docstrings and descriptive M-x-friendly names. Constitutional compliance via single FR rather than per-feature.
- Q: Should doom-modeline justify its use over built-in mode-line-format per Principle IX? -> A: Yes. Document in research.md: built-in mode-line-format requires extensive custom elisp to show git branch, LSP status, and diagnostic counts together; doom-modeline provides this out of the box with doom-themes integration. Documentation fix, not design change.
- Q: Should SC-004 ("noticeably faster" LSP) be quantified with a fixed latency threshold? -> A: Keep as-is. eglot-booster gains are workload-dependent (GC pressure varies by project/language server). A fixed threshold would be arbitrary. Validate by confirming eglot-booster is active + manual acceptance testing.

## Assumptions

- The developer has API keys for at least one cloud AI provider (Anthropic or Google) or a running local Ollama instance. The configuration supports all three but requires at least one for AI features.
- The Nix package repository (nixpkgs) contains all required packages. Packages not yet in nixpkgs will be sourced from the emacs-overlay or community flakes.
- GitHub personal access token is available for forge integration (stored via auth-source). GitLab token also supported for work repositories; forge is configured for both backends.
- The existing test infrastructure (`just test-smoke`, `just test-fast`) remains the quality gate for regression testing.
- All formatter binaries (nixfmt, rustfmt, goimports, prettier, shfmt, ruff, clang-format) are available either globally via Nix runtime deps or per-project via devShell. Apheleia's formatter associations must align with the project's treefmt configuration to ensure on-save formatting matches batch `just format` output.
- The current `direnv` package will be cleanly replaced by `envrc` -- the direnv Emacs package is removed and envrc added in its place. The underlying direnv CLI and `.envrc` files remain unchanged; only the Emacs integration layer changes from global to buffer-local environment scoping.
- Claude Code is the primary AI coding tool; Emacs AI packages complement it for in-buffer convenience.

## Scope Boundaries

**In scope**:
- AI integration (chat, inline rewrite, context management via gptel; ghost text and MCP as optional P3 enhancements)
- Async formatting infrastructure
- LSP performance optimization
- Structural editing
- Environment isolation improvement
- UI polish (modeline, indent guides, visual feedback)
- Git workflow enhancement (forge, history browsing)
- Nix packaging for all new dependencies

**Out of scope**:
- Multi-file agentic editing from within Emacs (handled by Claude Code)
- Org-mode/writing enhancements (org-roam, olivetti) -- separate feature
- Snippet engine (tempel/yasnippet) -- separate feature
- Additional theme packages (ef-themes, catppuccin) -- cosmetic preference
- Tab-bar workspace management -- separate workflow feature
- Session persistence (desktop-save-mode) -- separate feature
- Spell checking overhaul (jinx replacing flyspell) -- separate feature
- Additional LSP servers for currently-commented-out languages -- per-project devShell responsibility
