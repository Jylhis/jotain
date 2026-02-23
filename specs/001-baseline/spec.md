# Feature Specification: Jotain Baseline

**Feature Branch**: `001-baseline`
**Created**: 2026-02-22
**Status**: Validated
**Input**: Baseline specification documenting the current state of the Jotain modular Emacs distribution

## Scope

Jotain is an opinionated, curated Emacs distribution. It is NOT a
general-purpose framework for arbitrary user customisation.

**In scope**: All functionality described in this specification —
installation, LSP, completion, Git, Org-mode, themes, platform
support, and the Nix build system that binds them together.

**Target audience**:
- Generalist software engineers who want a batteries-included,
  Nix-managed Emacs distribution without maintaining their own config
  - Experienced Emacs users who want curated defaults over DIY configuration
  - Nix power users who want Emacs deeply integrated into their
    declarative system configuration
- NOT beginners to Emacs; no guided onboarding or tutorial system
  is provided

**Out of scope**:
- User-defined module systems or plugin architectures beyond
  `extraPackages` and `custom.el`
- Email clients (mu4e is available on Linux but not a core feature)
- Window manager integration (EXWM is referenced in platform code
  but not a supported workflow)
- Alternative completion stacks (Helm, Ivy) or LSP clients (lsp-mode)
- Persistent package installation at runtime (MELPA/ELPA/package.el).
  Ad-hoc session-only loading via `M-x load-library` or `require`
  is permitted but does not persist across sessions

## Clarifications

### Session 2026-02-22

- Q: What are Jotain's out-of-scope boundaries? → A: Opinionated
  distribution; not a framework for arbitrary customisation. Users
  extend only via `extraPackages` and `custom.el`.
- Q: How should credential/secrets management be specified? → A:
  Optional capability; 1Password auth-source integration exists but
  requires user-supplied `op` CLI; not a core requirement.
- Q: Who is the target audience? → A: Experienced Emacs users
  wanting a curated Nix-managed distribution, and Nix power users
  wanting Emacs integrated into their Nix workflow. Beginners are
  not the target persona.
- Q: What is the standalone (non-daemon) startup performance
  target? → A: Under 3 seconds on a modern machine.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Install and Launch Emacs (Priority: P1)

A developer wants a fully configured, modern Emacs environment on
their machine. They add Jotain to their Home Manager configuration,
rebuild, and get a working Emacs with LSP support, completion,
Git integration, and a curated theme — without manual configuration.

**Why this priority**: This is the core value proposition. If a user
cannot install Jotain and get a working Emacs, nothing else matters.

**Independent Test**: Can be verified by enabling the Home Manager
module, rebuilding, and launching Emacs to confirm it starts
without errors and provides a functional editing environment.

**Acceptance Scenarios**:

1. **Given** a NixOS or Home Manager system with Jotain enabled,
   **When** the user rebuilds and opens Emacs,
   **Then** Emacs starts with all configured modules loaded, no
   error messages, and a dashboard greeting screen.

2. **Given** a fresh installation with default options,
   **When** the user opens a source file (e.g., `.py`, `.go`, `.nix`),
   **Then** syntax highlighting, indentation, and tree-sitter
   grammars are active for that language.

3. **Given** `enableDaemon` is true (default),
   **When** the user runs `emacsclient -c`,
   **Then** a new frame opens instantly connected to the running
   daemon with full configuration available.

4. **Given** a successful installation,
   **When** the user checks the environment,
   **Then** LSP servers (nil, gopls, clangd, marksman, etc.),
   CLI tools (ripgrep, fd, direnv), and fonts are available on PATH.

---

### User Story 2 - Code with Modern Completion and LSP (Priority: P1)

A developer opens a project and writes code. They expect intelligent
completion suggestions, real-time diagnostics, go-to-definition,
and find-references — powered by LSP and a modern completion UI.

**Why this priority**: Code editing with language intelligence is the
primary daily workflow. It is co-equal with installation in importance.

**Independent Test**: Open a Go or Nix file in a project with an LSP
server, verify that completions appear, diagnostics are shown, and
navigation commands work.

**Acceptance Scenarios**:

1. **Given** an open source file with a configured LSP server,
   **When** the user types a partial identifier,
   **Then** Corfu displays completion candidates with documentation.

2. **Given** a file with a syntax error,
   **When** Flymake processes the buffer,
   **Then** the error location is highlighted and a diagnostic
   message is accessible at point.

3. **Given** a symbol under the cursor,
   **When** the user invokes go-to-definition,
   **Then** Eglot navigates to the symbol's definition.

4. **Given** any open buffer,
   **When** the user invokes Consult search (`C-s` or `M-s`),
   **Then** Vertico presents a minibuffer with live narrowing,
   Orderless matching, and Marginalia annotations.

---

### User Story 3 - Manage Git Repositories (Priority: P2)

A developer uses Magit to stage changes, commit, browse history,
and work with Git worktrees — all within Emacs.

**Why this priority**: Git integration is essential for daily
workflow but is secondary to the core editing experience.

**Independent Test**: Open Magit status in a Git repository, stage a
file, create a commit, and verify the operation succeeds.

**Acceptance Scenarios**:

1. **Given** the user is in a Git repository,
   **When** they press `C-c g`,
   **Then** Magit status buffer opens showing the working tree state.

2. **Given** the Magit status buffer,
   **When** the user stages a file and commits,
   **Then** the commit is created and reflected in the log.

3. **Given** a repository with worktrees,
   **When** the user views Magit status,
   **Then** worktrees are listed in a dedicated status section.

---

### User Story 4 - Write and Organise with Org-mode (Priority: P2)

A developer uses Org-mode for notes, documentation, task tracking,
and agenda management. They expect mixed-pitch fonts, modern
styling, and dynamic agenda file discovery.

**Why this priority**: Org-mode is a key secondary workflow but not
all users rely on it.

**Independent Test**: Create an Org file, verify visual styling
(variable-pitch headings, fixed-pitch code blocks), and check that
agenda files are discovered from configured directories.

**Acceptance Scenarios**:

1. **Given** an Org-mode buffer,
   **When** the user views the document,
   **Then** headings use variable-pitch fonts and code blocks use
   fixed-pitch fonts.

2. **Given** `~/Documents` contains `.org` files,
   **When** the user opens the agenda,
   **Then** agenda files are dynamically discovered from that
   directory (recursively, excluding hidden directories).

3. **Given** Org-mode is active,
   **When** the user exports a document,
   **Then** exporters for Slack, Jira, Hugo, and GitHub-Flavored
   Markdown are available.

---

### User Story 5 - Switch Themes and Customise Appearance (Priority: P3)

A user wants to toggle between light and dark themes, adjust font
sizes, and have the system theme detected automatically.

**Why this priority**: Visual customisation enhances comfort but
does not block core functionality.

**Independent Test**: Press `C-c t` to toggle themes, use `C-+`/`C--`
to adjust font size, and verify auto-dark detection responds to
system theme changes.

**Acceptance Scenarios**:

1. **Given** Emacs is running with the dark theme (doom-nord),
   **When** the user presses `C-c t`,
   **Then** the theme switches to light (doom-nord-light) without
   blending artifacts from the previous theme.

2. **Given** a running Emacs frame,
   **When** the user presses `C-+` or `C--`,
   **Then** the font size increases or decreases respectively, and
   `C-0` resets to default.

3. **Given** auto-dark is enabled and the OS switches to dark mode,
   **When** Emacs detects the change,
   **Then** the theme automatically switches to the dark variant.

---

### User Story 6 - Use Jotain Across Platforms (Priority: P3)

A developer uses Jotain on Linux (primary), macOS (secondary), and
Android/Termux (mobile). Platform-specific adaptations activate
automatically without user intervention.

**Why this priority**: Multi-platform support is important for
portability but Linux is the primary target.

**Independent Test**: Build and run Jotain on each supported platform,
verify platform detection constants are correct and platform-specific
features activate.

**Acceptance Scenarios**:

1. **Given** Jotain running on Linux,
   **When** platform detection runs,
   **Then** `platform-linux-p` is true, vterm is available, and
   XDG-compliant browser opening works.

2. **Given** Jotain running on macOS,
   **When** platform detection runs,
   **Then** `platform-macos-p` is true, modifier keys are set
   (Command=Meta, Option=Super), and vterm is disabled.

3. **Given** Jotain running on Android/Termux,
   **When** platform detection runs,
   **Then** `platform-android-p` is true, touch-optimised UI
   activates, and volume keys are bound to useful commands.

4. **Given** the Emacs daemon is running,
   **When** the user runs `emacsclient -c`,
   **Then** a new graphical frame opens connected to the daemon
   with full configuration and theme applied.

5. **Given** the Emacs daemon is running,
   **When** the user runs `emacsclient -t`,
   **Then** a new terminal frame opens connected to the daemon
   with full configuration available in the terminal.

---

### Edge Cases

- What happens when a user enables Jotain but has no fonts installed
  and `includeRuntimeDeps` is false?
  The system MUST still start; font setup degrades gracefully to
  system defaults.

- What happens when a tree-sitter grammar is missing for an opened
  file type?
  The system MUST fall back to traditional syntax highlighting
  without errors.

- What happens when no LSP server is available for a language?
  Eglot MUST NOT auto-start; the user gets standard editing
  without LSP errors.

- What happens when the Emacs daemon is enabled but the service
  fails to start?
  The user MUST still be able to launch Emacs directly (non-daemon)
  as a fallback.

- What happens when `extraPackages` adds a package that conflicts
  with a built-in Jotain package?
  The system MUST load without errors; the user-provided package
  takes precedence via standard Emacs load-path ordering.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Jotain MUST build deterministically from `flake.nix`
  with no external state beyond flake inputs.
- **FR-002**: All Emacs packages MUST be installed via Nix; no
  runtime package installation from MELPA/ELPA.
- **FR-003**: The Home Manager module MUST provide `enable`,
  `enableDaemon`, `includeRuntimeDeps`, and `extraPackages` options.
- **FR-004**: When `enableDaemon` is true, Jotain MUST configure a
  systemd user service (Linux) or launchd agent (macOS) for the
  Emacs daemon.
- **FR-005**: When `includeRuntimeDeps` is true, LSP servers, CLI
  tools, fonts, and tree-sitter grammars MUST be available on PATH.
- **FR-006**: Elisp modules MUST load in dependency-safe order
  without circular dependencies.
- **FR-007**: The completion system MUST provide minibuffer narrowing
  (Vertico), in-buffer completion (Corfu), search (Consult), and
  fuzzy matching (Orderless).
- **FR-008**: Eglot MUST be configured with server programs for at
  least Nix, Go, TypeScript, YAML, Docker, Markdown, and C/C++.
- **FR-009**: Tree-sitter grammars MUST be provided for all supported
  languages via the `TREE_SITTER_DIR` environment variable.
- **FR-010**: Theme switching MUST disable all active themes before
  loading a new one to prevent blending.
- **FR-011**: Font configuration MUST support fallback chains and
  cache font enumeration for performance.
- **FR-012**: Platform detection MUST expose boolean constants and
  conditional macros for Linux, macOS, Android, and Windows.
- **FR-013**: Magit MUST be configured with repository discovery
  under `~/Developer` and worktree status integration.
- **FR-014**: Org-mode MUST dynamically discover agenda files from
  configured directories recursively.
- **FR-015**: Jotain MUST support running as a NixOS system-wide
  module in addition to Home Manager.
- **FR-016**: The `nix/lib/dependencies.nix` scanner MUST
  automatically extract package names from `use-package :ensure t`
  declarations without running Emacs.
- **FR-017**: ERT tests MUST exist for all critical modules, tagged
  by execution speed (smoke, fast, unit, integration, slow).
- **FR-018**: NMT tests MUST validate each Home Manager module
  option (enabled/disabled, daemon on/off, runtime deps on/off).
- **FR-019**: Jotain SHOULD provide 1Password auth-source
  integration as an optional capability. It MUST NOT fail or
  produce errors when the `op` CLI is absent.

### Key Entities

- **Elisp Module**: A single `.el` file in `elisp/` covering one
  functional domain. Has a name, purpose, dependencies on other
  modules, and use-package declarations.
- **Runtime Dependency**: An external tool (LSP server, CLI utility,
  font, or tree-sitter grammar) required at runtime. Declared in
  `nix/lib/runtime-deps.nix` and wrapped into the Emacs derivation.
- **Home Manager Option**: A configurable parameter exposed by the
  Home Manager module. Has a name, type, default value, and effect
  on the generated configuration.
- **Platform**: A target operating system (Linux, macOS, Android,
  Windows). Detected at load time; gates conditional behavior
  throughout the configuration.
- **Theme**: A visual appearance configuration. Jotain maintains
  exactly two sanctioned themes (one light, one dark) with a toggle
  and auto-detection mechanism.
- **Test Suite**: A collection of ERT tests grouped by tag. Smoke
  tests validate critical infrastructure, fast tests cover module
  behavior, integration tests verify cross-module interactions.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: A new user can install Jotain via Home Manager and
  have a working Emacs environment after a single `nixos-rebuild`
  or `home-manager switch` command.
- **SC-002**: Emacs starts without errors or warnings on all
  supported platforms (Linux, macOS, Android).
- **SC-003**: All smoke tests pass in under 1 second, all fast
  tests in under 5 seconds.
- **SC-004**: The Emacs daemon starts and accepts `emacsclient`
  connections within 10 seconds of service activation.
- **SC-005**: LSP servers are available and functional for all
  supported languages (as defined in FR-008) when
  `includeRuntimeDeps` is enabled.
- **SC-006**: Theme switching completes without visual artifacts
  (no blended theme remnants).
- **SC-007**: Font scaling (increase, decrease, reset) responds
  immediately to keybindings without delay.
- **SC-008**: Tree-sitter enhanced highlighting is active for all
  languages with available grammars.
- **SC-009**: NMT tests cover 100% of Home Manager module options
  (enabled/disabled, daemon, runtime deps).
- **SC-010**: The project builds successfully with `nix build` from
  a clean checkout with no network access beyond the flake inputs.
- **SC-011**: Standalone (non-daemon) Emacs starts with full
  configuration loaded in under 3 seconds on a modern machine.
