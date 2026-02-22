# Feature Specification: Jotain Baseline

**Feature Branch**: `main`
**Created**: 2026-02-22
**Status**: Implemented
**Input**: Baseline specification documenting the current state of the Jotain project

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Install Jotain via Home Manager (Priority: P1)

A NixOS or home-manager user adds Jotain to their flake inputs and enables `programs.jotain.enable = true`. After a system rebuild, they get a fully configured Emacs with LSP support, modern completion, Git integration, and a consistent theme — without any manual package installation or configuration.

**Why this priority**: This is the primary delivery mechanism. Without reliable installation, no other feature matters.

**Independent Test**: Can be tested by enabling the Home Manager module in a minimal configuration and verifying that Emacs starts, config files are placed correctly, and runtime dependencies are available.

**Acceptance Scenarios**:

1. **Given** a home-manager configuration with `programs.jotain.enable = true`, **When** the user runs `home-manager switch`, **Then** Emacs is installed with all configured packages, config files are symlinked to `~/.config/emacs/`, and the Emacs daemon service is enabled.
2. **Given** `programs.jotain.includeRuntimeDeps = true` (default), **When** installation completes, **Then** LSP servers (nil, gopls, typescript-language-server, etc.), fonts (JetBrainsMono, Inter, etc.), and CLI tools (ripgrep, fd, direnv) are available on PATH.
3. **Given** `programs.jotain.enableDaemon = true` (default), **When** the user runs `emacsclient -c`, **Then** a new frame connects to the running daemon within 1 second.

---

### User Story 2 - Programming with LSP Support (Priority: P1)

A developer opens a source file (Nix, Go, TypeScript, Python, etc.) in Jotain. Eglot automatically connects to the appropriate LSP server, providing code completion, go-to-definition, diagnostics, and other IDE features without manual configuration.

**Why this priority**: Programming is the primary use case for Emacs. LSP integration is the core value proposition of a modern Emacs distribution.

**Independent Test**: Open a Nix file in a project with nil LSP server available and verify that completions, diagnostics, and navigation work.

**Acceptance Scenarios**:

1. **Given** a Nix file is opened and `nil` LSP server is on PATH, **When** the buffer loads, **Then** Eglot starts automatically and provides diagnostics within 5 seconds.
2. **Given** Eglot is connected, **When** the user invokes completion (Corfu), **Then** context-aware completions appear inline.
3. **Given** a tree-sitter grammar is available for the language, **When** a source file is opened, **Then** tree-sitter-based syntax highlighting is active.

---

### User Story 3 - Modern Completion System (Priority: P2)

A user interacts with Emacs minibuffer prompts (find file, switch buffer, M-x) and gets a vertical completion interface (Vertico) with fuzzy matching (Orderless), rich annotations (Marginalia), and search commands (Consult).

**Why this priority**: Completion is used in every Emacs workflow. A modern completion stack dramatically improves the user experience.

**Independent Test**: Press `M-x` and verify Vertico shows a vertical completion list with marginalia annotations and orderless filtering.

**Acceptance Scenarios**:

1. **Given** the user presses `M-x`, **When** they type a partial command name with spaces (orderless), **Then** matching commands are shown with descriptions.
2. **Given** the user presses `C-x b`, **When** they type a buffer name fragment, **Then** Consult shows matching buffers with previews.

---

### User Story 4 - Theme System with Light/Dark Toggle (Priority: P2)

A user can toggle between light (doom-nord-light) and dark (nord) themes using `C-c t`. In daemon mode, themes are correctly applied when new frames are created.

**Why this priority**: Visual comfort directly impacts productivity. Theme toggling is essential for users who work in varying lighting conditions.

**Independent Test**: Press `C-c t` and verify the theme switches between light and dark variants.

**Acceptance Scenarios**:

1. **Given** the dark theme is active, **When** the user presses `C-c t`, **Then** the light theme is applied without any theme blending artifacts.
2. **Given** Emacs is running as a daemon, **When** a new frame is created via `emacsclient -c`, **Then** the current theme is correctly applied to the new frame.

---

### User Story 5 - Git Integration via Magit (Priority: P2)

A developer presses `C-x g` to open Magit status, can stage/unstage changes, commit, push, and manage branches entirely within Emacs.

**Why this priority**: Git is fundamental to software development. Magit is one of Emacs's strongest advantages over other editors.

**Independent Test**: Open a git repository, press `C-x g`, verify Magit status buffer shows repository state.

**Acceptance Scenarios**:

1. **Given** a git repository, **When** the user presses `C-x g`, **Then** Magit status shows tracked/untracked files and staged changes.

---

### User Story 6 - Cross-Platform Operation (Priority: P3)

Jotain works on Linux (primary), macOS, and Android (via Termux). Platform-specific features (clipboard, font rendering, input methods) adapt automatically based on detection.

**Why this priority**: Broadens the user base but most users are on Linux.

**Independent Test**: Launch Emacs on each supported platform and verify platform-specific adaptations are active.

**Acceptance Scenarios**:

1. **Given** Emacs is launched on Android, **When** the platform is detected, **Then** Android-specific keybindings and UI adaptations are applied.
2. **Given** Emacs is launched on macOS, **When** the platform is detected, **Then** macOS-specific modifier key mappings are applied.

---

### User Story 7 - Extend with Extra Packages (Priority: P3)

A user adds custom Emacs packages via `programs.jotain.extraPackages` in their home-manager configuration. The packages are available in Emacs without conflicting with Jotain's built-in packages.

**Why this priority**: Customizability ensures Jotain works for diverse workflows, but the core experience must work without customization.

**Independent Test**: Add `epkgs.pdf-tools` to extraPackages, rebuild, and verify the package is available in Emacs.

**Acceptance Scenarios**:

1. **Given** `programs.jotain.extraPackages = epkgs: [ epkgs.pdf-tools ]`, **When** the user rebuilds, **Then** `pdf-tools` is available in Emacs alongside all built-in packages.

---

### Edge Cases

- What happens when an LSP server is not installed (includeRuntimeDeps = false)? Eglot should not error; it should report the missing server gracefully.
- What happens when Emacs is started without a display (TTY mode)? UI features should degrade gracefully (no GUI-only themes, appropriate terminal settings).
- What happens when tree-sitter grammars are missing for a language? Emacs should fall back to traditional font-lock highlighting.
- What happens when the user has an existing `~/.config/emacs/` directory? Home Manager should manage symlinks without destroying user files (custom.el is preserved).

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST install Emacs 30 (PGTK variant on Linux) with all declared packages via Nix.
- **FR-002**: System MUST automatically extract use-package declarations from elisp files and resolve them to nixpkgs packages.
- **FR-003**: System MUST provide a Home Manager module with options for enable, daemon, runtime deps, and extra packages.
- **FR-004**: System MUST configure Eglot as the LSP client with automatic server detection for supported languages.
- **FR-005**: System MUST provide Vertico + Corfu + Consult + Orderless as the completion stack.
- **FR-006**: System MUST support light/dark theme toggling via `C-c t` without theme blending.
- **FR-007**: System MUST install and configure tree-sitter grammars for syntax highlighting.
- **FR-008**: System MUST run Emacs as a systemd/launchd daemon service when enableDaemon is true.
- **FR-009**: System MUST provide an isolated development environment (`.dev-home/`) for safe testing.
- **FR-010**: System MUST detect the host platform and apply appropriate adaptations.
- **FR-011**: System MUST provide a tiered testing infrastructure (smoke < 1s, fast < 5s, full suite, NMT, VM runtime).

### Key Entities

- **Jotain Package**: The Nix package containing init.el, early-init.el, and elisp modules. Delivered to `$out/share/jotain/`.
- **JotainEmacs**: The wrapped Emacs derivation with all packages, runtime deps, and environment variables baked in.
- **Home Manager Module**: The NixOS/home-manager integration point. Manages Emacs installation, daemon, runtime deps, and config file placement.
- **Elisp Module**: A single `.el` file in `elisp/` covering one functional domain. Loaded in order by `init.el`.
- **Runtime Dependencies**: LSP servers, fonts, CLI tools, and tree-sitter grammars installed alongside Emacs.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: A new user can go from adding Jotain to their flake to a fully working Emacs environment in a single `home-manager switch` command.
- **SC-002**: Emacs starts within 3 seconds in daemon mode and `emacsclient` connects within 1 second.
- **SC-003**: All smoke tests pass in under 1 second; all fast tests pass in under 5 seconds.
- **SC-004**: LSP features (completion, diagnostics, navigation) are functional within 5 seconds of opening a supported file type.
- **SC-005**: The distribution supports at least 3 platforms (Linux, macOS, Android) with automatic adaptation.
- **SC-006**: Users can add custom packages via `extraPackages` without conflicts with built-in packages.
- **SC-007**: Theme toggling switches instantly with no visual artifacts from theme blending.
