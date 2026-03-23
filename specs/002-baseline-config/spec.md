# Feature Specification: Jotain Baseline Configuration

**Spec Directory**: `002-baseline-config`
**Created**: 2026-03-23
**Status**: Draft
**Input**: Baseline specification for an opinionated, modular Emacs configuration that favors built-ins, supports Nix and non-Nix deployments, and targets Linux x86_64, aarch64, and Android aarch64.

## Clarifications

### Session 2026-03-23

- Q: Does the baseline include specific functional modules or only the framework? → A: Framework + one minimal proof-of-life module (core defaults / platform detection) to validate the architecture end-to-end.
- Q: Can modules depend on other modules, or must they be strictly independent? → A: `lisp/` modules may depend on any other `lisp/` module (all assumed always available, dependencies documented). `modules/` may depend on anything under `lisp/` but not on other `modules/` files.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Launch Emacs with a Working Configuration (Priority: P1)

As a developer, I want to start Emacs and have a fully functional, opinionated editing environment immediately available, so that I can begin working on code without manual setup or package installation delays.

**Why this priority**: Without a bootable configuration, nothing else functions. This is the foundation every other story depends on.

**Independent Test**: Launch Emacs with the configuration loaded; verify that `init.el` completes without errors and that core editing features (syntax highlighting, line numbers, recent files) are operational.

**Acceptance Scenarios**:

1. **Given** a fresh Emacs 30.2 installation with jotain configuration files in place, **When** the user launches Emacs, **Then** `init.el` loads without errors or warnings and the scratch buffer is displayed with the expected theme and UI settings.
2. **Given** a jotain configuration on a system without Nix, **When** the user launches Emacs, **Then** third-party packages are fetched from package archives automatically and the configuration reaches the same functional state as a Nix-managed installation.
3. **Given** a jotain configuration on a Nix-managed system (NIX_PROFILES present), **When** the user launches Emacs, **Then** no network package fetching occurs, all packages are loaded from the Nix store, and runtime package archives are not configured.

---

### User Story 2 - Modular Feature Organization (Priority: P2)

As a developer maintaining jotain, I want each functional domain (editing, completion, version control, etc.) isolated in its own module file, so that I can add, remove, or modify features without affecting unrelated areas.

**Why this priority**: Modularity is the architectural backbone. Without clear separation, the configuration becomes a monolithic init.el that is hard to maintain and test.

**Independent Test**: Remove a single module file and verify that Emacs starts without errors (with degraded functionality only in the removed domain). Add a new module file and verify it is picked up by `init.el`.

**Acceptance Scenarios**:

1. **Given** the configuration with all modules loaded, **When** any single `lisp/` module is removed, **Then** Emacs starts without errors and modules that depended on the removed one degrade gracefully (log a warning, skip their functionality) rather than causing a hard failure.
2. **Given** a new module file placed in the appropriate directory with a `(provide 'module-name)` form, **When** `(require 'module-name)` is added to `init.el`, **Then** the module loads and its features are available.
3. **Given** a module in the built-in directory uses only built-in packages, **When** the configuration is loaded, **Then** no external package downloads are triggered by that module.

---

### User Story 3 - Nix and Home Manager Integration (Priority: P3)

As a NixOS user, I want to deploy jotain through Home Manager with a single option toggle, so that my Emacs environment is reproducibly built and managed alongside my system configuration.

**Why this priority**: Nix integration is the critical differentiator. It ensures reproducibility and leverages the existing system management tooling.

**Independent Test**: Enable jotain in a Home Manager configuration, build the generation, and verify that Emacs launches with all declared packages present from the Nix store.

**Acceptance Scenarios**:

1. **Given** a Home Manager configuration with jotain enabled, **When** the generation is built and activated, **Then** Emacs is available on PATH with all packages and runtime dependencies installed.
2. **Given** runtime dependencies are enabled, **When** Emacs is launched, **Then** LSP servers, tree-sitter grammars, and CLI tools referenced by modules are available on PATH.
3. **Given** extra packages are specified by the user, **When** the generation is built, **Then** those additional packages are available within Emacs alongside the standard jotain packages.

---

### User Story 4 - Non-Nix Portable Usage (Priority: P4)

As a user on a non-Nix Linux system or Android (via Termux), I want to clone the jotain repository and have a working Emacs configuration after a single launch, so that I can use it without adopting Nix.

**Why this priority**: Portability ensures the configuration is useful beyond the NixOS ecosystem and on mobile devices.

**Independent Test**: On a system without Nix (including Android/aarch64 via Termux), place the configuration files in the Emacs config directory, launch Emacs, and verify packages are auto-installed and the configuration is functional.

**Acceptance Scenarios**:

1. **Given** Emacs 30.2 installed on a non-Nix Linux system, **When** jotain files are placed in the Emacs configuration directory and Emacs is launched, **Then** all third-party packages are downloaded and installed automatically on first launch.
2. **Given** Emacs running on Android/aarch64 via Termux, **When** the configuration is loaded, **Then** it detects the constrained environment and loads without errors (platform-specific features may gracefully degrade).
3. **Given** the user creates a `private.el` file in the same directory as `init.el`, **When** Emacs initializes, **Then** `private.el` is loaded after all standard modules, allowing user customizations to override defaults.

---

### User Story 5 - Documentation for Three Audiences (Priority: P5)

As a user (future Markus, NixOS adopter, or blog reader), I want clear documentation that explains the philosophy, module structure, and deployment options, so that I can understand, adopt, or learn from jotain.

**Why this priority**: Documentation enables all three target audiences. Without it, the configuration is opaque to everyone except the author in the moment.

**Independent Test**: Read the README and verify it contains: philosophy statement, directory tree, Home Manager deployment instructions, non-Nix instructions, and architecture doc link — all within one screen for the quick-start section.

**Acceptance Scenarios**:

1. **Given** the README, **When** a reader opens it, **Then** the quick-start section fits within one screen (~40 lines) and includes deployment instructions for both Nix and non-Nix users.
2. **Given** the README, **When** a reader scans the directory tree, **Then** each module file has a one-line description of its functional domain.
3. **Given** the README, **When** a reader looks for architecture details, **Then** a link to a separate architecture document is provided.

---

### Edge Cases

- What happens when Emacs is launched on an unsupported platform (e.g., macOS, Windows)? The configuration should load without hard errors; platform-specific features should degrade gracefully with informational messages.
- What happens when a third-party package declared with `:ensure t` is unavailable in the Nix package set? The build should fail with a clear error message identifying the missing package mapping.
- What happens when `private.el` contains an error? Emacs should still complete initialization of the core configuration; the error in `private.el` should be caught and reported to the user without preventing startup.
- What happens when an older Emacs version (< 30) is used? The configuration should detect the version mismatch at startup and display a clear minimum-version warning.
- What happens when the Nix environment variable is set but the Nix-built packages are not on the load path? The configuration should detect this inconsistency and warn the user rather than silently failing.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The configuration MUST load without errors on Emacs 30.2 across Linux x86_64, Linux aarch64, and Android aarch64 platforms.
- **FR-002**: The configuration MUST detect whether it is running in a Nix-managed environment (by checking for the Nix environment indicator) and set a runtime flag accordingly.
- **FR-003**: When running under Nix management, the system MUST NOT configure package archives or enable automatic package fetching, ensuring no runtime network activity for packages.
- **FR-004**: When running outside Nix management, the system MUST configure package archives and enable automatic package fetching so that third-party packages are downloaded on first launch.
- **FR-005**: The configuration MUST organize built-in feature configurations and hand-written modules in a dedicated directory, using `use-package` with no external fetching.
- **FR-006**: The configuration MUST organize third-party package configurations in a separate directory, using `use-package` with external fetching enabled.
- **FR-007**: Each module file MUST declare what it provides and be loaded explicitly in `init.el`.
- **FR-008**: Load order in `init.el` MUST be explicit and deterministic. `lisp/` modules MAY depend on any other `lisp/` module (all assumed always available); dependencies MUST be documented and SHOULD fail gracefully. `modules/` MAY depend on `lisp/` but MUST NOT depend on other `modules/` files.
- **FR-009**: The configuration MUST load a user extension file (if it exists) after all standard modules have initialized, via a hook.
- **FR-010**: Every third-party package included MUST have documented justification for why the built-in alternative is insufficient.
- **FR-011**: The Home Manager module MUST expose options for: enable toggle, package override, daemon control, runtime dependency inclusion, and extra package addition.
- **FR-012**: The Nix build system MUST auto-extract package declarations from module files to determine the package set, with manual mappings for name mismatches.
- **FR-013**: The README MUST contain: a one-paragraph philosophy statement, a directory tree with one-line module descriptions, Home Manager deployment instructions, non-Nix usage instructions, and an architecture document link.
- **FR-014**: The README quick-start section MUST NOT exceed one screen of text (~40 lines).
- **FR-015**: The configuration MUST display a clear warning if the Emacs version is below the minimum required version.
- **FR-016**: Automated tests MUST exist for core initialization, Nix detection, module loading, and platform-specific behavior, organized by execution speed (smoke < 1s, fast < 5s, full suite).
- **FR-017**: The baseline MUST include proof-of-life modules in `lisp/` named after the Emacs features they extend (e.g., `jotain-defaults.el`, `jotain-platform.el`) to validate the framework end-to-end. Additional functional modules (editing, completion, version control, etc.) are deferred to future feature specs.

### Key Entities

- **Module**: A single file covering a functional domain (editing, completion, UI, etc.). Has a name, a directory (built-in or third-party), package dependencies, and a provide declaration.
- **Configuration Runtime**: The running Emacs instance with its detected environment (Nix-managed flag, platform, Emacs version). Determines package sourcing strategy and feature availability.
- **Home Manager Module**: The Nix integration layer that exposes user-facing options, builds the Emacs package set, and installs runtime dependencies.
- **Private Extensions**: User-provided extension file loaded after initialization. Allows overrides without modifying the core configuration.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Emacs starts and reaches a usable state in under 3 seconds on a modern system (both Nix-managed and non-Nix after initial package installation).
- **SC-002**: 100% of automated smoke tests pass on all three supported platforms (Linux x86_64, Linux aarch64, Android aarch64).
- **SC-003**: A new user can go from cloning the repository to a working Emacs session in under 5 minutes on a non-Nix system (including package download time on a broadband connection).
- **SC-004**: A NixOS user can deploy jotain via Home Manager with a single enable option and have a fully functional environment after one build-and-switch command.
- **SC-005**: Removing any single module from the configuration results in degraded functionality in only that domain — no cascading failures in unrelated modules.
- **SC-006**: The README quick-start section is comprehensible to a reader unfamiliar with jotain in under 2 minutes of reading time.
- **SC-007**: All smoke tests complete in under 1 second; all fast tests complete in under 5 seconds.

## Assumptions

- The user has Emacs 30.2 (or newer) installed. Older versions are detected and warned about but not supported.
- On non-Nix systems, the user has network access for initial package download on first launch.
- The Android/aarch64 target refers to Termux or similar terminal emulator environments; graphical Emacs on Android is not a requirement.
- "One screen" for the README quick-start is defined as approximately 40 lines at standard terminal width (80 columns).
- The `lisp/` and `modules/` directory names are the intended convention but may be adjusted during implementation if the current `elisp/` directory structure requires a transition period.
