# Jotain Constitution

## Core Principles

### I. Nix-First Package Management

All Emacs packages MUST be installed and managed exclusively through Nix. Runtime package installation via MELPA/ELPA is prohibited. The `nix/lib/dependencies.nix` module automatically extracts `use-package` declarations with `:ensure t` from elisp files and maps them to nixpkgs package names. This ensures fully reproducible builds across all environments.

**Rationale**: Reproducibility is the foundation of a reliable Emacs distribution. Nix guarantees that every user gets the exact same set of packages, eliminating "works on my machine" issues.

### II. Modular Elisp Architecture

Each elisp file in `elisp/` MUST cover a single functional domain (e.g., completion, programming, git). Modules are loaded by `init.el` in a defined order and MUST NOT have circular dependencies. New functionality MUST be added to the appropriate existing module or, when no module fits, introduced as a new domain-specific file.

**Rationale**: Modularity enables independent development, testing, and understanding of each feature domain. Users and contributors can reason about one domain without needing global knowledge.

### III. Comprehensive Testing

All code changes MUST be validated by the existing test infrastructure. Tests are tagged (`smoke`, `fast`, `unit`, `integration`) and run in isolated environments (`HOME=.dev-home`). Smoke tests MUST complete in under 1 second, fast tests in under 5 seconds. New features SHOULD include corresponding ERT tests. NMT integration tests validate the Home Manager module contract.

**Rationale**: A distribution used by others demands confidence in stability. The tiered testing strategy balances development speed (smoke/fast) with thoroughness (full suite, NMT, VM runtime).

### IV. Cross-Platform Support

Jotain MUST support Linux, macOS, and Android. Platform-specific behavior is handled through `elisp/platform.el` (detection) and `elisp/platforms.el` (adaptations). Platform-specific code MUST be guarded by feature detection, never by hardcoded assumptions. The Nix build system handles platform-specific runtime dependencies.

**Rationale**: Emacs users work across diverse platforms. Platform support broadens the user base and ensures the distribution is useful in varied environments.

### V. Declarative Configuration via Home Manager

The primary installation method is the Home Manager module (`nix/modules/home/default.nix`). Module options (`programs.jotain.*`) MUST provide sensible defaults while allowing customization. The module MUST handle daemon setup, runtime dependency injection, font configuration, and XDG-compliant file placement.

**Rationale**: Home Manager integration makes Jotain a first-class NixOS/home-manager citizen, allowing users to manage their Emacs configuration declaratively alongside the rest of their system.

### VI. Simplicity and Minimalism

New features and changes MUST justify their complexity. Prefer built-in Emacs functionality over external packages when the built-in solution is adequate. Avoid over-engineering: no premature abstractions, no unused configuration options, no speculative features. The `YAGNI` principle applies.

**Rationale**: Emacs configurations tend toward unbounded growth. Discipline in adding complexity keeps the distribution maintainable and the startup time fast.

## Build System Constraints

- `nix/lib/dependencies.nix` uses pure Nix regex extraction (no derivations) to scan elisp files for package declarations. This MUST remain pure to avoid cross-system build issues.
- Tree-sitter grammars are injected via the `TREE_SITTER_DIR` environment variable set in `emacs.nix`. Grammar management MUST go through `nix/lib/runtime-deps.nix`.
- `early-init.el` handles pre-package initialization (native compilation, UI optimization, tree-sitter path setup) and MUST NOT depend on any external packages.
- Byte compilation is skipped at build time due to load-order dependencies; native JIT compilation handles optimization at runtime.

## Development Workflow

- All development commands are exposed via `just` (see `justfile`). Contributors MUST use `just` commands rather than invoking Nix or Emacs directly for standard operations.
- Development uses an isolated `.dev-home/` directory to prevent contamination of the developer's personal Emacs configuration.
- The `emacs-dev` and `emacs-test-interactive` commands provide safe, isolated Emacs instances for testing changes.
- CI runs via DeterminateSystems/ci with Nix flake checks. VM runtime tests (`test-runtime`) only execute in CI (`CI=1`).
- Formatting is enforced via `treefmt` (nixfmt for Nix, shfmt for shell scripts, built-in Emacs indentation for elisp).

## Governance

- This constitution defines the non-negotiable principles for the Jotain project. All contributions MUST comply with these principles.
- Amendments require updating this document, documenting the rationale, and incrementing the version according to semver (MAJOR for principle removals/redefinitions, MINOR for additions, PATCH for clarifications).
- The CLAUDE.md file provides runtime development guidance and MUST remain consistent with this constitution.

**Version**: 1.0.0 | **Ratified**: 2026-02-22 | **Last Amended**: 2026-02-22
