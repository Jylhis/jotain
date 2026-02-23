<!--
  Sync Impact Report
  ===========================================================================
  Version change: 0.0.0 → 1.0.0 (initial ratification)

  Modified principles: N/A (all new)

  Added sections:
    - Core Principles (7 principles)
    - Technical Constraints
    - Development Workflow
    - Governance

  Removed sections: N/A

  Templates requiring updates:
    - .specify/templates/plan-template.md          ✅ compatible (no changes needed)
    - .specify/templates/spec-template.md           ✅ compatible (no changes needed)
    - .specify/templates/tasks-template.md          ✅ compatible (no changes needed)
    - .specify/templates/checklist-template.md      ✅ compatible (no changes needed)
    - .specify/templates/constitution-template.md   ✅ source template (unchanged)
    - .claude/commands/speckit.*.md                 ✅ no outdated references

  Follow-up TODOs: none
  ===========================================================================
-->

# Jotain Constitution

## Core Principles

### I. Nix-Only Package Management (NON-NEGOTIABLE)

All Emacs packages MUST be installed via Nix. Runtime package installation
from MELPA, ELPA, or any other Emacs package archive is prohibited.

- `use-package-always-ensure` MUST be set to `nil` globally.
- Packages requiring `:ensure t` MUST be resolvable by
  `nix/lib/dependencies.nix` either automatically or via an explicit
  entry in `packageNameMap`.
- Built-in Emacs packages MUST use `:ensure nil`.
- `package-archives` MUST be set to `nil` in `init.el`.
- No Elisp file may call `package-install`, `package-refresh-contents`,
  or equivalent functions.

**Rationale**: Nix-managed packages guarantee reproducibility. Runtime
installation introduces version drift, network dependencies during
startup, and breaks the declarative model.

### II. Modular Elisp Architecture

Every Elisp configuration file MUST cover exactly one functional domain.
Modules MUST NOT create circular dependencies.

- Each file in `elisp/` MUST have a single, well-defined purpose
  (e.g., `completion.el` handles completion, `git.el` handles Git).
- New functionality MUST be added as a new module or extend the
  appropriate existing module; cross-domain pollution is prohibited.
- All modules MUST use `lexical-binding: t`.
- Modules MUST declare dependencies explicitly via `require` when
  depending on another project module.
- `init.el` MUST load modules in dependency-safe order and MUST be
  the sole orchestrator of load sequence.

**Rationale**: Modular design enables independent testing, clear
ownership, and reduces merge conflicts. Lexical binding is required
for performance and correctness in modern Elisp.

### III. Reproducibility First

The project MUST build deterministically from `flake.nix` with no
external state required beyond the flake inputs.

- All external dependencies (LSP servers, CLI tools, fonts,
  tree-sitter grammars) MUST be declared in
  `nix/lib/runtime-deps.nix`.
- Tree-sitter grammars MUST be provided via the `TREE_SITTER_DIR`
  environment variable injected at build time.
- The Home Manager module MUST produce identical configurations
  given the same options and flake lock.
- `flake.lock` MUST be committed and updated intentionally
  (via Renovate or explicit `nix flake update`).

**Rationale**: Reproducibility is the core value proposition. A user
on any supported platform MUST get the same Emacs environment from
the same commit.

### IV. Testing Discipline

Every user-facing feature and critical infrastructure MUST have
corresponding tests. Tests MUST pass before merging to `main`.

- ERT tests MUST exist in `tests/` and follow the naming convention
  `test-<module>.el`.
- Tests MUST be tagged: `smoke`, `fast`, `unit`, `integration`,
  or `slow`.
- Smoke tests MUST complete in under 1 second.
- Fast tests MUST complete in under 5 seconds.
- NMT tests in `nmt-tests/` MUST validate Home Manager module
  behavior for each configurable option.
- New Home Manager options MUST have corresponding NMT test cases.
- Tests MUST run in isolation (`HOME=.dev-home`) and MUST NOT
  contaminate the user's actual configuration.

**Rationale**: The three-tier test strategy (smoke/fast/full) enables
rapid feedback during development while maintaining comprehensive
coverage for CI.

### V. Platform Portability

Jotain MUST function on Linux, macOS, and Android. Platform-specific
code MUST be guarded by `platform.el` detection.

- `platform.el` MUST provide constants (`platform-linux-p`,
  `platform-macos-p`, `platform-android-p`, `platform-windows-p`)
  and macros (`platform-when`, `platform-unless`, `platform-cond`).
- Platform-specific behavior MUST use these guards; raw
  `system-type` checks outside `platform.el` are prohibited.
- Features that cannot work on a platform MUST degrade gracefully
  (disable silently) rather than error.
- Runtime dependency lists in `nix/lib/runtime-deps.nix` MUST
  account for platform availability (e.g., `libvterm` Linux-only).

**Rationale**: Users expect the same configuration to work across
their devices. Platform detection centralised in one module prevents
scattered, inconsistent conditionals.

### VI. Performance by Design

Startup and runtime performance MUST be considered for every change.
Regressions MUST be justified.

- `early-init.el` MUST disable GUI elements and configure native
  compilation before the frame is drawn.
- Font enumeration MUST be cached; repeated system calls for font
  lists are prohibited.
- Idle timers MUST be used for non-critical post-startup work
  (e.g., GC threshold restoration).
- Daemon mode MUST be the default; theme and font setup MUST
  support both daemon and non-daemon paths via appropriate hooks.
- JSON-RPC logging in Eglot MUST be disabled in production builds
  to avoid I/O overhead.

**Rationale**: Emacs startup time directly affects user experience.
Daemon mode amortises this cost, but initial load must still be
optimised for `emacsclient` frame creation.

### VII. Simplicity & YAGNI

Prefer the simplest solution that satisfies the requirement. Do not
add abstractions, features, or configuration options speculatively.

- New `use-package` declarations MUST solve an existing, concrete
  problem; "might be useful later" is insufficient justification.
- Helper functions MUST NOT be created for one-time operations.
- Over-engineering (unnecessary indirection, premature abstraction,
  speculative configuration options) MUST be rejected in review.
- Home Manager module options MUST be added only when users need
  to toggle behavior; internal implementation details MUST NOT be
  exposed as options.
- Three similar lines of code are preferable to a premature
  abstraction.

**Rationale**: Complexity is the primary threat to maintainability.
Every abstraction carries a comprehension cost that must be earned
by repeated, concrete use.

## Technical Constraints

- **Emacs Version**: Emacs 30+ (PGTK build) is required. Features
  relying on Emacs 30 APIs (e.g., `use-package` built-in,
  tree-sitter integration) are permitted without fallback.
- **Nix**: Flake-based builds only. Legacy `nix-build` or
  `nix-channel` workflows are not supported.
- **Package Management**: `nix/lib/dependencies.nix` auto-extracts
  packages from `use-package :ensure t` declarations. Manual
  mappings in `packageNameMap` are required only when the Elisp
  package name does not match the nixpkgs attribute name.
- **Runtime Dependencies**: LSP servers, CLI tools, fonts, and
  tree-sitter grammars are declared in `nix/lib/runtime-deps.nix`
  and wrapped into the Emacs derivation via `symlinkJoin`.
- **Home Manager**: The `homeModules.default` output MUST remain
  the primary integration point. NixOS module is supplementary.
- **Byte Compilation**: The `jotain-modules` overlay skips
  byte-compilation at build time (load-order conflicts). Native
  compilation handles JIT optimization at runtime.
- **Completion Stack**: Vertico, Corfu, Consult, Orderless, and
  Embark form the completion ecosystem. Replacing any component
  requires a constitution amendment.
- **LSP**: Eglot is the LSP client. Lsp-mode is not permitted
  unless Eglot is formally deprecated upstream.
- **Theme System**: `doom-themes` (doom-nord-light) and `nord-theme`
  (nord) are the sanctioned themes. `auto-dark` handles system
  theme detection. Theme additions require review.

## Development Workflow

### Branching and Commits

- The `main` branch is the stable integration branch.
- Feature branches follow the pattern `NNN-feature-name` when
  using speckit, or descriptive names otherwise.
- Commits MUST follow Conventional Commits:
  `type(scope): description` where type is one of `feat`, `fix`,
  `perf`, `refactor`, `docs`, `test`, `chore`, `style`.

### Quality Gates (CI)

All checks MUST pass before merging to `main`:

1. **Formatting**: `nixfmt`, `shfmt`, custom Elisp indentation.
2. **Smoke tests**: `just test-smoke` (< 1 second).
3. **Fast tests**: `just test-fast` (< 5 seconds).
4. **Full ERT suite**: `just test` (all tags).
5. **NMT module tests**: `just test-nmt`.
6. **VM runtime test** (CI only, `CI=1`): `just test-runtime`.

### Local Development

- `just emacs-dev` MUST be used for interactive testing; it
  isolates to `.dev-home/` and symlinks local sources.
- `just compile` validates byte-compilation without Nix.
- `just check-fast` is the recommended pre-push validation.
- The `.envrc` / `shell.nix` provides a consistent development
  environment via `direnv`.

### Dependency Updates

- Renovate bot manages `flake.lock` updates automatically.
- Manual updates via `nix flake update` MUST be followed by
  a full test run (`just test-all`).

## Governance

This constitution is the authoritative reference for architectural
decisions and development practices in the Jotain project.

- **Supremacy**: This constitution supersedes all other guidance
  documents, comments, and informal practices. Conflicts MUST be
  resolved in favour of the constitution.
- **Amendments**: Changes to principles MUST be documented with
  rationale, reviewed, and reflected in a version increment.
  Backward-incompatible changes (principle removal, redefinition)
  require a MAJOR version bump.
- **Compliance**: All pull requests MUST be verified against
  applicable principles. Violations MUST be either fixed or
  justified with an entry in the Complexity Tracking table
  of the implementation plan.
- **Versioning**: MAJOR.MINOR.PATCH semantic versioning.
  MAJOR = principle removal/redefinition, MINOR = new principle
  or material expansion, PATCH = wording/clarification.
- **Review Cadence**: The constitution SHOULD be reviewed when
  adding significant new subsystems or when repeated violations
  suggest a principle needs revision.
- **Runtime Guidance**: `CLAUDE.md` serves as the runtime
  development guidance file and MUST remain consistent with
  this constitution.

**Version**: 1.0.0 | **Ratified**: 2026-02-22 | **Last Amended**: 2026-02-22
