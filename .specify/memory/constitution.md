<!--
Sync Impact Report
==================
Version change: N/A → 1.0.0 (initial ratification)

Added principles:
  - I. Built-in First
  - II. Nix-Native Packaging
  - III. Modular Separation
  - IV. Personal Config, Not a Distribution
  - V. Test-First Development
  - VI. Justified Complexity
  - VII. Three-Audience Documentation

Added sections:
  - Technology Stack & Constraints
  - Development Workflow

Removed sections: none (initial version)

Templates requiring updates:
  - .specify/templates/plan-template.md — ✅ no update needed
    (Constitution Check section is generic; populated per feature)
  - .specify/templates/spec-template.md — ✅ no update needed
    (User stories and requirements structure is compatible)
  - .specify/templates/tasks-template.md — ✅ no update needed
    (Phase structure is compatible; Elisp/Nix specifics go in generated tasks)
  - .specify/templates/checklist-template.md — ✅ no update needed
  - .specify/templates/agent-file-template.md — ✅ no update needed
  - No command templates exist yet

Follow-up TODOs: none
-->

# Jotain Constitution

## Core Principles

### I. Built-in First

Every feature MUST start as an Emacs built-in capability configured
through `use-package` with `:ensure nil`, or as hand-written Elisp in
the `lisp/` directory. Third-party packages are permitted only when a
built-in solution does not exist or is demonstrably inadequate.

**Rationale:** Emacs 30+ ships a rich set of built-in packages
(Eglot, Treesit, use-package, project.el, etc.). Relying on built-ins
reduces dependency surface, improves startup time, and ensures the
configuration tracks upstream Emacs evolution rather than fighting it.

### II. Nix-Native Packaging

All Emacs packages MUST be installed via Nix — never via MELPA/ELPA
at runtime when running under NixOS. `use-package` declarations with
`:ensure t` serve double duty: they configure packages in Elisp and
simultaneously declare them to Nix at build time via automatic
extraction in `nix/lib/dependencies.nix`.

A runtime detection variable `jotain/nix-managed-p` (checking for
`NIX_PROFILES` in the environment) MUST gate `package-archives` and
`use-package-always-ensure` so the configuration degrades gracefully
on non-Nix systems.

**Rationale:** Nix guarantees reproducible builds and binary caching.
The dual-purpose `use-package` pattern eliminates drift between the
Elisp config and the Nix package set. Graceful degradation lets others
try the config without adopting Nix.

### III. Modular Separation

The Elisp source MUST be organized into two distinct directories:

- **`lisp/`** — Built-in Emacs features configured via `use-package`
  with `:ensure nil`, and hand-written Elisp modules. Files are named
  `jotain-<feature>.el` where `<feature>` matches the Emacs domain
  being extended (e.g., `jotain-defaults`, `jotain-platform`).
  Everything under `lisp/` is assumed always available.
- **`modules/`** — Third-party package configurations only (`:ensure t`).

Each file MUST `(provide 'module-name)` and be loaded via
`(require 'module-name)` in `init.el`.

**Dependency policy:**
- `lisp/` modules MAY depend on any other `lisp/` module. Dependencies
  MUST be documented and SHOULD fail gracefully if missing.
- `modules/` MAY depend on anything under `lisp/`.
- `modules/` MUST NOT depend on other `modules/` files.

**Feature evaluation order:** When adding a capability, evaluate in
this order: (1) Is there a built-in Emacs feature? (2) Can we extend
the existing built-in? (3) Should we write our own `lisp/` module?
(4) Is there a justified third-party package for `modules/`?

**Rationale:** Clear separation makes it immediately obvious which
parts of the config depend on external packages and which are
self-contained. This aids auditing, simplifies the Nix build, and
keeps the built-in layer usable independently.

### IV. Personal Config, Not a Distribution

Jotain is an opinionated personal development environment. It is NOT
a distribution. It MUST NOT attempt to cover every user's needs, and
contributions that push it toward generality SHOULD be declined.

Users who want modifications SHOULD create a `private.el` file (loaded
after init via a hook) rather than forking the core config.

**Rationale:** Distributions must handle configuration, defaults, and
support for diverse workflows. Jotain exists to serve Markus's daily
personal and professional work. Keeping it personal allows aggressive
opinionation and fast iteration without consensus overhead.

### V. Test-First Development

New features and modules MUST have corresponding ERT tests before
merging. Tests live in `tests/` and are auto-discovered by
`tests/test-all.el`. Tests MUST be tagged (`smoke`, `fast`, `unit`,
`integration`, `slow`) and run in an isolated home (`HOME=.dev-home`).

The fastest feedback loop is `just test-tag TAG` for direct execution;
`just test-smoke` and `just test-fast` run Nix-sandboxed variants.
All tests MUST pass in CI before merge.

**Rationale:** Emacs Lisp configuration is notoriously fragile across
versions and package updates. Automated tests catch regressions early
and make refactoring safe. The tagging system enables sub-second
feedback during development.

### VI. Justified Complexity

Every third-party package MUST have documented justification (why the
built-in alternative is insufficient). Every abstraction MUST earn its
place — prefer three similar lines over a premature helper. New Nix
overlays, custom builds, and runtime dependencies each require
explicit rationale.

**Rationale:** Complexity is the primary enemy of a maintainable Emacs
config. Unjustified packages rot, unjustified abstractions confuse,
and unjustified Nix overrides break on upstream updates. The bar for
"worth it" MUST remain high.

### VII. Three-Audience Documentation

Documentation MUST serve three audiences:

1. **Markus six months from now** — module-level comments, commit
   messages, and architecture notes that explain *why*, not just *what*.
2. **NixOS Emacs users** — Home Manager integration docs, the
   `nix run` quick-try experience, and patterns worth adopting.
3. **Blog readers** — clear explanations of Emacs configuration
   architecture for the accompanying blog series.

The README MUST contain: a one-paragraph philosophy statement, the
directory tree with one-line module descriptions, Home Manager deploy
instructions, non-Nix usage instructions, and an architecture doc
link. The quick-start section MUST NOT exceed one screen of text.

**Rationale:** Documentation debt compounds. Writing for three
concrete audiences forces clarity and prevents both over-documentation
(nobody reads it) and under-documentation (nobody understands it).

## Technology Stack & Constraints

- **Emacs version:** Latest stable release (currently 30.2). Bleeding-edge
  features MAY be tested but MUST NOT be required for core functionality.
- **Build variant:** PGTK (pure GTK) via emacs-overlay.
- **Package management:** Nix Flakes with flake-parts and emacs-overlay.
  No runtime MELPA/ELPA under Nix.
- **Home Manager:** Exposed as `homeModules.default` with options for
  daemon, runtime deps, and extra packages.
- **Formatting:** `treefmt-nix` — `nixpkgs-fmt` for Nix, `shfmt` +
  `shellcheck` for shell, Emacs `indent-region` for Elisp.
- **Testing:** ERT framework, tagged tests, Nix-sandboxed and direct
  execution modes.
- **Name origin:** "Jotain" is Finnish for "something."

## Development Workflow

1. **Add a module:** Create the file in the appropriate directory
   (`lisp/` or `modules/`), add `(provide 'name)`, require it in
   `init.el`, and write ERT tests in `tests/`.
2. **Add a package:** Add `use-package` in the right `modules/*.el`
   file. If the name does not match nixpkgs, add a manual mapping in
   `nix/lib/dependencies.nix`. If not in nixpkgs, add a `trivialBuild`
   in `nix/overlays/default.nix`. Runtime tools go in
   `nix/lib/runtime-deps.nix`.
3. **Verify changes:** `just check-instant` for formatting + smoke,
   `just check-fast` to include fast unit tests, `just test` for the
   full suite. `just compile` for byte-compilation feedback.
4. **Test locally:** `just emacs-dev` runs Emacs with the project config
   in an isolated `.dev-home` directory.
5. **Document:** If adding a third-party package, document the
   justification inline. Update README if the module tree changes.

## Governance

This constitution is the authoritative guide for all development
decisions in jotain. When a proposed change conflicts with a principle
above, the principle wins unless the constitution is amended first.

**Amendment procedure:**
1. Propose the change with rationale in a PR or conversation.
2. Markus reviews and approves (sole maintainer authority).
3. Update this file with the change, bump the version, and set
   `Last Amended` to the amendment date.

**Versioning policy:** MAJOR for principle removals or redefinitions,
MINOR for new principles or material expansions, PATCH for
clarifications and wording fixes.

**Compliance review:** Before merging any feature, verify alignment
with principles I–VII. The plan template's "Constitution Check"
section is the gate for this.

**Version**: 1.1.0 | **Ratified**: 2026-03-23 | **Last Amended**: 2026-03-23
