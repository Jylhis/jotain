# Implementation Plan: Jotain Baseline Configuration

**Spec Directory**: `002-baseline-config` | **Date**: 2026-03-23 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `specs/002-baseline-config/spec.md`

## Summary

Deliver the architectural framework for jotain: rename `elisp/` to `lisp/` + `modules/`, implement proof-of-life modules named after the Emacs features they extend (`jotain-defaults`, `jotain-platform`), wire `init.el` with module loading and `private.el` support, update all Nix build files for the new directory structure, write ERT tests for every new capability, and produce README + architecture documentation. The Nix integration exposes a flake interface but also supports non-flake usage (e.g., devenv).

## Technical Context

**Language/Version**: Emacs Lisp (Emacs 30.2), Nix (2.31.3) # REVIEW: nix version
**Primary Dependencies**: use-package (built-in to Emacs 29+), no-littering (third-party, justified: prevents config dir pollution)
**Storage**: Files (Emacs configuration directory, Nix store)
**Testing**: ERT (Emacs Lisp Regression Testing), tagged tests, Nix-sandboxed and direct execution
**Target Platform**: Linux x86_64, Linux aarch64, Android aarch64 (Termux, nix-on-droid)
**Project Type**: Emacs configuration + Nix (NOTE: we expose things as nix flake, but we also support non-flake)
**Performance Goals**: < 3s startup time
**Constraints**: No runtime MELPA/ELPA under Nix; Emacs 30.2 minimum; lisp/ modules may depend on each other freely
**Scale/Scope**: Single-user configuration; baseline delivers framework + proof-of-life modules (jotain-defaults, jotain-platform)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Status | Evidence |
|-----------|--------|----------|
| I. Built-in First | PASS | Core module uses only Emacs built-ins (`:ensure nil`). No third-party packages in `lisp/`. |
| II. Nix-Native Packaging | PASS | `jotain/nix-managed-p` gates package archives. `dependencies.nix` auto-extracts from `modules/`. Graceful non-Nix fallback. |
| III. Modular Separation | PASS | `lisp/` for built-ins, `modules/` for third-party. Each file provides + requires. |
| IV. Personal Config | PASS | `private.el` loaded via hook. Not a distribution. |
| V. Test-First | PASS | ERT tests for core init, Nix detection, module loading, platform detection. Tagged smoke/fast/unit. |
| VI. Justified Complexity | PASS | Only `no-littering` as third-party (prevents `~/.emacs.d` pollution). Documented justification required for any addition. |
| VII. Three-Audience Docs | PASS | README with philosophy, tree, HM deploy, non-Nix usage, architecture link. |

## Project Structure

### Documentation (this feature)

```text
specs/002-baseline-config/
├── plan.md              # This file
├── spec.md              # Feature specification
├── research.md          # Phase 0 decisions
├── data-model.md        # Entity definitions
├── contracts/           # Interface contracts
│   └── module-contract.md
└── quickstart.md        # Quick-start guide
```

### Source Code (repository root)

```text
# Elisp configuration — files named after the Emacs feature they extend
lisp/                        # Built-in Emacs features extended (replaces elisp/)
├── jotain-defaults.el       # Sensible defaults: version check, basic settings
└── jotain-platform.el       # Platform/environment: Nix detection, OS, arch, Termux

modules/                     # Third-party packages (new, empty for baseline)
└── .gitkeep

# Init files (modified)
early-init.el                # Updated: conditional Nix behavior
init.el                      # Updated: module loading, load-path, private.el

# Nix build system (modified)
nix/package.nix              # Updated: install lisp/ and modules/
emacs.nix                    # Updated: scan lisp/ instead of elisp/
devenv.nix                   # Updated: paths from elisp/ to lisp/

# Tests (modified + new)
tests/
├── test-helpers.el          # Updated: load-path for lisp/
├── test-smoke.el            # Updated: check lisp/ dir, version threshold
├── test-defaults.el         # NEW: defaults, version check tests
├── test-platform.el         # NEW: Nix detection, platform detection tests
└── test-modules.el          # NEW: module loading, private.el tests

# Documentation (new)
README.md                    # Philosophy, tree, deploy instructions
docs/
└── architecture.md          # Architecture document for three audiences
```

**Structure Decision**: The directory rename from `elisp/` to `lisp/` + `modules/` is the foundational change. All Nix files, tests, devenv, and justfile references must be updated atomically. The `lisp/` directory holds proof-of-life modules named after the Emacs features they extend; `modules/` starts empty (with `.gitkeep`) since no third-party packages are in baseline scope.

## Implementation Phases

### Phase 1: Directory Rename (`elisp/` → `lisp/` + `modules/`)

This is the atomic structural change that everything else depends on.

**Files to modify:**
- Rename `elisp/` directory to `lisp/`
- Create `modules/` with `.gitkeep`
- `nix/package.nix`: Change all `elisp` references to `lisp`, add `modules` to fileset
- `emacs.nix`: Change `elispDir = ./elisp` to `./lisp`, add `./modules` scan
- `devenv.nix`: Update `JOTAIN_ELISP_DIR` → `JOTAIN_LISP_DIR`, update symlinks
- `justfile`: Update load-path from `elisp` to `lisp`
- `tests/test-smoke.el`: Update directory existence check
- `tests/test-helpers.el`: Update load-path if referenced
- `CLAUDE.md`: Update directory references
- `.dir-locals.el`: Update if paths referenced

### Phase 2: Proof-of-Life Modules

Modules named after the Emacs features they extend, validating the
framework end-to-end.

**`lisp/jotain-defaults.el`** — extends Emacs defaults:
1. **Version check**: Warn if Emacs < 30
2. **Sensible defaults**: Basic editor settings that improve the out-of-box experience

**`lisp/jotain-platform.el`** — extends Emacs environment detection:
1. **Nix detection**: Set `jotain/nix-managed-p` by checking `NIX_PROFILES` env var
2. **Platform detection**: Detect OS, architecture, Android/Termux environment
3. **Package archive configuration**: When `jotain/nix-managed-p` is nil, configure MELPA + GNU ELPA and set `use-package-always-ensure t`

### Phase 3: Init Files (`early-init.el` + `init.el`)

**`early-init.el` changes:**
- Move Nix-specific hardcoded settings behind `NIX_PROFILES` env check
- Keep the early UI optimizations and native-comp settings as-is

**`init.el` changes:**
- Add `lisp/` and `modules/` to `load-path`
- `(require 'jotain-defaults)` then `(require 'jotain-platform)`
- Load `private.el` via `after-init-hook` if it exists
- Remove hardcoded `package-archives nil`; let `jotain-platform` handle it

### Phase 4: Tests

**New test files:**
- `tests/test-defaults.el`: Version check behavior
- `tests/test-platform.el`: `jotain/nix-managed-p` under both values, platform detection, package archive configuration
- `tests/test-modules.el`: Module loading from `lisp/`, `private.el` hook

**Updated tests:**
- `tests/test-smoke.el`: Check `lisp/` directory exists (not `elisp/`), update version threshold to >= 30

### Phase 5: Documentation

- `README.md`: Philosophy, directory tree, Home Manager quick-start, non-Nix quick-start, architecture link
- `docs/architecture.md`: Detailed architecture for three audiences

## Complexity Tracking

No constitution violations to justify. All decisions align with principles I–VII.

## Key Decisions

1. **Directory rename is atomic**: All references change in one coordinated step to avoid broken intermediate states.
2. **`jotain/nix-managed-p` is the single source of truth**: Both `early-init.el` and `jotain-platform.el` check `NIX_PROFILES`. `early-init.el` checks directly (runs before modules); `jotain-platform.el` re-exposes the variable for use by other modules.
3. **Modules named after features**: No monolithic "core" module. Each `lisp/` file is named `jotain-<feature>.el` matching the Emacs domain it extends, following emacs-solo naming conventions.
4. **`modules/` starts empty**: No third-party packages in baseline scope per spec clarification.
5. **`lisp/` dependency freedom**: All `lisp/` modules may depend on each other (assumed always available). Dependencies documented, graceful degradation if missing.
6. **Flake + non-flake support**: The Nix integration exposes a flake interface but everything also works without flakes. devenv is non-flake.
7. **Feature evaluation order**: Built-in → extend existing → own lisp/ module → third-party modules/ package.
