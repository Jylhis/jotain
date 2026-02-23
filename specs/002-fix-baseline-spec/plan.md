# Implementation Plan: Fix Baseline Spec from PR Review

**Branch**: `002-fix-baseline-spec` | **Date**: 2026-02-23 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `specs/002-fix-baseline-spec/spec.md`

## Summary

Apply 6 targeted text corrections to `specs/001-baseline/spec.md` and
`specs/001-baseline/data-model.md` based on owner review comments from
PR #29 and one consistency fix identified during clarification. No code
changes, no new tests, no build system modifications.

## Technical Context

**Language/Version**: Markdown (prose edits only)
**Primary Dependencies**: None
**Storage**: N/A
**Testing**: Manual diff review against PR #29 comments
**Target Platform**: N/A (documentation)
**Project Type**: Documentation correction
**Performance Goals**: N/A
**Constraints**: Edits MUST be scoped to the 6 identified corrections only
**Scale/Scope**: 2 files, 6 edit locations

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

| Principle | Applicable | Status | Notes |
|-----------|------------|--------|-------|
| I. Nix-Only Package Management | Yes | ✅ Pass | FR-001 clarifies ad-hoc loading is permitted; this is consistent — Principle I prohibits persistent runtime installation via MELPA/ELPA, not ad-hoc `require`/`load-library` |
| II. Modular Elisp Architecture | No | ✅ N/A | No code changes |
| III. Reproducibility First | No | ✅ N/A | No build changes |
| IV. Testing Discipline | No | ✅ N/A | No new functionality; spec text corrections don't require ERT/NMT tests |
| V. Platform Portability | No | ✅ N/A | No code changes |
| VI. Performance by Design | No | ✅ N/A | No code changes |
| VII. Simplicity & YAGNI | Yes | ✅ Pass | 6 minimal, targeted edits; no unnecessary changes |

**Gate result**: PASS — no violations.

## Project Structure

### Documentation (this feature)

```text
specs/002-fix-baseline-spec/
├── plan.md              # This file
├── research.md          # Phase 0 output (no research needed)
├── spec.md              # Feature specification
└── checklists/
    └── requirements.md  # Spec quality checklist
```

### Target Files (edits applied to)

```text
specs/001-baseline/
├── spec.md              # 5 edit locations (FR-001 through FR-004, FR-006)
└── data-model.md        # 1 edit location (FR-005)
```

### Edit Map

| FR | File | Location | Current Text | Corrected Text |
|----|------|----------|-------------|----------------|
| FR-001 | spec.md | Line 32 | `Package installation at runtime from any source` | `Persistent package installation at runtime (MELPA/ELPA/package.el). Ad-hoc session-only loading via M-x load-library or require is permitted but does not persist across sessions` |
| FR-002 | spec.md | Lines 17-23 | Target audience lists only "Experienced Emacs users" and "Nix power users" | Add "Generalist software engineers" as overarching primary persona; retain existing bullets as secondary sub-segments |
| FR-003 | spec.md | Lines 215-243 | User Story 6 covers only OS platforms (Linux, macOS, Android) | Add acceptance scenarios for GUI mode (`emacsclient -c`) and terminal mode (`emacsclient -t`) |
| FR-004 | spec.md | Lines 354-355 | `At least 7 LSP servers are available and functional out of the box` | `LSP servers are available and functional for all supported languages (as defined in FR-008) when includeRuntimeDeps is enabled` |
| FR-005 | data-model.md | Line 140 | `dark \| nord \| jotain-theme-dark \| C-c t` | `dark \| doom-nord \| jotain-theme-dark \| C-c t` |
| FR-006 | spec.md | Line 199 | `dark theme (nord)` | `dark theme (doom-nord)` |

## Complexity Tracking

No constitution violations to justify. Table intentionally empty.
