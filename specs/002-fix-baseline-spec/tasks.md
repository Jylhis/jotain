# Tasks: Fix Baseline Spec from PR Review

**Input**: Design documents from `specs/002-fix-baseline-spec/`
**Prerequisites**: plan.md (required), spec.md (required), research.md

**Tests**: Not requested. Validation is by manual diff review.

**Organization**: Single user story (US1) with 6 targeted text edits across 2 files.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1)
- Include exact file paths in descriptions

---

## Phase 1: User Story 1 - Correct Spec Inaccuracies (Priority: P1) MVP

**Goal**: Apply 6 corrections to `specs/001-baseline/spec.md` and `specs/001-baseline/data-model.md` to match owner review intent from PR #29.

**Independent Test**: Diff each corrected section against the Edit Map in plan.md and verify each change matches the corresponding FR and PR #29 review comment.

### Implementation for User Story 1

- [x] T001 [US1] Replace out-of-scope package installation line in specs/001-baseline/spec.md line 32: change "Package installation at runtime from any source" to "Persistent package installation at runtime (MELPA/ELPA/package.el). Ad-hoc session-only loading via `M-x load-library` or `require` is permitted but does not persist across sessions" (FR-001)
- [x] T002 [US1] Restructure target audience in specs/001-baseline/spec.md lines 17-23: add "Generalist software engineers who want a batteries-included, Nix-managed Emacs distribution" as the first bullet (overarching primary persona), then indent existing "Experienced Emacs users" and "Nix power users" bullets as secondary sub-segments (FR-002)
- [x] T003 [US1] Add GUI and terminal acceptance scenarios to User Story 6 in specs/001-baseline/spec.md after line 243: add scenario 4 for GUI mode (`emacsclient -c` creates a new graphical frame connected to daemon) and scenario 5 for terminal mode (`emacsclient -t` creates a terminal frame connected to daemon) (FR-003)
- [x] T004 [US1] Replace LSP success criterion SC-005 in specs/001-baseline/spec.md lines 354-355: change "At least 7 LSP servers are available and functional out of the box when `includeRuntimeDeps` is enabled" to "LSP servers are available and functional for all supported languages (as defined in FR-008) when `includeRuntimeDeps` is enabled" (FR-004)
- [x] T005 [P] [US1] Fix dark theme name in specs/001-baseline/data-model.md line 140: change `nord` to `doom-nord` in the Theme table dark row (FR-005)
- [x] T006 [US1] Fix dark theme parenthetical in specs/001-baseline/spec.md line 199: change "(nord)" to "(doom-nord)" in User Story 5 acceptance scenario 1 (FR-006)

**Checkpoint**: All 6 corrections applied. Each edit should be verifiable against its FR in the spec and the corresponding PR #29 review comment.

---

## Phase 2: Validation

**Purpose**: Verify corrections are complete and no unintended changes were introduced.

- [x] T007 Review all 6 edits against the Edit Map in specs/002-fix-baseline-spec/plan.md to confirm each correction matches the specified before/after text
- [x] T008 Verify no other sections of specs/001-baseline/spec.md or specs/001-baseline/data-model.md were modified beyond the 6 targeted corrections (SC-002)
- [x] T009 Verify specs/001-baseline/data-model.md Theme table dark row matches `elisp/ui.el` line 17 defcustom value (SC-004)
- [x] T010 Re-validate specs/001-baseline/checklists/requirements.md checklist against the corrected spec to confirm no new failures (SC-003)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (US1)**: No dependencies — can start immediately
- **Phase 2 (Validation)**: Depends on all Phase 1 tasks being complete

### Within Phase 1

- T001 through T004 and T006 all edit `specs/001-baseline/spec.md` — execute sequentially
- T005 edits `specs/001-baseline/data-model.md` — can execute in parallel with any spec.md task
- Recommended order: T002 → T001 → T006 → T003 → T004 (spec.md, top-to-bottom by line number), T005 in parallel

### Parallel Opportunities

```text
# spec.md edits (sequential within same file, top-to-bottom by line):
T002 (L17) → T001 (L32) → T006 (L199) → T003 (L243) → T004 (L354)

# data-model.md edit (parallel with any spec.md task):
T005 [P]
```

---

## Implementation Strategy

### MVP (all tasks are MVP)

1. Apply all 6 edits (T001-T006)
2. Validate (T007-T009)
3. Complete — feature is a single atomic deliverable

### Single-Agent Execution

Execute T001-T004, T006 sequentially (same file, top-to-bottom by line number to avoid line-shift issues), then T005 (different file). Then T007-T009 for validation.

---

## Notes

- Edit spec.md corrections in top-to-bottom line order (T001:L32, T002:L17-23, T003:L243, T004:L354, T006:L199) to avoid line number shifts affecting later edits. Recommended execution: T002 (L17) → T001 (L32) → T006 (L199) → T003 (L243) → T004 (L354).
- T005 is the only task on a different file and is marked [P].
- Commit after all 6 edits are applied, not per-task, to keep the change atomic.
