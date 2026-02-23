# Feature Specification: Fix Baseline Spec from PR Review

**Feature Branch**: `002-fix-baseline-spec`
**Created**: 2026-02-23
**Status**: Draft
**Input**: Incorporate 6 corrections (5 owner review comments from PR #29 + 1 consistency fix from clarification) into the baseline specification and data model

## Scope

Update `specs/001-baseline/spec.md` and `specs/001-baseline/data-model.md`
to resolve factual errors and ambiguities identified during PR #29 review.

**In scope**: The 5 corrections identified by the repository owner in
inline review comments on PR #29, plus 1 additional consistency fix
(spec.md User Story 5 dark theme name) identified during clarification.

**Out of scope**: Structural changes to the spec, new user stories, new
requirements, or changes to any other spec artifacts (plan.md, tasks.md,
contracts, quickstart, research).

## Clarifications

### Session 2026-02-23 (PR #29 Review)

- Q: Is ad-hoc package loading during a session permitted? → A: Yes.
  Only persistent package installation across sessions is out of scope.
  Users may `M-x load-library` or `require` packages ad-hoc but these
  do not survive session restarts (Jylhis, spec.md:32).
- Q: Who is the target audience more precisely? → A: Generalist
  software engineers. Not limited to Emacs power users or Nix
  enthusiasts (Jylhis, spec.md:10).
- Q: Does User Story 6 cover only OS platforms? → A: No. It also
  covers usage modes: GUI (`emacsclient -c`) and terminal
  (`emacsclient -t`) (Jylhis, spec.md:217).
- Q: Should SC-005 specify an exact LSP server count? → A: No. The
  number is irrelevant. What matters is LSP coverage for all supported
  languages with sufficient confidence that things work (Jylhis,
  spec.md:354).
- Q: What is the correct dark theme name? → A: `doom-nord`, not `nord`.
  Source: `elisp/ui.el` line 17 defines `jotain-theme-dark` as
  `'doom-nord` (Jylhis, data-model.md:140).

### Session 2026-02-23 (Clarification)

- Q: Should spec.md User Story 5 line 199 "(nord)" also be corrected
  to "(doom-nord)"? → A: Yes. `doom-nord` and `nord` are distinct
  themes from different packages; using `nord` is factually incorrect,
  not merely informal.
- Q: Does "generalist software engineer" replace the existing audience
  bullets or supplement them? → A: It becomes the overarching primary
  persona; existing "Experienced Emacs users" and "Nix power users"
  bullets are kept as secondary sub-segments.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Correct Spec Inaccuracies (Priority: P1)

A contributor reads the baseline specification and encounters statements
that contradict the actual codebase or the owner's stated intent. They
need the spec to accurately reflect project decisions so they can rely
on it as a source of truth for future feature work.

**Why this priority**: The spec is the authoritative reference for all
future development. Inaccurate statements undermine trust and lead to
incorrect implementations.

**Independent Test**: Read each corrected section and verify it matches
the codebase and the owner's review comments verbatim.

**Acceptance Scenarios**:

1. **Given** the out-of-scope section in spec.md,
   **When** a contributor reads the package installation boundary,
   **Then** it clearly distinguishes persistent installation (out of
   scope) from ad-hoc session-only loading (permitted).

2. **Given** the target audience section in spec.md,
   **When** a contributor reads who Jotain is for,
   **Then** "generalist software engineer" is listed as the primary
   persona.

3. **Given** User Story 6 in spec.md,
   **When** a contributor reads the cross-platform story,
   **Then** it covers both GUI and terminal usage modes, not just
   operating system platforms.

4. **Given** SC-005 in spec.md,
   **When** a contributor reads the LSP success criterion,
   **Then** it specifies coverage for all supported languages rather
   than a fixed numeric count.

5. **Given** the Theme table in data-model.md,
   **When** a contributor reads the dark theme entry,
   **Then** it shows `doom-nord` (matching `elisp/ui.el:17`), not `nord`.

6. **Given** User Story 5 in spec.md,
   **When** a contributor reads the dark theme parenthetical,
   **Then** it says "(doom-nord)", not "(nord)".

---

### Edge Cases

- What happens if the corrected wording in spec.md contradicts the
  constitution? It MUST NOT. All corrections are consistent with
  Constitution v1.0.0. Principle I (Nix-Only Package Management)
  prohibits persistent runtime installation but does not address
  ad-hoc in-session loading.

- What happens if the data-model.md theme name change affects other
  spec artifacts? Yes — spec.md User Story 5 line 199 also uses
  "(nord)" which is factually incorrect (`doom-nord` ≠ `nord`).
  This is included as an additional correction (see Clarification
  session 2026-02-23).

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The out-of-scope list in spec.md MUST distinguish
  between persistent runtime package installation (prohibited) and
  ad-hoc session-only package loading (permitted).
- **FR-002**: The target audience in spec.md MUST list "generalist
  software engineer" as the overarching primary persona, with the
  existing "Experienced Emacs users" and "Nix power users" retained
  as secondary sub-segments.
- **FR-003**: User Story 6 in spec.md MUST cover GUI and terminal
  usage modes as acceptance scenarios, in addition to OS platform
  variants.
- **FR-004**: SC-005 in spec.md MUST define LSP success in terms of
  supported-language coverage, not a fixed server count.
- **FR-005**: The Theme entity table in data-model.md MUST list
  `doom-nord` as the dark theme Emacs theme name, matching the
  source code in `elisp/ui.el`.
- **FR-006**: The spec.md User Story 5 dark theme parenthetical
  MUST say "(doom-nord)", not "(nord)", consistent with FR-005.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All 6 corrections are applied and each corrected
  passage matches the owner's review comment intent word-for-word
  in meaning.
- **SC-002**: No other sections of spec.md or data-model.md are
  modified beyond the 6 targeted corrections.
- **SC-003**: The corrected spec passes the existing
  `specs/001-baseline/checklists/requirements.md` validation
  checklist with no new failures.
- **SC-004**: The corrected data-model.md Theme table matches
  the actual source code values in `elisp/ui.el`.
