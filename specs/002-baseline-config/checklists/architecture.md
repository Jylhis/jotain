# Architecture Requirements Quality Checklist: Jotain Baseline Configuration

**Purpose**: Validate that architecture, modularity, and cross-platform requirements are complete, clear, and consistent before implementation
**Created**: 2026-03-23
**Feature**: [spec.md](../spec.md)

## Requirement Completeness

- [ ] CHK001 Are the exact directories for built-in vs. third-party modules named and defined? [Completeness, Spec §FR-005, §FR-006]
- [ ] CHK002 Are module file naming conventions specified (e.g., prefix, suffix, case)? [Gap]
- [ ] CHK003 Are the specific package archives to configure on non-Nix systems enumerated? [Gap, Spec §FR-004]
- [ ] CHK004 Is the content of the proof-of-life core module specified beyond "core defaults and platform detection"? [Completeness, Spec §FR-017]
- [ ] CHK005 Are the Home Manager module option types, defaults, and validation rules documented for all five options? [Completeness, Spec §FR-011]
- [ ] CHK006 Are requirements defined for what the NixOS module (not just Home Manager) provides? [Gap]
- [ ] CHK007 Is the `private.el` loading hook explicitly named (e.g., `after-init-hook` vs. `emacs-startup-hook`)? [Gap, Spec §FR-009]
- [ ] CHK008 Are requirements for the `nix run` quick-try experience defined? [Gap]

## Requirement Clarity

- [ ] CHK009 Is "graceful degradation" on unsupported platforms quantified — which features degrade and how? [Ambiguity, Spec §Edge Cases]
- [ ] CHK010 Is "clear warning" for old Emacs versions defined — message format, visibility, blocking vs. non-blocking? [Ambiguity, Spec §FR-015]
- [ ] CHK011 Is "usable state" in SC-001 (< 3s startup) defined — what constitutes "usable"? [Ambiguity, Spec §SC-001]
- [ ] CHK012 Is "one screen" (~40 lines) a hard limit or a guideline for the README? [Clarity, Spec §FR-014]
- [ ] CHK013 Does "no network package fetching" under Nix include elpa-mirror or any local archive? [Clarity, Spec §FR-003]
- [ ] CHK014 Is "documented justification" for third-party packages defined — inline comment, separate doc, or both? [Ambiguity, Spec §FR-010]

## Requirement Consistency

- [ ] CHK015 Does the constitution's module dependency rule ("modules MAY depend on earlier ones") align with the spec's single-root DAG constraint? [Conflict, Spec §FR-008 vs. Constitution §III]
- [ ] CHK016 Are the supported platforms consistent between spec (3 platforms) and test requirements (SC-002 references all three)? [Consistency, Spec §FR-001, §SC-002]
- [ ] CHK017 Is the Emacs version requirement consistent across spec (30.2), constitution (30.2), and tests (smoke test checks >= 29.1)? [Conflict, Spec §FR-001 vs. test-smoke.el]

## Acceptance Criteria Quality

- [ ] CHK018 Can SC-001 (< 3s startup) be measured without specifying hardware baseline? [Measurability, Spec §SC-001]
- [ ] CHK019 Can SC-003 (< 5 minutes clone-to-working) be verified reproducibly given network variability? [Measurability, Spec §SC-003]
- [ ] CHK020 Can SC-005 (module isolation) be objectively tested — is there a defined procedure for "removing a module"? [Measurability, Spec §SC-005]
- [ ] CHK021 Can SC-006 (README comprehensible in < 2 min) be objectively measured? [Measurability, Spec §SC-006]

## Scenario Coverage

- [ ] CHK022 Are requirements defined for first-launch behavior on a non-Nix system with no network? [Coverage, Exception Flow]
- [ ] CHK023 Are requirements defined for the case where Nix-built Emacs version doesn't match the minimum? [Coverage, Edge Case]
- [ ] CHK024 Are requirements specified for running jotain alongside an existing Emacs configuration? [Coverage, Alternate Flow]
- [ ] CHK025 Are requirements defined for upgrading from one jotain version to another? [Gap, Recovery Flow]

## Edge Case Coverage

- [ ] CHK026 Does the spec define behavior when `NIX_PROFILES` is set but the system is not actually NixOS (e.g., standalone Nix on Ubuntu)? [Edge Case, Spec §FR-002]
- [ ] CHK027 Are requirements specified for when tree-sitter grammars are unavailable (TREE_SITTER_DIR unset)? [Edge Case, Gap]
- [ ] CHK028 Does the spec define behavior when `modules/` directory is missing entirely? [Edge Case, Gap]
- [ ] CHK029 Are requirements defined for when multiple `private.el` errors occur (cascading failures)? [Edge Case, Spec §FR-009]

## Dependencies & Assumptions

- [ ] CHK030 Is the assumption that Android target means Termux validated — are other Android Emacs environments excluded? [Assumption, Spec §Assumptions]
- [ ] CHK031 Is the dependency on emacs-overlay documented as a hard requirement vs. optional? [Dependency, Gap]
- [ ] CHK032 Is the `no-littering` package dependency justified inline per FR-010? [Dependency, Spec §FR-010]

## Notes

- Focus areas: architecture (modularity, Nix/non-Nix dual-mode) and cross-platform (three targets, degradation)
- Depth: Standard
- Audience: Author (self-review before implementation)
- Items CHK015 and CHK017 flag potential conflicts that should be resolved before implementation
