# Tasks: Jotain Baseline Configuration

**Input**: Design documents from `specs/002-baseline-config/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, contracts/

**Tests**: Constitution Principle V mandates test-first development. Test tasks precede implementation.

**Organization**: Tasks follow the plan's phases, mapped to user stories.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1–US5)
- Include exact file paths in descriptions

---

## Phase 1: Setup (Directory Rename)

**Purpose**: Atomic rename from `elisp/` to `lisp/` + `modules/` — the structural foundation everything depends on.

- [x] T001 Rename `elisp/` directory to `lisp/` and remove stale `platform.elc` from it
- [x] T002 Create `modules/` directory with `.gitkeep` at repository root
- [x] T003 [P] Update `emacs.nix`: change `elispDir = ./elisp` to `./lisp` and add `./modules` scan path in `autoPackages`
- [x] T004 [P] Update `nix/package.nix`: change all `./../elisp` fileset references to `./../lisp`, add `./../modules` to fileset unions
- [x] T005 [P] Update `devenv.nix`: change `JOTAIN_ELISP_DIR` to `JOTAIN_LISP_DIR`, update symlink from `elisp` to `lisp` in emacs-dev script, add `modules` symlink
- [x] T006 [P] Update `justfile`: change load-path entries from `elisp` to `lisp`, add `modules` to load-path
- [x] T007 [P] Update `tests/test-smoke.el`: change `elisp-directory-exists` test to check `lisp` directory, update `emacs-version-minimum` threshold from `29.1` to `30`
- [x] T008 [P] Update `tests/test-helpers.el`: change load-path from `elisp` to `lisp` if referenced
- [x] T009 [P] Update `CLAUDE.md`: change all `elisp/` references to `lisp/` and document `modules/` directory
- [x] T010 [P] Update `.dir-locals.el`: change any `elisp` directory references to `lisp`

**Checkpoint**: `just test-tag smoke` passes with new directory structure. Nix build (`just build`) succeeds.

---

## Phase 2: Foundational (Proof-of-Life Modules + Init Wiring)

**Purpose**: Implement `jotain-defaults.el` and `jotain-platform.el`, wire `init.el` — BLOCKS all user stories.

**CRITICAL**: No user story work can begin until this phase is complete. Tests MUST be written before implementation (Constitution Principle V).

### Tests first

- [x] T011 [P] Write ERT tests for version check in `tests/test-defaults.el`: test warning message when Emacs version < 30, test no warning when >= 30 (tag: fast)
- [x] T012 [P] Write ERT tests for Nix detection in `tests/test-platform.el`: test `jotain/nix-managed-p` is t when `NIX_PROFILES` is set, nil when unset (tag: fast)
- [x] T013 [P] Write ERT tests for platform detection in `tests/test-platform.el`: test `jotain/platform` returns correct symbol for current system, test Termux detection via `TERMUX_VERSION` env var (tag: fast)
- [x] T014 [P] Write ERT tests for package archive config in `tests/test-platform.el`: test non-Nix mode configures MELPA + GNU ELPA and `use-package-always-ensure` t; test Nix mode leaves archives nil (tag: fast)
- [x] T015 [P] Write ERT tests for module loading in `tests/test-modules.el`: test that `lisp/` and `modules/` are on `load-path`, test that a mock module with `(provide 'test-dummy)` can be loaded (tag: fast)
- [x] T016 [P] Write ERT tests for private.el in `tests/test-modules.el`: test `private.el` loads when present via `after-init-hook`, test errors in `private.el` are caught without preventing startup (tag: fast)
- [x] T017 Update `tests/test-suite-fast.el`: add loading of `test-defaults.el`, `test-platform.el`, and `test-modules.el`

### Implementation (after tests exist)

- [x] T018 Create `lisp/jotain-defaults.el`: Emacs version check (warn if < 30), sensible editor defaults, `(provide 'jotain-defaults)`
- [x] T019 Create `lisp/jotain-platform.el`: `jotain/nix-managed-p` (check `NIX_PROFILES`), `jotain/platform` detection (system-type + system-configuration + `TERMUX_VERSION`), package archive configuration (conditional on Nix status), `(provide 'jotain-platform)` — depends on `jotain-defaults` (documented)
- [x] T020 Update `early-init.el`: make `package-enable-at-startup`, `use-package-always-ensure`, and `use-package-ensure-function` conditional on `NIX_PROFILES` env check (runs before modules)
- [x] T021 Update `init.el`: add `lisp/` and `modules/` to `load-path`, `(require 'jotain-defaults)` then `(require 'jotain-platform)`, remove hardcoded `package-archives nil`, load `private.el` via `after-init-hook` with `condition-case` error handling
- [x] T022 Update `nix/package.nix` `installPhase`: copy `lisp/` and `modules/` directories into `$out/share/jotain/` alongside init files
- [x] T023 Add `no-littering` justification comment in `emacs.nix` where it is declared in `corePackages`: document why the built-in alternative is insufficient (FR-010)

**Checkpoint**: `just test-tag smoke` and `just test-tag fast` pass. `jotain/nix-managed-p` and `jotain/platform` set correctly. `init.el` loads without errors.

---

## Phase 3: User Story 1 — Launch Emacs with Working Configuration (Priority: P1)

**Goal**: Emacs starts error-free on all platforms with correct Nix/non-Nix behavior.

**Independent Test**: Launch Emacs; verify init.el completes without errors; verify Nix detection is correct.

### Implementation for US1

- [x] T024 [US1] Add smoke test in `tests/test-smoke.el`: verify `jotain-defaults` and `jotain-platform` features are present after loading (tag: smoke)
- [x] T025 [US1] Verify `init.el` loads without errors by running `emacs --batch -l init.el` and checking exit code (manual validation step, document in quickstart.md)

**Checkpoint**: Emacs launches without errors. `jotain/nix-managed-p` reflects environment. Package archives configured correctly per mode.

---

## Phase 4: User Story 2 — Modular Feature Organization (Priority: P2)

**Goal**: Module isolation works — removing any module degrades gracefully.

**Independent Test**: Add a dummy module then remove it; verify graceful behavior.

### Implementation for US2

- [x] T026 [US2] Verify `init.el` load order is deterministic — add comments documenting the expected load sequence and dependency policy (`lisp/` modules may depend on each other, `modules/` may depend on `lisp/`)
- [x] T027 [US2] Verify graceful degradation: temporarily remove `jotain-platform` require from `init.el`, confirm `jotain-defaults` still loads, then restore

**Checkpoint**: Module loading works. Dependencies documented. Graceful degradation confirmed.

---

## Phase 5: User Story 3 — Nix and Home Manager Integration (Priority: P3)

**Goal**: Home Manager deployment works with `programs.jotain.enable = true`.

**Independent Test**: Build a Home Manager generation with jotain enabled; verify Emacs is available with packages.

### Implementation for US3

- [x] T028 [US3] Update `nix/modules/home/default.nix`: add XDG config symlinks/copies for `lisp/` and `modules/` directories from the jotain package
- [x] T029 [US3] Update `nix/overlays/default.nix`: verify `pkgs.jotain` and `pkgs.jotainEmacs` work with new directory structure
- [x] T030 [US3] Verify all five HM options (enable, package, enableDaemon, includeRuntimeDeps, extraPackages) function correctly after directory rename — run `nix build .#default` and inspect output

**Checkpoint**: `nix build .#default` succeeds. Home Manager module references correct paths. All five options verified.

---

## Phase 6: User Story 4 — Non-Nix Portable Usage (Priority: P4)

**Goal**: Clone-and-launch works on non-Nix Linux and Android/Termux.

**Independent Test**: On a non-Nix system, place config files and launch Emacs.

### Implementation for US4

- [x] T031 [US4] Update `lisp/jotain-platform.el`: add Termux/Android-specific graceful degradation (skip GUI-only features, adjust paths if needed)
- [x] T032 [US4] Document non-flake Nix usage in `quickstart.md`: how to use jotain without flakes enabled

**Checkpoint**: Platform detection correct for all three targets. Non-Nix fallback installs packages. Non-flake usage documented.

---

## Phase 7: User Story 5 — Documentation (Priority: P5)

**Goal**: README and architecture doc serve three audiences.

**Independent Test**: Read README; verify it has philosophy, tree, HM instructions, non-Nix instructions, architecture link, and quick-start under 40 lines.

### Implementation for US5

- [x] T033 [P] [US5] Write `README.md` at repository root: philosophy paragraph, directory tree with one-line module descriptions, Home Manager quick-start, non-Nix quick-start (both flake and non-flake), architecture doc link — quick-start section under 40 lines
- [x] T034 [P] [US5] Write `docs/architecture.md`: detailed architecture document covering module system (naming convention, dependency policy, feature evaluation order), Nix integration (flake + non-flake), package extraction, test infrastructure, and directory structure — targeting all three audiences

**Checkpoint**: README quick-start < 40 lines. Architecture doc covers module system, Nix flow, and test setup.

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final validation and cleanup across all stories.

- [x] T035 Run `just format` to ensure all files pass treefmt formatting
- [x] T036 Run `just compile` to byte-compile all `.el` files and fix any warnings
- [x] T037 Run `just test-fast` (Nix-sandboxed) to verify all smoke + fast tests pass
- [x] T038 Run `just build` to verify the Nix package builds correctly
- [x] T039 Measure startup time with `emacs --batch --eval '(message "%s" (emacs-init-time))'` and verify < 3s (SC-001 validation)
- [x] T040 Review and update `CLAUDE.md` to reflect final directory structure, commands, and architecture

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies — can start immediately
- **Foundational (Phase 2)**: Depends on Phase 1 — BLOCKS all user stories
- **US1 (Phase 3)**: Depends on Phase 2 — MVP deliverable
- **US2 (Phase 4)**: Depends on Phase 2 — validates modularity
- **US3 (Phase 5)**: Depends on Phase 2 + T022 (package.nix changes)
- **US4 (Phase 6)**: Depends on Phase 2
- **US5 (Phase 7)**: Depends on Phases 3–6 (documents final state)
- **Polish (Phase 8)**: Depends on all previous phases

### User Story Dependencies

- **US1 (P1)**: Depends on Foundational — Nix/non-Nix behavior
- **US2 (P2)**: Depends on Foundational — module loading mechanics
- **US3 (P3)**: Depends on T022 (package.nix) — Nix package must include lisp/modules dirs
- **US4 (P4)**: Depends on Foundational — platform detection and non-Nix fallback
- **US5 (P5)**: Depends on all stories — documents the final state

### Parallel Opportunities

- Phase 1: T003–T010 are all [P] (different files)
- Phase 2 tests: T011–T016 are all [P] (different test files)
- Phase 3–4: US1 and US2 can proceed in parallel after Phase 2
- Phase 5–6: US3 and US4 can proceed in parallel
- Phase 7: T033 and T034 are [P] (different files)

---

## Implementation Strategy

### MVP First (US1 Only)

1. Complete Phase 1: Directory Rename
2. Complete Phase 2: Proof-of-Life Modules + Init Wiring
3. Complete Phase 3: US1 (Launch works)
4. **STOP and VALIDATE**: Emacs launches, Nix detection works, tests pass
5. This is the minimum viable deliverable

### Incremental Delivery

1. Phase 1 + 2 → Framework ready
2. + Phase 3 (US1) → Emacs launches correctly (MVP)
3. + Phase 4 (US2) → Module isolation validated
4. + Phase 5 (US3) → Home Manager deployment works
5. + Phase 6 (US4) → Non-Nix and Android/Termux work
6. + Phase 7 (US5) → Documentation complete
7. + Phase 8 → Polish, all tests green

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story
- Tests are written BEFORE implementation per Constitution Principle V
- `lisp/` modules named after the Emacs feature they extend (`jotain-<feature>.el`)
- `lisp/` modules may depend on each other; `modules/` may depend on `lisp/` only
- Commit after each phase or logical group
- Stop at any checkpoint to validate independently
