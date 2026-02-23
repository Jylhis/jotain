# Tasks: Jotain Baseline

**Input**: Design documents from `/specs/001-baseline/`
**Prerequisites**: plan.md (required), spec.md (required), research.md, data-model.md, contracts/

**Context**: This is a baseline specification. The code already exists. Tasks focus on validating the existing system against the spec, filling identified gaps, and ensuring test coverage meets the constitution (Principle IV: Testing Discipline).

**Organization**: Tasks are grouped by user story to enable independent validation and gap-filling for each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

---

## Phase 1: Setup (Validation Infrastructure)

**Purpose**: Verify the project structure matches the plan and all baseline artifacts are in place.

- [x] T001 Verify all 17 Elisp modules exist in elisp/ and each has `lexical-binding: t` header
- [x] T002 [P] Verify init.el loads modules in the order documented in plan.md
- [x] T003 [P] Verify early-init.el disables GUI elements and configures native compilation
- [x] T004 [P] Verify flake.nix exposes all documented outputs (packages, homeModules, nixosModules, overlays, checks)
- [x] T005 [P] Verify nix/lib/dependencies.nix auto-extracts packages from all elisp/*.el files without errors

**Checkpoint**: Project structure validated — all files exist and are correctly configured.

---

## Phase 2: Foundational (Build System & Core Infrastructure)

**Purpose**: Validate the Nix build system and Home Manager contract produce correct outputs. MUST complete before user story validation.

**CRITICAL**: No user story validation can begin until this phase confirms the build works.

- [x] T006 Run `nix build` and verify the jotain package builds successfully from flake.nix
- [x] T007 Run `nix build .#emacs` and verify the Emacs derivation includes all wrapped dependencies
- [x] T008 [P] Verify nix/lib/runtime-deps.nix declares all LSP servers listed in the contract (nil, gopls, bash-language-server, typescript-language-server, clangd, marksman, yaml-language-server, dockerfile-language-server)
- [x] T009 [P] Verify nix/lib/runtime-deps.nix declares all CLI tools listed in the contract (ripgrep, fd, direnv)
- [x] T010 [P] Verify nix/lib/runtime-deps.nix declares all fonts listed in the contract (JetBrainsMono, FiraCode, Iosevka, CascadiaCode, Hack, Inter, Source Serif Pro, Liberation Serif, Noto Color Emoji)
- [x] T011 [P] Verify nix/modules/home/default.nix exposes all 4 options documented in contracts/home-manager-module.md (enable, enableDaemon, includeRuntimeDeps, extraPackages)
- [x] T012 [P] Verify nix/modules/nixos/default.nix installs system packages and fonts (FR-015) in nix/modules/nixos/default.nix
- [x] T013 Run `just format` (dry-run/check mode) and confirm formatting passes for Nix, shell, and Elisp files (constitution Quality Gate 1)
- [x] T014 Run `just compile` and confirm byte-compilation produces no errors (constitution Local Dev)
- [x] T015 Run `just test-smoke` and confirm all smoke tests pass in under 1 second
- [x] T016 Run `just test-fast` and confirm all fast tests pass in under 5 seconds
- [x] T017 Run `just test` and confirm the full ERT suite passes
- [x] T018 Run `just test-nmt` and confirm all 5 NMT test cases pass

**Checkpoint**: Build system and test infrastructure validated — user story validation can begin.

---

## Phase 3: User Story 1 — Install and Launch Emacs (Priority: P1)

**Goal**: Validate that a fresh Jotain installation via Home Manager produces a working Emacs environment with all modules loaded, daemon running, and runtime deps available.

**Independent Test**: Enable the Home Manager module, rebuild, and launch Emacs to confirm it starts without errors with a dashboard greeting screen.

### Validation for User Story 1

- [x] T019 [US1] Verify test-init-loads.el confirms init.el and early-init.el load without errors in tests/test-init-loads.el
- [x] T020 [US1] Verify test-init-loads.el confirms all config modules load successfully in tests/test-init-loads.el
- [x] T021 [US1] Verify test-init-loads.el confirms package-archives is nil (FR-002) in tests/test-init-loads.el
- [x] T022 [P] [US1] Verify test-runtime-deps.el confirms tree-sitter directory and grammars are available in tests/test-runtime-deps.el
- [x] T023 [P] [US1] Verify test-runtime-deps.el confirms CLI tools (rg, fd, direnv) are on PATH in tests/test-runtime-deps.el
- [x] T024 [P] [US1] Verify test-runtime-deps.el confirms LSP servers (nil, gopls, clangd, marksman, etc.) are on PATH in tests/test-runtime-deps.el
- [x] T025 [P] [US1] Verify elisp/dashboard.el configures Enlight with menu items for startup screen (US1 acceptance scenario 1) in elisp/dashboard.el
- [x] T026 [US1] Verify NMT test-module-enabled validates config files deployed to .config/emacs/ in nmt-tests/default.nix
- [x] T027 [US1] Verify NMT test-module-enabled validates systemd service and socket created in nmt-tests/default.nix
- [x] T028 [US1] Verify NMT test-module-enabled validates EDITOR/VISUAL set to emacsclient in nmt-tests/default.nix

**Checkpoint**: User Story 1 validated — installation produces a working Emacs environment.

---

## Phase 4: User Story 2 — Code with Modern Completion and LSP (Priority: P1)

**Goal**: Validate that the completion system (Vertico, Corfu, Consult, Orderless) and LSP integration (Eglot) are correctly configured and tested.

**Independent Test**: Open a Go or Nix file, verify completions appear, diagnostics show, and navigation works.

### Validation for User Story 2

- [x] T029 [P] [US2] Verify test-completion.el covers Vertico configuration and availability in tests/test-completion.el
- [x] T030 [P] [US2] Verify test-completion.el covers Corfu configuration and availability in tests/test-completion.el
- [x] T031 [P] [US2] Verify test-completion.el covers Consult keybindings and configuration in tests/test-completion.el
- [x] T032 [P] [US2] Verify test-completion.el covers Orderless completion styles in tests/test-completion.el
- [x] T033 [P] [US2] Verify test-completion.el covers Embark and Marginalia availability in tests/test-completion.el
- [x] T034 [US2] Verify test-programming.el covers Eglot server program configuration for Nix, Go, TypeScript, YAML, Docker, Markdown, C/C++ (FR-008) in tests/test-programming.el
- [x] T035 [P] [US2] Verify test-programming.el covers Flymake diagnostic configuration in tests/test-programming.el
- [x] T036 [P] [US2] Verify test-programming.el covers tree-sitter auto-mode registration in tests/test-programming.el
- [x] T037 [US2] Verify test-use-package-ensure.el confirms external packages use :ensure t and built-ins use :ensure nil (FR-002) in tests/test-use-package-ensure.el

**Checkpoint**: User Story 2 validated — completion and LSP systems are correctly configured.

---

## Phase 5: User Story 3 — Manage Git Repositories (Priority: P2)

**Goal**: Validate that Magit is configured with repository discovery, worktree support, and diff-hl integration.

**Independent Test**: Open Magit status in a Git repository, stage a file, create a commit.

### Validation for User Story 3

- [x] T038 [US3] Verify test-git-worktrees.el confirms Magit worktrees hook is installed in tests/test-git-worktrees.el
- [x] T039 [P] [US3] Verify elisp/git.el configures magit-repository-directories to ~/Developer with depth 1 (FR-013) in elisp/git.el
- [x] T040 [P] [US3] Verify elisp/git.el configures C-c g keybinding for magit-status in elisp/git.el
- [x] T041 [P] [US3] Verify elisp/git.el configures diff-hl with flydiff mode in elisp/git.el

**Checkpoint**: User Story 3 validated — Git integration is correctly configured.

---

## Phase 6: User Story 4 — Write and Organise with Org-mode (Priority: P2)

**Goal**: Validate that Org-mode is configured with mixed-pitch fonts, dynamic agenda discovery, and exporters.

**Independent Test**: Create an Org file, verify styling, and check agenda file discovery.

### Validation for User Story 4

- [x] T042 [US4] Verify elisp/writing.el configures org-directory to ~/Documents (FR-014) in elisp/writing.el
- [x] T043 [P] [US4] Verify elisp/writing.el configures mixed-pitch font setup function (my/setup-org-fonts) in elisp/writing.el
- [x] T044 [P] [US4] Verify elisp/writing.el configures org exporters (ox-slack, ox-jira, ox-hugo, ox-gfm) with :ensure t in elisp/writing.el
- [x] T045 [US4] Verify test-utils.el covers recursive org file discovery function (my/find-org-files-recursively) in tests/test-utils.el
- [x] T046 [US4] Verify test-utils.el covers agenda file updating function (my/update-org-agenda-files) in tests/test-utils.el

**Checkpoint**: User Story 4 validated — Org-mode writing workflow is correctly configured.

---

## Phase 7: User Story 5 — Switch Themes and Customise Appearance (Priority: P3)

**Goal**: Validate the theme system (toggle, auto-dark, blending prevention) and font scaling.

**Independent Test**: Press C-c t to toggle themes, use C-+/C-- for font scaling.

### Validation for User Story 5

- [x] T047 [US5] Verify test-ui.el confirms jotain-theme-light and jotain-theme-dark variables are defined in tests/test-ui.el
- [x] T048 [P] [US5] Verify test-ui.el confirms load-theme advice is installed for blending prevention (FR-010) in tests/test-ui.el
- [x] T049 [P] [US5] Verify test-ui.el confirms auto-dark configuration is present in tests/test-ui.el
- [x] T050 [US5] Verify elisp/fonts.el provides C-+, C--, C-0 keybindings for font scaling (SC-007) in elisp/fonts.el
- [x] T051 [P] [US5] Verify elisp/fonts.el implements font enumeration caching (FR-011) in elisp/fonts.el

**Checkpoint**: User Story 5 validated — theme and appearance system works correctly.

---

## Phase 8: User Story 6 — Use Jotain Across Platforms (Priority: P3)

**Goal**: Validate platform detection constants, macros, and platform-specific adaptations.

**Independent Test**: Verify platform detection constants are correct and platform-specific features activate.

### Validation for User Story 6

- [x] T052 [US6] Verify test-platform.el confirms platform constants are defined (platform-linux-p, platform-macos-p, platform-android-p, platform-windows-p) in tests/test-platform.el
- [x] T053 [P] [US6] Verify test-platform.el confirms platform macros work (platform-when, platform-unless, platform-cond) in tests/test-platform.el
- [x] T054 [P] [US6] Verify test-platform.el confirms platform feature detection function (platform-has-feature-p) in tests/test-platform.el
- [x] T055 [US6] Verify elisp/platforms.el guards platform-specific code with platform.el constants (constitution V) in elisp/platforms.el
- [x] T056 [P] [US6] Verify elisp/android.el uses platform-android-p guard and provides touch-optimised UI in elisp/android.el
- [x] T057 [US6] Verify NMT test-runtime-deps-disabled confirms Emacs still functions when includeRuntimeDeps=false (edge case: graceful degradation) in nmt-tests/default.nix

**Checkpoint**: User Story 6 validated — platform portability works across Linux, macOS, and Android.

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Address gaps identified in research.md and cross-cutting validation.

- [x] T058 Add ERT test for 1Password auth-source graceful failure when `op` CLI is absent (FR-019 gap from research.md) in tests/test-auth-source-1password.el
- [x] T059 [P] Add ERT test measuring standalone Emacs startup time to validate SC-011 (< 3s target, gap from research.md) in tests/test-smoke.el
- [x] T060 [P] Verify edge case: font degradation when preferred fonts unavailable (graceful fallback) in tests/test-ui.el or elisp/fonts.el
- [x] T061 [P] Verify edge case: missing tree-sitter grammar does not prevent file opening in tests/test-programming.el or elisp/programming.el
- [x] T062 [P] Verify edge case: missing LSP server allows editing without errors (Eglot graceful failure) in tests/test-programming.el or elisp/programming.el
- [x] T063 [P] Verify edge case: daemon failure falls back to standalone Emacs in nmt-tests/default.nix or elisp/platforms.el
- [x] T064 [P] Verify edge case: extraPackages option merges without conflict with base packages in nmt-tests/default.nix
- [x] T065 Audit modules without dedicated tests (dashboard.el, help.el, per-project.el, collaboration.el, systems.el) and add minimal smoke coverage per constitution Principle IV in tests/
- [x] T066 Run `just test-all` to confirm all existing and newly added tests pass
- [x] T067 Run quickstart.md verification checklist against a live Emacs instance via `just emacs-dev`
- [x] T068 Update spec.md status from "Draft" to "Validated" after all tasks complete in specs/001-baseline/spec.md

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies — can start immediately
- **Foundational (Phase 2)**: Depends on Phase 1 structure verification
- **User Stories (Phases 3-8)**: All depend on Phase 2 build validation passing
  - User stories can proceed in parallel (independent validation)
  - Or sequentially in priority order (P1 → P2 → P3)
- **Polish (Phase 9)**: Depends on all user story phases completing

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Phase 2 — No dependencies on other stories
- **User Story 2 (P1)**: Can start after Phase 2 — No dependencies on other stories
- **User Story 3 (P2)**: Can start after Phase 2 — Independent of US1/US2
- **User Story 4 (P2)**: Can start after Phase 2 — Independent, uses utils tested in US4 only
- **User Story 5 (P3)**: Can start after Phase 2 — Independent
- **User Story 6 (P3)**: Can start after Phase 2 — Independent

### Within Each User Story

- Validation tasks verify existing test files and source code
- Tasks marked [P] can run in parallel (different files)
- Sequential tasks depend on prior validation results

### Parallel Opportunities

- All Phase 1 tasks marked [P] can run in parallel
- All Phase 2 tasks marked [P] can run in parallel (within Phase 2)
- Once Phase 2 completes, all 6 user story phases can start in parallel
- Within each story, all tasks marked [P] can run in parallel
- Phase 9 gap-filling tasks T058-T064 can run in parallel (where marked [P])

---

## Parallel Example: Phase 2 Foundational

```bash
# Build validation (sequential — T006 first, then T007)
Task: "Run nix build and verify jotain package builds"
Task: "Run nix build .#emacs and verify Emacs derivation"

# Contract validation (all parallel after builds pass)
Task: "Verify runtime-deps.nix declares all LSP servers"
Task: "Verify runtime-deps.nix declares all CLI tools"
Task: "Verify runtime-deps.nix declares all fonts"
Task: "Verify home module exposes all 4 options"

# Test suite validation (sequential — smoke first, then fast, then full)
Task: "Run just test-smoke — confirm < 1s"
Task: "Run just test-fast — confirm < 5s"
Task: "Run just test — confirm full suite passes"
Task: "Run just test-nmt — confirm all 5 NMT tests pass"
```

---

## Parallel Example: User Stories After Phase 2

```bash
# All 6 user stories can validate in parallel
Task: "US1: Validate init loading and runtime deps"
Task: "US2: Validate completion and LSP configuration"
Task: "US3: Validate Magit and Git integration"
Task: "US4: Validate Org-mode and writing tools"
Task: "US5: Validate theme system and font scaling"
Task: "US6: Validate platform detection and portability"
```

---

## Implementation Strategy

### MVP First (User Stories 1 + 2 Only)

1. Complete Phase 1: Setup validation
2. Complete Phase 2: Foundational build validation (CRITICAL — blocks all stories)
3. Complete Phase 3: User Story 1 (Install and Launch)
4. Complete Phase 4: User Story 2 (Completion and LSP)
5. **STOP and VALIDATE**: Core functionality confirmed
6. Proceed with remaining stories

### Incremental Delivery

1. Complete Setup + Foundational → Build system validated
2. Validate US1 → Installation works → First milestone
3. Validate US2 → Coding workflow works → Second milestone
4. Validate US3 + US4 → Git and Org-mode work → Third milestone
5. Validate US5 + US6 → Themes and platforms work → Fourth milestone
6. Complete Polish → All gaps filled → Baseline complete

### Parallel Team Strategy

With multiple agents:

1. All agents complete Setup + Foundational together
2. Once Foundational passes:
   - Agent A: User Story 1 + User Story 2 (P1 priority)
   - Agent B: User Story 3 + User Story 4 (P2 priority)
   - Agent C: User Story 5 + User Story 6 (P3 priority)
3. All reconvene for Phase 9 Polish

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- This baseline validates existing code — most tasks are verification, not creation
- T058, T059, and T065 require writing new code (gap-filling tests and module audit)
- T060-T064 verify edge cases from spec.md individually for traceability
- T068 is the final task — marks the spec as validated
- Commit after each phase or logical group
- Stop at any checkpoint to confirm story independently
