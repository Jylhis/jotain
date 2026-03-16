---
description: Run the full speckit pipeline (specify → clarify → plan → tasks → analyze → implement) in a single orchestrated invocation with configurable quality gates.
---

## User Input

```text
$ARGUMENTS
```

Parse flags from `$ARGUMENTS`:
- `--auto` — Skip phase-to-phase confirmation prompts; proceed automatically
- `--strict` — Halt before implementation if `/speckit.analyze` finds any CRITICAL issues
- `--skip-clarify` — Skip the clarify phase even if spec has open questions
- `--skip-analyze` — Skip the cross-artifact consistency check before implementation
- `--from=PHASE` — Resume from a specific phase (specify|clarify|plan|tasks|analyze|implement)
- `--feature="..."` — Feature description (alternative to positional argument)

Everything in `$ARGUMENTS` that is not a flag is treated as the feature description.

## Purpose

This orchestration command runs the complete speckit workflow end-to-end. Use it when:
- Starting a well-understood new feature from scratch
- Resuming a feature at a known phase (`--from=plan`)
- Running CI-style automated feature delivery (`--auto --strict`)

For interactive, exploratory feature development, running the individual commands phase-by-phase gives more control.

## Pre-flight Checks

### 1. Validate inputs

- Extract feature description from `$ARGUMENTS` (non-flag portion or `--feature` value)
- If no feature description and `--from` is not set, abort:
  ```
  ERROR: No feature description provided. Usage: /speckit-workflow "Description of the feature" [--flags]
  ```
- If `--from=PHASE` is set without a feature description, verify that a current feature directory exists (run `.specify/scripts/bash/check-prerequisites.sh --json` to detect it)

### 2. Report configuration

Display the resolved configuration before starting:

```
Speckit Workflow
================
Feature: <description or "resuming existing feature">
Phases:  specify → clarify → plan → tasks → analyze → implement
Flags:   --auto=<yes/no>  --strict=<yes/no>  --skip-clarify=<yes/no>  --skip-analyze=<yes/no>
Starting from: <phase>
```

If `--auto` is NOT set, ask the user to confirm before proceeding:
"Ready to start the workflow. Proceed? (yes/no)"

## Phase Execution

Execute phases in order. After each phase, unless `--auto` is set, pause and confirm:
"Phase <N> complete. Proceed to <next phase>? (yes/no)"

### Phase 1: Specify

**Skip if**: `--from` is set to a later phase.

Run the `/speckit.specify` workflow inline with the feature description.

Key outputs to capture:
- `FEATURE_DIR` — absolute path to the new feature directory
- `BRANCH_NAME` — git branch name
- `WORKTREE_PATH` — worktree location (if created)

If this phase fails, abort the workflow with:
```
ABORT: Specify phase failed. Fix the error above, then resume with:
  /speckit-workflow --from=specify "<description>"
```

### Phase 2: Clarify

**Skip if**: `--skip-clarify` flag is set OR `--from` is set to a later phase.

Run the `/speckit.clarify` workflow inline.

- If clarify produces no open questions, report "No clarification needed — spec is complete" and proceed
- If `--auto` is set and clarify produces questions, apply the suggested defaults automatically and note them in the output
- If `--auto` is NOT set and questions exist, present them interactively and wait for answers before proceeding

### Phase 3: Plan

**Skip if**: `--from` is set to tasks, analyze, or implement.

Run the `/speckit.plan` workflow inline.

If this phase fails, abort with:
```
ABORT: Plan phase failed. Fix the error above, then resume with:
  /speckit-workflow --from=plan
```

### Phase 4: Tasks

**Skip if**: `--from` is set to analyze or implement.

Run the `/speckit.tasks` workflow inline.

If this phase fails, abort with:
```
ABORT: Tasks phase failed. Fix the error above, then resume with:
  /speckit-workflow --from=tasks
```

### Phase 5: Analyze

**Skip if**: `--skip-analyze` flag is set OR `--from=implement`.

Run the `/speckit.analyze` workflow inline (read-only, no file modifications).

Evaluate the analysis report:
- Count CRITICAL findings
- Count HIGH findings

**If `--strict` is set and CRITICAL findings exist**:
```
HALT (--strict mode): Found <N> CRITICAL issue(s) in spec/plan/tasks consistency check.
Review the analysis above and resolve the issues, then resume with:
  /speckit-workflow --from=analyze
```
Stop execution. Do NOT proceed to implementation.

**If `--strict` is NOT set and CRITICAL findings exist**:
Warn the user and, if not `--auto`, ask:
"Found <N> CRITICAL consistency issue(s). Proceed to implementation anyway? (yes/no)"

**If only LOW/MEDIUM findings**: Note them and proceed automatically.

### Phase 6: Implement

Run the `/speckit.implement` workflow inline.

This phase:
- Checks checklists (if any exist in FEATURE_DIR/checklists/)
- Executes tasks phase-by-phase per tasks.md
- Runs the project's test suite after each phase

If implementation halts mid-way, report:
```
PARTIAL: Implementation stopped at task <task-id>.
To resume: /speckit-workflow --from=implement
(Or run /speckit.implement directly to continue from where tasks.md left off)
```

## Completion Report

After all phases complete successfully, output:

```
Workflow Complete
================
Feature:      <FEATURE_DIR>
Branch:       <BRANCH_NAME>
Phases run:   specify → clarify → plan → tasks → analyze → implement

Next steps:
  git push                    # Push feature branch
  gh pr create                # Open pull request (if applicable)
```

## Error Recovery

If any phase fails and the user fixes the issue externally, they can resume at any phase:

```bash
/speckit-workflow --from=plan              # Resume from plan (uses existing spec)
/speckit-workflow --from=implement --auto  # Run implementation non-interactively
```

The `--from` flag reads existing artifacts from the current feature directory detected by `.specify/scripts/bash/check-prerequisites.sh`.

## Notes

- This command is a **coordinator**: it does not duplicate the logic of individual commands. Each phase runs the actual slash command workflow inline.
- All quality gates from individual commands remain active (checklists, test-gate hooks).
- `--auto` does not bypass quality gates — it only skips human confirmation prompts between phases.
- For debugging, run individual phase commands (`/speckit.specify`, `/speckit.plan`, etc.) to isolate failures.
