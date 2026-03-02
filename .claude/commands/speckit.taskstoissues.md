---
description: Convert tasks.md into beads issues for multi-session tracking, with epic grouping, dependency wiring, and worktree-aware operation.
---

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty).

## Outline

### 1. Setup

Run the following two commands in parallel:

```bash
# Validate prerequisites and get FEATURE_DIR
.specify/scripts/bash/check-prerequisites.sh --json --require-tasks --include-tasks

# Get branch name and repo root (works from any worktree)
.specify/scripts/bash/check-prerequisites.sh --json --paths-only
```

Parse from the outputs:
- `FEATURE_DIR` — absolute path to the feature directory
- `TASKS` — absolute path to tasks.md
- `BRANCH` — current feature branch name (e.g. `003-analytics-dashboard`)
- `REPO_ROOT` — absolute repo root

**Worktree note**: `bd` automatically discovers `.beads/` from the git root, so it works correctly regardless of which worktree you run from.

### 2. Read tasks.md

Read the full tasks.md file. For each task line matching the checkbox format, extract:

- **Task ID**: `T001`, `T002`, etc.
- **Parallel marker**: `[P]` present or absent
- **User story label**: `[US1]`, `[US2]`, etc. (absent for setup/foundational/polish phases)
- **Description**: the rest of the line after the labels
- **Completion status**: `- [ ]` (pending) vs `- [X]` or `- [x]` (done)
- **Existing beads ref**: `<!-- bd:BEADS_ID -->` trailing comment (skip creation if present)

Also check whether the tasks.md header already contains a `<!-- bd:epic:BEADS_ID -->` comment.

### 3. Create or reuse the feature epic

**If no `<!-- bd:epic:BEADS_ID -->` comment exists in tasks.md**:

```bash
bd create --type epic \
          --priority 1 \
          --title "BRANCH: <feature title from first H1 of spec.md or plan.md>" \
          --labels "speckit,BRANCH" \
          --silent
```

Record the returned epic ID (e.g. `bd-042`).

Insert `<!-- bd:epic:bd-042 -->` on a new line immediately after the first H1 heading in tasks.md.

**If the comment already exists**: parse the existing epic ID and skip creation.

### 4. Create beads issues for each pending task

Process tasks in order. For each task that has **no existing `<!-- bd:BEADS_ID -->`** comment and is **not already completed** (`- [ ]`):

**a. Determine priority**:
- Setup phase / Foundational phase: priority `1`
- `[US1]` tasks: priority `1`
- `[US2]` tasks: priority `2`
- `[US3]` and beyond: priority `3`
- Polish/cross-cutting phase: priority `3`

**b. Determine labels**:
- Always include: `speckit`, `BRANCH`
- If `[P]` marker present: add `parallel`
- If `[USN]` marker present: add `usN` (e.g. `us1`, `us2`)

**c. Determine user story description** (for --description):
- Extract the phase heading from tasks.md for context
- Keep it to one short sentence

**d. Create the issue**:

```bash
bd create --type task \
          --parent EPIC_ID \
          --priority N \
          --title "TASKID: description (truncated to ~80 chars)" \
          --labels "label1,label2" \
          --external-ref "TASKID" \
          --silent
```

Record the returned beads ID alongside the task ID.

**e. Update tasks.md** immediately after creation: append `<!-- bd:BEADS_ID -->` to the task line.

Example result:
```
- [ ] T001 Create project structure per implementation plan <!-- bd:bd-043 -->
- [ ] T005 [P] [US1] Implement user model in src/models/user.py <!-- bd:bd-044 -->
```

### 5. Wire sequential dependencies

After all issues are created, identify sequential dependency chains:
- Within each phase, sequential tasks (no `[P]` marker) depend on the task immediately before them
- A parallel group's first task depends on the last sequential task before the group
- The first task of each user-story phase depends on the last task of the foundational phase

For each dependency (task B depends on task A):

```bash
bd dep add BD_ID_OF_B BD_ID_OF_A
```

**Skip** adding dependencies between tasks within the same parallel group (`[P]` tasks at the same level are independent).

### 6. Handle already-completed tasks

For any task line with `- [X]` or `- [x]` that has a `<!-- bd:BEADS_ID -->` comment:

```bash
bd close BEADS_ID --reason "Completed during implementation"
```

### 7. Sync

```bash
bd sync
```

### 8. Report

Output a summary:
- Epic ID and title
- Total beads issues created (and any skipped because already annotated)
- Issues grouped by user story / phase
- Dependency chains wired
- Any issues closed (pre-completed tasks)
- **Worktree note** if currently in a linked worktree: remind the user that beads issues are shared across all worktrees for this repository
- Next step: run `/speckit.implement` — beads issues will be closed automatically as tasks are completed

## Notes

- This command is **idempotent**: re-running it skips tasks that already have `<!-- bd:BEADS_ID -->` annotations and the epic that already has `<!-- bd:epic:BEADS_ID -->`.
- The `bd` CLI works from any worktree; no need to `cd` to the main repo root.
- `--parent` creates a proper hierarchical child relationship (not just a dep edge), giving a clean tree in `bd graph`.
- `--external-ref "T001"` lets you search by task ID: `bd list --external-ref T001`.
