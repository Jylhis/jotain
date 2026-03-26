---
description: Apply a quick fix for trivial changes (typos, style, single-line bugs) that don't warrant the full speckit workflow.
---

## User Input

```text
$ARGUMENTS
```

You **MUST** consider the user input before proceeding (if not empty). The user input describes the fix to apply.

## Purpose

`/speckit.fix` is the **escape hatch** for trivial changes that don't justify the full spec → plan → tasks → implement pipeline. Use it for:

- Typo and documentation corrections
- Style/formatting fixes
- Single-line bug fixes where the problem is already obvious
- Adding a missing import, export, or small constant
- Renaming a symbol that doesn't change behavior

**Do NOT use this for**:
- New features or behavior changes (use `/speckit.specify`)
- Refactors affecting multiple files (use `/speckit.specify`)
- Bug fixes that require design decisions (use `/speckit.specify`)
- Anything requiring tests to be written from scratch (use `/speckit.tasks`)

## Execution

### 1. Confirm fix scope

Check that the fix is genuinely trivial:
- Identify all files that need to change
- If more than 3 files are affected, warn the user and suggest `/speckit.specify` instead
- If the fix introduces new behavior (not just correcting existing behavior), warn and suggest the full workflow

If the user confirms they want to proceed despite the warning, continue.

### 2. Apply the fix

Make the targeted change(s) directly. For each file changed:
- Read the file first to understand context
- Apply the minimal edit that resolves the problem
- Do not refactor surrounding code
- Do not add comments, docstrings, or type annotations to unchanged code

### 3. Run quality gate

Run the project's fast test suite to verify the fix doesn't break anything:

```bash
just test-fast 2>&1 | tail -30
```

If tests fail:
- Report the failure output
- Revert the change or propose a corrected version
- Do not proceed to step 5

If `just` is unavailable (e.g., outside the dev shell), report that tests could not be verified and ask the user to confirm manually.

### 4. Report

Summarize what was changed:
- Files modified
- Nature of the fix (one sentence)
- Test result

## Constraints

- NEVER skip the quality gate (step 4) — even trivial fixes can break things
- NEVER touch files outside the direct scope of the fix
- NEVER create speckit artifacts (spec.md, plan.md, tasks.md) for a quick fix
- If the fix turns out to be non-trivial mid-execution, stop and recommend `/speckit.specify`
