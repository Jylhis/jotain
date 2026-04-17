---
name: beads-integration
description: Beads issue tracking integration for the jotain project. Auto-loaded when discussing tasks, work planning, issue management, session continuity, or bd commands.
user-invocable: false
---

# Beads Integration Context

Apply this context when discussing work tracking, task management, issue lifecycle, or multi-session continuity in the jotain project.

## Issue Lifecycle

```
open → in_progress → closed
```

- Claim work: `bd update <id> --status=in_progress`
- Complete work: `bd close <id>`
- Close multiple at once: `bd close <id1> <id2> <id3>`

## Key Commands

```bash
bd ready                          # Show unblocked, ready-to-work issues
bd list --status=open             # All open issues
bd list --status=in_progress      # Active work
bd show <id>                      # Full issue detail with dependencies
bd create --title="..." --type=task|bug|feature --priority=2
bd update <id> --status=in_progress
bd close <id> --reason="..."
bd dep add <issue> <depends-on>   # Wire dependency: issue depends on depends-on
bd blocked                        # Show all blocked issues
bd sync                           # Push beads state to git remote
bd stats                          # Project health overview
```

**Priority values**: 0=critical, 1=high, 2=medium, 3=low, 4=backlog. Use numbers, not labels.

## Speckit Integration

When tasks.md exists with `<!-- bd:ID -->` annotations, beads issues are already linked:

- `speckit.taskstoissues` creates the epic + issues from tasks.md
- `speckit.implement` auto-closes beads issues as tasks are marked `[X]`
- No manual `bd close` needed during implementation if running `/speckit.implement`

## Session Continuity

At the **start** of a new session or after compaction:
```bash
bd prime    # Recover context: shows active issues and recent work
```

At **session end** (MANDATORY before saying "done"):
```bash
git status
git add <files>
bd sync
git commit -m "..."
bd sync
git push
```

Work is NOT complete until `git push` succeeds.

## When to Use Beads

| Situation | Action |
|-----------|--------|
| Work spans multiple sessions | `bd create` before starting |
| Task has blockers or dependencies | `bd create` + `bd dep add` |
| Quick fix (< 1 session, no blockers) | Skip beads, use `/speckit.fix` |
| Implementing a speckit feature | Run `speckit.taskstoissues` first |
| Resuming after compaction | `bd prime` to recover context |

## Project-Specific Notes

- Issue prefix: check with `bd stats` (typically `jotain-` or similar)
- All 28 historical issues are closed; new work starts fresh
- Beads v0.42.0 with JSONL+SQLite storage
- `bd sync` runs automatically via git hooks; run manually at session end
