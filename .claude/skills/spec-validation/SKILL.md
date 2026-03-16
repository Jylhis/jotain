---
name: spec-validation
description: Speckit artifact quality standards for the jotain project. Auto-loaded when reviewing spec.md, plan.md, or tasks.md files, analyzing feature readiness, or checking artifact consistency.
user-invocable: false
---

# Speckit Artifact Quality Standards

Apply these standards when reviewing or validating speckit artifacts in the jotain project.

## Spec Quality (spec.md)

A good spec is:
- **Technology-agnostic**: No mention of frameworks, languages, libraries, or APIs
- **User-focused**: Written for business stakeholders, not developers
- **Testable**: Every requirement has a measurable acceptance criterion
- **Bounded**: Scope is explicitly defined (what's in and out)

**Red flags in spec.md**:
- Implementation details ("React component", "PostgreSQL", "REST API endpoint")
- Vague terms without metrics ("fast", "scalable", "intuitive", "prominent")
- Missing edge cases (what happens when data is empty, network fails, etc.)
- `[NEEDS CLARIFICATION]` markers (must be resolved before planning)
- TODO/TKTK/??? placeholders

**Success criteria must be**:
- Measurable with specific numbers ("< 3 seconds", "95% of users", "10,000 concurrent")
- Technology-agnostic ("users see results instantly", not "API returns in 200ms")
- Verifiable without knowing the implementation

## Plan Quality (plan.md)

A good plan is:
- **Grounded in spec**: Every design decision traces back to a requirement
- **Specific**: Exact libraries, file paths, data schemas — no vagueness here
- **Complete**: Includes error handling, edge cases, testing strategy
- **Consistent**: Terminology matches spec.md exactly

**Check for**:
- Does the tech stack choice match project conventions (Nix-managed, no runtime installs)?
- Are all spec requirements covered by at least one design element?
- Are file paths concrete and follow existing project structure?
- Are dependencies between components explicit?

## Tasks Quality (tasks.md)

Every task must follow the strict format:
```
- [ ] T001 [P] [US1] Description with exact file path
```

**Required elements**:
1. Checkbox `- [ ]`
2. Sequential Task ID (T001, T002...)
3. `[P]` only if genuinely parallelizable (different files, no data deps)
4. `[US1]` story label in user-story phases (omit in Setup/Foundational/Polish)
5. Concrete description with exact file path

**Phase ordering**:
- Phase 1: Setup (project init)
- Phase 2: Foundational (shared prerequisites)
- Phase 3+: One per user story in priority order
- Final: Polish & cross-cutting concerns

## Cross-Artifact Consistency

When all three artifacts exist, check:

| Check | What to verify |
|-------|----------------|
| Spec → Plan coverage | Every functional requirement has a corresponding design element in plan.md |
| Plan → Tasks coverage | Every component in plan.md has at least one task |
| Terminology drift | Same concepts use the same names across all three files |
| No orphan tasks | Every task maps to a requirement or story |

## When to Escalate

- **CRITICAL**: Constitution violation, zero task coverage for a requirement, conflicting requirements
- **HIGH**: Duplicate/conflicting requirements, ambiguous security/perf attribute, untestable acceptance criterion
- **MEDIUM**: Terminology drift, missing non-functional task coverage, underspecified edge case
- **LOW**: Style/wording improvements, minor redundancy

Run `/speckit.analyze` for a full automated cross-artifact consistency report.

## Jotain-Specific Constraints

The jotain project constitution requires:
- All packages managed via Nix (never MELPA/ELPA at runtime)
- Use-package declarations for all Emacs packages
- ERT tests for new functionality
- Module prefix `jotain-` for all global symbols
- `lexical-binding: t` in all `.el` files
- Lazy loading (`:defer`, `:hook`, `:after`, `:commands`) for all packages

When reviewing specs/plans for jotain features, verify these constraints are not violated.
