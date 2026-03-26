# Research: Fix Baseline Spec from PR Review

**Branch**: `002-fix-baseline-spec` | **Date**: 2026-02-23

## Overview

No research was required for this feature. All corrections are
sourced directly from the repository owner's inline review comments
on PR #29 and verified against the codebase.

## Decisions

### Dark theme name: `doom-nord` (not `nord`)

- **Decision**: Use `doom-nord` throughout spec and data model
- **Rationale**: `elisp/ui.el:17` defines `(defcustom jotain-theme-dark 'doom-nord`.
  `doom-nord` is from the `doom-themes` package. `nord` is a different
  theme from the separate `nord-theme` package. Using `nord` is factually
  incorrect, not merely informal shorthand.
- **Alternatives considered**: Keeping `nord` as informal name — rejected
  because it refers to a different package/theme entirely.

### Ad-hoc package loading permitted

- **Decision**: Clarify that only persistent runtime installation is
  out of scope; ad-hoc session-only loading is permitted.
- **Rationale**: Constitution Principle I prohibits `package-install`,
  `package-refresh-contents`, and equivalent functions. It does not
  prohibit `require` or `load-library` for packages already available
  on `load-path`. Ad-hoc loading within a session does not violate
  the Nix-only model because it doesn't persist.
- **Alternatives considered**: Blanket prohibition of all runtime
  loading — rejected because it would prevent normal Emacs operation
  (e.g., `require` is used extensively by the config itself).

### Target audience: generalist software engineer

- **Decision**: Add "generalist software engineer" as the overarching
  primary persona; retain existing sub-segments.
- **Rationale**: Owner clarification (PR #29, spec.md:10). Jotain
  targets engineers who happen to use Emacs, not Emacs enthusiasts
  specifically. The existing "Experienced Emacs users" and "Nix power
  users" bullets describe sub-segments within this broader audience.
- **Alternatives considered**: Replacing existing bullets entirely —
  rejected because they provide useful specificity.

### LSP success criterion: language-based, not count-based

- **Decision**: Replace "at least 7 LSP servers" with coverage for
  all supported languages.
- **Rationale**: Owner clarification (PR #29, spec.md:354). The
  specific number is arbitrary. What matters is that every language
  listed in FR-008 has a working LSP server.
- **Alternatives considered**: Keeping a minimum count as a floor —
  rejected because it adds no value over the language-based criterion.

## Follow-up Items

- Constitution `constitution.md` line 192 says "nord-theme (nord)"
  which is also inconsistent with `doom-nord`. This is out of scope
  for this feature but should be addressed as a separate constitution
  PATCH amendment.
