---
name: validate-deps
description: Audit that all use-package declarations with :ensure have corresponding nixpkgs entries and that nix/lib/dependencies.nix manual mappings are accurate. Triggers on: "validate deps", "check dependencies", "dependency audit", "missing nixpkgs", "package mapping", "use-package ensure", "dep audit", "nix package mapping".
disable-model-invocation: true
allowed-tools: Bash, Read, Grep, Glob
---

# Validate Package Dependencies

Audit use-package → nixpkgs mappings for the jotain distribution.

## Procedure

### 1. Collect All :ensure Declarations

Find every `use-package` form with `:ensure` (excluding `:ensure nil` and `:nodep`):

```bash
grep -rn ':ensure' elisp/*.el | grep -v ':ensure nil' | grep -v ':nodep' | grep -v ';;'
```

This lists all packages that must resolve in nixpkgs.

### 2. Check Manual Mappings in dependencies.nix

Read the current mappings:

```bash
cat nix/lib/dependencies.nix
```

Identify:
- Which packages have manual name-override mappings
- Which packages are excluded/suppressed
- Whether any manual mappings reference packages that no longer exist in `elisp/`

### 3. Verify Each Package Against nixpkgs

For packages where the nixpkgs mapping may be wrong or missing:

```bash
# Search nixpkgs for an emacs package
nix search nixpkgs "emacs.*packagename" 2>/dev/null | head -10

# Or check emacs package overlay directly
nix eval "nixpkgs#emacs30Packages.packagename" --apply 'p: p.meta.description' 2>&1
```

### 4. Check for :nodep Annotations

`:nodep` suppresses auto-extraction. Verify these packages are intentionally excluded and don't need manual handling:

```bash
grep -rn ':nodep' elisp/*.el
```

### 5. Check for Stale Manual Mappings

Manual mappings in `dependencies.nix` for packages no longer in any `elisp/*.el` file should be removed:

```bash
# Get all package names from use-package :ensure forms
grep -rh ':ensure' elisp/*.el | grep -v ':ensure nil' | grep -v ':nodep' | \
  sed -n 's/(use-package \([a-z0-9-]*\).*/\1/p' | sort -u
```

Cross-reference with the manual mappings in `dependencies.nix`.

## Report Format

Output a structured report:

```
## Dependency Audit Report

### Packages Auto-Extracted (should resolve automatically)
- package-a ✓
- package-b ✓

### Packages with Manual Mappings
- emacs-name → nixpkgs-name ✓
- old-mapping → (package no longer in elisp/) ⚠️ STALE

### Packages Suppressed with :nodep
- package-x (intentional: comment explaining why)

### Issues Found
- [ ] package-y: :ensure but no nixpkgs match — needs manual mapping or :nodep
- [ ] old-mapping in dependencies.nix: package removed from elisp/ — remove mapping

### Next Steps
```

## Quick Validation

After making any changes to mappings:

```bash
just check     # Nix syntax dry-run
just build     # Full build to confirm all packages resolve
```

## See Also

- `/add-package` — adding a new package with Nix integration
- `/nix-debug` — for build failures after dependency changes
- `/nix-expert` — Nix conventions and auto-extraction mechanism
