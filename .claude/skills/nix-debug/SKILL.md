---
name: nix-debug
description: Debug Nix evaluation errors, build failures, and flake issues in jotain. Triggers on: "nix build fails", "nix error", "evaluation error", "infinite recursion", "missing attribute", "flake check fails", "nix broken", "build failure", "nix trace", "overlay conflict".
argument-hint: "[error message or build target]"
allowed-tools: Bash, Read, Grep, Glob
---

# Debug Nix Build/Evaluation Failures

Diagnose and resolve Nix errors in the jotain project. $ARGUMENTS

This skill handles build and evaluation failures. For configuration validation (checking .nix syntax, formatting), use `/check-config` instead.

## First Response: Get the Full Trace

Always run with `--show-trace` to see the full evaluation stack:

```bash
# Build with trace
nix build .#emacs --show-trace 2>&1 | head -100

# Flake check with trace
nix flake check --show-trace 2>&1

# Evaluate an attribute path with trace
nix eval .#packages.x86_64-linux.emacs --show-trace 2>&1

# Check flake outputs exist
just info-nix
just info-checks
```

## Common Error Patterns

### "attribute 'X' missing"

The package name in `use-package :ensure` doesn't match a nixpkgs attribute.

```bash
# Check what attribute name nixpkgs uses
nix search nixpkgs packagename 2>/dev/null | head -20

# Check current manual mappings
cat nix/lib/dependencies.nix | grep -A2 "packagename"

# Check what's being auto-extracted
nix eval .#packages.x86_64-linux.emacs.passthru.packages --show-trace 2>&1
```

Fix: add a manual mapping in `nix/lib/dependencies.nix`.

### "infinite recursion encountered"

Circular dependency in Nix expressions.

```bash
nix-instantiate --strict --show-trace flake.nix 2>&1 | head -50
```

Look for circular `:after` chains or self-referential attribute sets.

### "evaluation aborted with the following error message"

Pure evaluation failure — check the Nix expression logic:

```bash
# Evaluate subexpressions to isolate the failure
nix eval --expr 'let pkgs = import <nixpkgs> {}; in pkgs.emacsPackages.yourpkg' 2>&1
nix eval .#packages.x86_64-linux 2>&1
```

### Build-time failures (sandbox errors, missing deps)

```bash
# Keep the failed build directory for inspection
nix build .#emacs --keep-failed 2>&1

# Check the build log for a previous attempt
nix log .#emacs 2>&1 | tail -50

# Show the full derivation to inspect inputs
nix show-derivation .#emacs 2>&1 | head -80
```

### Overlay conflicts

```bash
# Check which overlays are applied
cat nix/overlays/default.nix

# Check the emacs-overlay import
nix eval .#legacyPackages.x86_64-linux.emacs --show-trace 2>&1
```

## Debugging dependencies.nix Auto-Extraction

The `nix/lib/dependencies.nix` scanner maps `use-package` declarations to nixpkgs.

```bash
# See what packages are being extracted
grep -r ':ensure' elisp/*.el | grep -v ':ensure nil' | grep -v ':nodep'

# Check the manual mapping table
grep -A1 'mappings\|manualMappings\|nameMap' nix/lib/dependencies.nix | head -40

# Verify a specific package is in nixpkgs emacs overlays
nix eval 'nixpkgs#emacs30Packages.your-package-name' 2>&1
```

## NMT (Home Manager Module) Failures

```bash
# Run all NMT checks with output
just test-nmt 2>&1

# Run a specific check with full logs
nix build .#checks.x86_64-linux.test-module-enabled --print-build-logs 2>&1

# List all available NMT checks
just info-checks
```

## Flake Input Issues

```bash
# Check flake metadata (shows input URLs)
nix flake metadata 2>&1

# Update a specific input
nix flake update emacs-overlay 2>&1

# Check if emacs-overlay is the source of a problem
nix eval .#inputs.emacs-overlay.rev 2>&1
```

## Systematic Isolation Strategy

If the error is unclear, isolate it:

1. **Comment out** recent changes to `emacs.nix` or `dependencies.nix`
2. **Bisect** — check if `nix build` passed before the last commit: `git stash && nix build .#emacs`
3. **Minimal reproduction** — evaluate a smaller expression:
   ```bash
   nix eval --expr '(import ./nix/lib/dependencies.nix { lib = (import <nixpkgs> {}).lib; })'
   ```
4. **Check derivation inputs** — confirm all dependencies resolve:
   ```bash
   nix-store --query --references $(nix-instantiate ./emacs.nix) 2>&1
   ```

## See Also

- `/check-config` — static validation (byte-compile, formatting, syntax)
- `/validate-deps` — audit use-package → nixpkgs mappings
- `/nix-expert` — general Nix conventions for this project
