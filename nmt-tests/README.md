# NMT Tests for Emacs Home-Manager Module

This directory contains NMT (Nix Module Tests) for validating the home-manager Emacs module defined in `module.nix`.

## Overview

NMT tests provide integration testing for the home-manager module by building actual home-manager configurations and validating their outputs.

## Available Tests

### test-module-enabled
Comprehensive integration test for module when enabled. Validates:

**Configuration Files:**
- `.config/emacs` directory creation
- `init.el` and `early-init.el` exist

**Directory Structure:**
- Expected directories exist (`config/`, `lisp/`)
- Development files are filtered out (`tests/` directory absent)
- Fileset filtering works correctly

**Shell Aliases:**
- Emacs aliases configured (`emc`, `emcg`, `emqg`, `emq`)
- Aliases included in activation scripts

**Systemd Service:**
- Service files created in systemd user directory
- Socket activation enabled
- Service configured when `programs.emacs.enable = true`

**Font Packages:**
- Font packages included in home-path
- Fonts directory exists and contains fonts

### test-module-disabled
Validates module behavior when disabled:
- Confirms no config files are created when `programs.emacs.enable = false`
- Ensures clean state when module is not active

## Running Tests

### Run all NMT tests
```bash
just test-nmt
```

### Run specific test
```bash
nix build .#checks.x86_64-linux.test-module-enabled
nix build .#checks.x86_64-linux.test-module-disabled
```

### Run all checks (including NMT)
```bash
just test-all
# or
nix flake check
```

## Test Implementation

Tests use the following approach:
1. Build a home-manager configuration with the module
2. Extract the activation package
3. Check the generated files and directory structure
4. Validate expected outputs exist and contain correct values

## Adding New Tests

To add a new test:

1. Add a new test entry in `nmt-tests/default.nix`:
```nix
test-my-feature = mkTest "my-feature" ''
  set -euo pipefail

  homeConfig="${buildHomeConfig [
    {
      programs.emacs = {
        enable = true;
        userConfig = ./..;
      };
    }
  ]}"

  # Your test assertions here
  echo "Testing my feature..."

  touch $out
'';
```

2. Update `justfile` to include the new test in the `test-nmt` command

3. Update this README with test description
