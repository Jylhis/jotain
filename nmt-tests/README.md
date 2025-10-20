# NMT Tests for Emacs Home-Manager Module

This directory contains NMT (Nix Module Tests) for validating the home-manager Emacs module defined in `module.nix`.

## Overview

NMT tests provide integration testing for the home-manager module by building actual home-manager configurations and validating their outputs.

## Available Tests

### test-emacs-config-files
Validates that Emacs configuration files are properly linked to `~/.config/emacs`:
- Checks for `.config/emacs` directory creation
- Verifies `init.el` exists
- Confirms file structure matches source

### test-shell-aliases
Validates shell aliases configuration:
- Checks for `emc`, `emcg`, `emqg`, `emq` aliases
- Verifies aliases are included in activation scripts

### test-emacs-service
Validates Emacs systemd service configuration:
- Checks for systemd user service files
- Verifies service is configured when `programs.emacs.enable = true`
- Confirms socket activation is enabled

### test-font-packages
Validates font package installation:
- Checks that font packages are included in home-path
- Verifies fonts directory exists
- Counts installed font files

### test-module-disabled
Validates module behavior when disabled:
- Confirms no config files are created when `programs.emacs.enable = false`
- Ensures clean state when module is not active

### test-fileset-source
Validates fileset-based source handling:
- Checks all expected directories exist (config, lisp, tests)
- Verifies key files are present (init.el, early-init.el)
- Confirms directory structure is preserved

## Running Tests

### Run all NMT tests
```bash
just test-nmt
```

### Run specific test
```bash
nix build .#checks.x86_64-linux.test-emacs-config-files
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
