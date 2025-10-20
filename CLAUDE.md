# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Management

All Emacs packages are defined in default.nix via `emacsWithPackages`. When adding new packages:
1. Add package to the `epkgs` list in default.nix
2. Run `nix build` to verify the package builds
3. Add corresponding `use-package` configuration in appropriate config/*.el file
4. Run `just test` to ensure no regressions

The configuration uses `package.el` for package metadata but packages are pre-installed by Nix, not downloaded at runtime.

## Development Commands

Build and test Emacs configuration:
```bash
# Check configuration syntax (Nix dry-run)
just check

# Run ERT unit tests via Nix
just test

# Run all tests (ERT + NMT home-manager module tests)
just test-all

# Run NMT tests only (validates home-manager module)
just test-nmt

# Run tests by tag (smoke, fast, unit, integration, etc.)
just test-tag smoke

# Build Emacs package with Nix
just build

# Byte-compile all Emacs Lisp files (for development)
just compile

# Clean compiled files and cache
just clean

# Start Emacs with clean config (for testing)
just emacs-clean
```

Development workflow:
```bash
# Enter Nix development shell (recommended)
nix develop

# Run tests after changes
just test              # ERT unit tests
just test-nmt          # Home-manager module tests
just test-all          # All tests

# Nix operations
nix build .#default    # Build Emacs package
nix flake check        # Run all checks
nix fmt                # Format Nix files

# Update dependencies
just update            # Update all flake inputs
just update-input nixpkgs  # Update specific input
```

## Architecture Overview

This is a modular Emacs configuration using Nix for reproducible builds. The configuration follows a feature-based module system where each aspect is isolated in its own file.

### Module System
The configuration is split into logical modules loaded via `require` in init.el:
- **Core modules** (config/): Each handles a specific feature domain (completion, git, programming, etc.)
- **Utility libraries** (lisp/): Shared functionality and platform detection
- **Platform adaptations**: Automatic OS-specific configurations via platform.el detection

All Emacs Lisp modules follow the `use-package` macro convention for package configuration, providing consistent structure with `:init`, `:config`, `:custom`, `:bind`, and `:hook` sections.

**Module loading order in init.el:**
1. `platform` (must be first for OS detection)
2. `core` (fundamental Emacs settings)
3. `fonts` (font configuration)
4. `ui` (UI and appearance)
5. `completion` (completion framework)
6. `programming` (development tools)
7. `per-project` (project-specific configurations)
8. `writing` (Org-mode and documentation)
9. `git` (version control)
10. `help` (enhanced help system)
11. `ai` (AI integrations)
12. `systems` (system administration)
13. `platforms` (platform adaptations)
14. `android` (conditional, only on Android)
15. `app-launchers` (application launcher utilities)

### Key Design Patterns
1. **Platform Detection**: The `platform.el` library detects the OS/environment and sets flags like `platform-android-p`, `platform-macos-p`, etc. Platform-specific code checks these flags. It must load first before other modules.

2. **Lazy Loading**: Modules use `with-eval-after-load` and autoloads to defer package loading until needed.

3. **Feature Modules**: Each config/*.el file is self-contained and can be loaded independently. They all `(provide 'module-name)` at the end.

4. **Nix Integration**:
   - **default.nix**: Builds Emacs with all required packages via `emacsWithPackages`. Contains ERT test definitions in `passthru.tests`.
   - **config.nix**: Creates deployment package using `lib.fileset` to filter out development files (tests, nix files, etc.).
   - **flake.nix**: Defines packages, dev shells, checks, overlays, and home-manager module.

### Module Dependencies
- `platform.el` must load first (provides OS detection)
- `core.el` sets fundamental Emacs defaults
- Other modules can load in any order but may have soft dependencies (e.g., completion enhances programming)

### Home Manager Module (module.nix)
The home-manager module handles deployment of the Emacs configuration:
- Deploys configuration files to `~/.config/emacs` via `programs.emacs.userConfig` option
- Configures systemd service for Emacs daemon with socket activation
- Sets up shell aliases: `jot` (terminal client), `emc` (terminal client), `emcg` (GUI client), `emqg` (terminal no config), `emq` (GUI no config)
- Installs Nerd Fonts and other font packages
- Controlled via `programs.emacs.enable` option
- When disabled, no configuration is deployed and no services are started
- Configuration path defaults to this repository's source but can be overridden

### Flake Outputs
- **packages.emacs**: Emacs with all packages pre-installed (from default.nix)
- **packages.config**: Configuration files only (from config.nix, filtered via fileset)
- **overlays.default**: Provides `jotain` and `jotain-config` to nixpkgs
- **homeModules.default**: Home-manager module (from module.nix)
- **devShells.default**: Development environment with just, nixpkgs-fmt, deadnix, statix

## Testing Approach

This project uses two testing frameworks:

### ERT (Emacs Lisp Regression Testing)
Unit tests for Emacs Lisp code:
- Test files in `tests/` directory
- Named `test-*.el`
- Run with `just test` (via Nix) or `just test-tag TAG` (by tag: smoke, fast, unit, integration)
- Tests are built into the Nix package via `passthru.tests` in default.nix
- Automatically run during `nix flake check`

When adding ERT tests:
1. Add tests in tests/test-feature.el
2. Update default.nix passthru.tests to load new test file (add `--load` statement)
3. Ensure tests pass with `just test`
4. Test naming convention: `(ert-deftest test-module-feature () ...)`

### NMT (Nix Module Tests)
Integration tests for the home-manager module:
- Test files in `nmt-tests/` directory
- Uses `home-manager.lib.homeManagerConfiguration` to build actual configurations
- Tests use `mkTest` helper which wraps `pkgs.runCommand` for test execution
- Run with `just test-nmt` or individual tests with `nix build .#checks.x86_64-linux.test-<name>`
- Available tests:
  - test-module-enabled: Comprehensive test (config files, directory structure, shell aliases, systemd service, fonts)
  - test-module-disabled: Validates behavior when disabled

When adding NMT tests:
1. Add test definition in nmt-tests/default.nix using `mkTest` helper
2. Update justfile test-nmt command to include new test
3. Update nmt-tests/README.md with test description
4. Ensure tests pass with `just test-nmt`
5. Tests should use `set -euo pipefail` for strict error handling

### Running All Tests
Use `just test-all` or `nix flake check` to run both ERT and NMT tests.

### Runtime Validation
Optional, comprehensive end-to-end test using nixosTest:
- Run with `just test-runtime` (starts a VM, slower)
- Validates actual Emacs execution, daemon, and client connectivity
- Not included in `just test-all` for faster local testing
- Always runs in CI via `nix flake check`

### Test Boundaries - What to Test Where

**Use ERT (tests/) for:**
- Pure Elisp functionality (utils.el, platform.el functions)
- Configuration loading (init.el, early-init.el, module requires)
- Unit tests (isolated function behavior, no external dependencies)
- Smoke tests (fast critical validation, < 1 second with `:tags '(smoke)`)
- Integration tests (package interactions, with `:tags '(integration)`)

**Use NMT (nmt-tests/) for:**
- Home-manager module behavior (file deployment, service configuration)
- Integration with Nix (packages installed, paths correct, fileset filtering)
- Module options (enable/disable, userConfig override)
- Cross-module interactions (fonts + emacs, systemd + emacs)

**Use nixosTest (nmt-tests/runtime.nix) for:**
- Actual execution validation (daemon starts, client connects)
- End-to-end testing (full configuration loads in real environment)
- Platform-specific behavior (systemd service actually works)
- Regression testing (catch breaking changes in real usage)

**Don't test in Nix layer:**
- Elisp correctness → Use ERT instead
- UI/UX behavior → Use ERT with temp buffers or manual testing
- Implementation details → Trust abstractions, test behavior

## Important Files

- **init.el**: Main entry point that loads all configuration modules in order
- **early-init.el**: Pre-initialization settings (loaded before package system)
- **lisp/platform.el**: Platform detection library (must load first)
- **config/core.el**: Fundamental Emacs settings and built-in package configurations
- **default.nix**: Emacs package builder with all dependencies
- **config.nix**: Creates filtered configuration package for deployment using lib.fileset
- **module.nix**: Home-manager module definition
- **flake.nix**: Main flake entry point with all outputs
- **justfile**: Development task runner with common commands

### Fileset Filtering (config.nix)
The config.nix file filters out development-only files when creating the deployment package.

**Excluded from deployment:**
- `.claude/` - Claude Code configuration
- `.envrc` - direnv configuration
- `.github/` - GitHub workflows and CI
- `.gitignore` - Git ignore rules
- `CLAUDE.md` - This documentation file
- `default.nix` - Nix package builder
- `flake.lock` - Nix flake lock file
- `flake.nix` - Nix flake definition
- `justfile` - Development task runner
- `module.nix` - Home-manager module definition
- `nmt-tests/` - NMT integration tests
- `tests/` - ERT unit tests

**Included in deployment:**
- `init.el` - Main configuration entry point
- `early-init.el` - Pre-initialization settings
- `config/` - Feature modules (core, ui, completion, programming, etc.)
- `lisp/` - Utility libraries (platform detection, app launchers, utils)
- `config.nix` - The package definition itself