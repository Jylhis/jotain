# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Management

All Emacs packages are defined in default.nix via `emacsWithPackages`. When adding new packages:
1. Add package to the `epkgs` list in default.nix
2. Run `nix build` to verify the package builds
3. Add corresponding `use-package` configuration in appropriate elisp/jotain-*.el file
   - Core built-ins → jotain-core.el
   - UI/completion/fonts → jotain-editor.el
   - Programming/languages → jotain-prog.el
   - Git/Org/help → jotain-tools.el
4. Run `nix build .#smoke-test` to verify module loads correctly

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

# Clean compiled files and cache (safe, only touches project files)
just clean

# Interactive development (isolated environment)
just emacs-dev                 # Recommended: Run Emacs with project config (safe)
just emacs-test-interactive    # Interactive testing with isolation messages
just emacs-clean              # Without isolation (not recommended, shows warning)
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

## Development Environment Isolation

**Your personal Emacs configuration is protected!** This project uses multiple isolation mechanisms to ensure development work never interferes with your personal Emacs setup.

### Nix DevShell Isolation

When you enter the development shell with `nix develop`, your environment is automatically isolated:

```bash
# Automatically sets:
HOME=$PWD/.dev-home
XDG_CONFIG_HOME=$PWD/.dev-home/.config
XDG_CACHE_HOME=$PWD/.dev-home/.cache
XDG_DATA_HOME=$PWD/.dev-home/.local/share
```

This means:
- ✅ Your personal `~/.config/emacs/` is untouched
- ✅ Your personal `~/.emacs.d/` is untouched
- ✅ All development activity happens in `.dev-home/` (git-ignored)
- ✅ `just clean` only affects project files, never your home directory

### Safe Development Commands

Use these commands for isolated testing:

```bash
# Recommended: Run Emacs with project config in isolated environment
just emacs-dev

# For interactive testing with clear isolation messages
just emacs-test-interactive

# Caution: Runs without isolation (not recommended)
just emacs-clean  # Shows warning
```

### Test Isolation

All testing commands are automatically isolated:

- **ERT tests** (`just test`, `just test-tag`): Use `emacs -Q` with `user-emacs-directory` set to project
- **NMT tests** (`just test-nmt`): Run in Nix sandbox, completely isolated
- **Runtime tests** (`just test-runtime`): Run in nixosTest VM with fresh testuser

### Clean Commands Are Safe

The `clean` commands only affect project files:

```bash
just clean      # Cleans project .elc files and .dev-home/
just clean-all  # Same as clean (no package dirs with Nix)
```

**Before the fix, these commands would have deleted your personal Emacs config! Now they're safe.**

### How Isolation Works

1. **DevShell**: Environment variables redirect HOME to `.dev-home/`
2. **Commands**: Use `-Q` flag and explicit `user-emacs-directory` settings
3. **Tests**: Run in Nix sandbox or VM, completely isolated
4. **Git**: `.dev-home/` is git-ignored, temporary development artifacts

### When You Need Caution

Only these scenarios bypass isolation (by design):

- Running `emacs` directly without any flags outside the devshell
- Using `just emacs-clean` (shows warning, prefer `just emacs-dev`)
- Manually setting paths that point to your home directory

**Best Practice**: Always use `nix develop` to enter the devshell, then all commands are automatically safe.

## Architecture Overview

Jotain is a minimalist, modular Emacs configuration using Nix for reproducible builds. The configuration follows a clean, consolidated module system.

### Module System

The configuration uses a simple two-tier structure:

**init.el** → **jotain.el** → **4 core modules**

1. **init.el**: Minimal loader that sets up load path and requires jotain.el
2. **jotain.el**: Main entry point that initializes modules in order
3. **Core modules** (elisp/jotain-*.el):
   - `jotain-core.el` - Core Emacs settings and built-in packages
   - `jotain-editor.el` - UI, themes, completion framework, fonts
   - `jotain-prog.el` - Programming tools, LSP, languages
   - `jotain-tools.el` - Git, Org-mode, help system

All modules use `use-package` for consistent configuration with `:init`, `:config`, `:custom`, `:bind`, and `:hook` sections.

### Key Design Patterns

1. **Simplicity**: Only 4 core modules (down from 14+), each focused on a clear domain
2. **Nix-native**: All packages pre-installed via Nix, no runtime package downloads
3. **Lazy Loading**: Extensive use of `:defer`, `:hook`, and autoloads
4. **Consolidated**: Related functionality grouped together (UI+completion, Git+Org+help)

### Nix Integration

- **default.nix**: Builds Emacs with all packages via `emacsWithPackages`. Defines smoke and full test targets in `passthru`
- **config.nix**: Deployment package containing only `init.el`, `early-init.el`, and `elisp/`
- **flake.nix**: Flake outputs (packages, devShells, checks, overlays, homeModules)

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

Jotain uses streamlined smoke tests for fast validation.

### Smoke Tests

All tests are in a single file `tests/test-jotain.el`:

```bash
nix build .#smoke-test  # Ultra-fast smoke tests (< 1 second)
nix build .#tests       # Full test suite (same as smoke for now)
```

Tests validate:
- Emacs version meets minimum (30.1+)
- Directory structure (elisp/, init.el, early-init.el)
- All jotain-* modules load without errors
- Basic functionality works

### Test Philosophy

- **Fast**: All tests complete in < 1 second
- **Essential**: Only test what matters (module loading, basic functionality)
- **Simple**: Single test file, no complex infrastructure

When adding tests:
1. Add to `tests/test-jotain.el`
2. Use `:tags '(smoke)` for critical tests
3. Keep tests simple and fast
4. Test module loading, not implementation details

## Important Files

- **init.el**: Minimal loader that sets up load path and requires jotain.el
- **early-init.el**: Pre-initialization settings (loaded before package system)
- **elisp/jotain.el**: Main entry point that initializes all modules
- **elisp/jotain-core.el**: Core Emacs settings and built-in packages
- **elisp/jotain-editor.el**: UI, themes, completion, fonts
- **elisp/jotain-prog.el**: Programming tools, LSP, languages
- **elisp/jotain-tools.el**: Git, Org-mode, help system
- **default.nix**: Emacs package builder with all dependencies and test definitions
- **config.nix**: Deployment package (only init.el, early-init.el, elisp/)
- **flake.nix**: Flake outputs (packages, devShells, checks, overlays, homeModules)
- **module.nix**: Home-manager module definition
- **tests/test-jotain.el**: Smoke tests

### Deployment Package (config.nix)

The config.nix creates a minimal deployment package containing only runtime files:

**Included:**
- `init.el` - Entry point
- `early-init.el` - Pre-initialization
- `elisp/` - All jotain-* modules

**Excluded:**
- Development files (.envrc, .github/, CLAUDE.md)
- Nix build files (default.nix, flake.nix, module.nix)
- Test files (tests/, nmt-tests/)
- Build artifacts (result, .dev-home/)

This keeps the deployed configuration minimal and focused on runtime needs.