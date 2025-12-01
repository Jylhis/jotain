# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## System Prerequisites

The following tools must be available in your system PATH for full functionality:

- **git** (required): Used by magit and diff-hl for version control operations
- **ripgrep** (optional): Fast text search for consult-ripgrep and xref
- **fd** (optional): Fast file finder for consult-fd
- **direnv** (optional): Automatic directory environment loading

**Git is NOT bundled** with this configuration to avoid conflicts with user's git setup. Users typically have git configured separately via:

```nix
# Home Manager configuration
programs.git = {
  enable = true;
  # ... your git config
};

# Or for git with SVN support:
programs.git = {
  enable = true;
  package = pkgs.gitSVN;  # or pkgs.gitFull for all features
};
```

If you need to install git manually:
```nix
home.packages = [ pkgs.git ];
```

**Note:** Other tools (ripgrep, fd, direnv) are provided by jotain's `includeRuntimeDeps` option but can be overridden if you manage them separately. Set `programs.jotain.includeRuntimeDeps = false` to manage all runtime dependencies yourself.

## Quick Start: Try Without Installing

You can try Jotain instantly without installation using `nix run`:

```bash
# Try Jotain with default configuration
nix run github:Jylhis/jotain

# Run with specific arguments
nix run github:Jylhis/jotain -- --batch --eval '(message "Hello from Jotain!")'

# Test locally after cloning
git clone https://github.com/Jylhis/jotain.git
cd jotain
nix run .
```

**How it works:**
- Creates a temporary isolated environment (automatically cleaned up on exit)
- Copies the full Jotain configuration to the temporary directory
- Launches Emacs with all packages and runtime dependencies
- Your personal Emacs config (`~/.config/emacs` or `~/.emacs.d`) remains completely untouched

**For development:** If you're working on Jotain itself, use `nix develop` and `emacs-dev` instead (see Development Commands section below).

## Package Management

All Emacs packages are defined in default.nix via `emacsWithPackages`. When adding new packages:
1. Add package to the `epkgs` list in default.nix
2. Run `nix build` to verify the package builds
3. Add corresponding `use-package` configuration in appropriate elisp/*.el file
4. Run `just test` to ensure no regressions

The configuration uses `package.el` for package metadata but packages are pre-installed by Nix, not downloaded at runtime.

**Note**: For runtime dependencies (tree-sitter grammars, LSP servers, CLI tools, fonts), see the "Runtime Dependency Management" section below.

## Runtime Dependency Management

This configuration manages non-Emacs dependencies (tree-sitter grammars, fonts, LSP servers, CLI tools) through a centralized runtime dependency system. These dependencies are external programs and data that Emacs packages invoke at runtime.

### Overview

Runtime dependencies are separated from Emacs packages for several reasons:
- **Modularity**: Easy to add/remove tools without rebuilding Emacs
- **Reusability**: Same tools can be used by other applications
- **Testability**: Can verify dependencies are correctly provisioned
- **Flexibility**: Users can opt out via `includeRuntimeDeps = false`

### Architecture

The runtime dependency system consists of three key components:

**1. nix/lib/runtime-deps.nix**

Central definition file that categorizes all runtime dependencies:
- `treeSitterGrammars`: Tree-sitter grammars for syntax highlighting
- `fonts`: Font packages (Nerd Fonts, Inter, Source Serif Pro, etc.)
- `lspServers`: Language Server Protocol servers (nil, gopls, etc.)
- `cliTools`: Command-line tools (ripgrep, fd, direnv, etc.) - Note: git is NOT included, see System Prerequisites

The file provides convenience aggregates:
- `allRuntimeDeps`: LSP servers + CLI tools (excludes fonts and tree-sitter)
- `allWithFonts`: All dependencies including fonts
- `devEnv`: Essential development tools for devShells

**2. emacs.nix**

Wraps Emacs with runtime dependencies:
- Adds all runtime deps to `PATH` via `makeWrapper`
- Creates tree-sitter grammar directory and sets `TREE_SITTER_DIR`
- Exposes dependencies via `passthru` for home-manager module
- Tree-sitter grammars are automatically discovered by `treesit-auto`

**3. nix/modules/home/default.nix**

Home-manager module that provisions dependencies:
- Controlled via `programs.jotain.includeRuntimeDeps` option (default: true)
- When enabled, installs LSP servers, CLI tools, and fonts
- Configures fontconfig automatically
- When disabled, users manage dependencies separately

### Integration Flow

```
runtime-deps.nix (definitions)
      ↓
emacs.nix (wrapping + PATH)
      ↓
home-manager module (deployment)
      ↓
User environment (installed packages)
```

### Adding New Dependencies

#### Tree-sitter Grammars

Tree-sitter grammars provide fast syntax highlighting and code analysis for treesit-auto and Emacs 29+ built-in treesit.

**Steps:**
1. Search nixpkgs: `nix search nixpkgs tree-sitter-grammars.<language>`
2. Add to `treesitterGrammars` list in `nix/lib/runtime-deps.nix`:
   ```nix
   treesitterGrammars = with pkgs.tree-sitter-grammars; [
     # ... existing grammars ...
     tree-sitter-<language>
   ];
   ```
3. Rebuild: `nix build` or `just build`
4. Grammar automatically available to treesit-auto (no elisp changes needed)

**Example**: Adding Haskell grammar
```nix
treesitterGrammars = with pkgs.tree-sitter-grammars; [
  # ... existing ...
  tree-sitter-haskell
];
```

#### LSP Servers

LSP servers are used by eglot (configured in elisp/programming.el).

**Steps:**
1. Search nixpkgs: `nix search nixpkgs <language>-lsp` or `nix search nixpkgs <language>-language-server`
2. Add to `lspServers` list in `nix/lib/runtime-deps.nix`:
   ```nix
   lspServers = builtins.filter (x: x != null) [
     # ... existing servers ...
     pkgs.<lsp-server-package>  # Language description
   ];
   ```
3. Optionally configure in `elisp/programming.el`:
   ```elisp
   (with-eval-after-load 'eglot
     (add-to-list 'eglot-server-programs
                  '(<mode> . ("<lsp-command>" "--args"))))
   ```
4. Test: `just test` and manually test in editor

**Example**: Adding Rust Analyzer
```nix
lspServers = builtins.filter (x: x != null) [
  # ... existing ...
  pkgs.rust-analyzer  # Rust language server
];
```

#### CLI Tools

CLI tools are external programs invoked by Emacs packages via `executable-find` or `shell-command`.

**Steps:**
1. Identify the tool: Search elisp/*.el for `(executable-find "tool-name")` or shell-command usage
2. Search nixpkgs: `nix search nixpkgs <tool-name>`
3. Add to `cliTools` list in `nix/lib/runtime-deps.nix`:
   ```nix
   cliTools = builtins.filter (x: x != null) [
     # ... existing tools ...
     pkgs.<package-name>  # Tool description (which package uses it)
   ];
   ```
4. If optional/unfree, use `optionalPackage` helper:
   ```nix
   (optionalPackage "<package-name>")  # Description
   ```
5. Test: `just test` and verify tool is in PATH

**Example**: Adding pandoc for document conversion
```nix
cliTools = builtins.filter (x: x != null) [
  # ... existing ...
  pkgs.pandoc  # Universal document converter (used by org-mode)
];
```

#### Fonts

Fonts are used by elisp/fonts.el for editor appearance.

**Steps:**
1. Search nixpkgs: `nix search nixpkgs <font-name>`
2. Add to `fonts` list in `nix/lib/runtime-deps.nix`:
   ```nix
   fonts = builtins.filter (x: x != null) [
     # ... existing fonts ...
     pkgs.<font-package>  # Font description
   ];
   ```
3. Update font preferences in `elisp/fonts.el` if needed:
   ```elisp
   (defcustom j10s-fonts-default-family
     '("New Font" "JetBrainsMono Nerd Font" ...)
     ...)
   ```
4. Test rendering: `just emacs-dev` and check font display

**Example**: Adding JetBrains Mono regular (not Nerd Font)
```nix
fonts = builtins.filter (x: x != null) [
  # ... existing ...
  pkgs.jetbrains-mono  # JetBrains Mono (regular, without Nerd Font icons)
];
```

### Testing Runtime Dependencies

**ERT Tests** (tests/test-runtime-deps.el):
- Test that expected executables are in PATH
- Test that tree-sitter grammars load correctly
- Use `:tags '(unit)` for fast testing

**NMT Tests** (nmt-tests/):
- `test-runtime-deps-enabled`: Verifies packages installed when `includeRuntimeDeps = true`
- `test-runtime-deps-disabled`: Verifies packages not installed when `includeRuntimeDeps = false`
- Run with `just test-nmt`

**Runtime Tests** (nmt-tests/runtime.nix):
- End-to-end validation in VM
- Tests actual tool execution
- Run with `just test-runtime`

**Manual Testing**:
```bash
# Enter development environment
nix develop

# Test in isolated Emacs
just emacs-dev

# Verify tool availability
M-x executable-find RET ripgrep RET
M-x executable-find RET gopls RET

# Check tree-sitter grammars
M-x treesit-language-available-p RET bash RET

# Verify fonts
M-x describe-font RET JetBrainsMono Nerd Font
```

### Home Manager Configuration

Users can control runtime dependency installation:

**Default behavior** (installs all dependencies):
```nix
programs.jotain = {
  enable = true;
  # includeRuntimeDeps = true;  # Default
};
```

**Minimal installation** (no runtime deps):
```nix
programs.jotain = {
  enable = true;
  includeRuntimeDeps = false;  # User manages dependencies separately
};
```

**Custom dependencies** (mix and match):
```nix
programs.jotain = {
  enable = true;
  includeRuntimeDeps = false;
};

# Install only specific tools
home.packages = with pkgs; [
  ripgrep  # For searching
  gopls    # Go development only
];
```

### Troubleshooting

**Tool not found in PATH**:
- Check if tool is in `nix/lib/runtime-deps.nix`
- Verify emacs.nix wraps tool in PATH
- Test with: `which <tool>` in shell after `nix develop`

**Tree-sitter grammar not loading**:
- Check grammar is in `treesitterGrammars` list
- Verify TREE_SITTER_DIR is set: `M-x getenv RET TREE_SITTER_DIR`
- Check grammar file exists: `ls $TREE_SITTER_DIR`

**Font not displaying**:
- Verify font is in `fonts` list
- Check fontconfig: `fc-list | grep <font-name>`
- Restart Emacs after font installation

**LSP server not starting**:
- Check server is in PATH: `M-x executable-find RET <lsp-command>`
- Check eglot configuration: `M-x eglot-show-workspace-configuration`
- View eglot events buffer: `*EGLOT <project> events*`

## Emacs Daemon Setup

This configuration uses the Emacs daemon/client model for faster startup and persistent sessions. The daemon runs in the background as a systemd user service (Linux) or launchd service (macOS), and you interact with it using `emacsclient`.

### Overview

**Benefits of Daemon Mode:**
- Instant editor startup (daemon already running)
- Persistent Emacs session across terminal windows and frames
- Standard workflow for modern Emacs usage
- Proper integration with system services

**Architecture:**
- Uses upstream home-manager `services.emacs` module (not custom implementation)
- Systemd socket activation on Linux (starts daemon on first connection)
- Automatic environment variable configuration (EDITOR/VISUAL use emacsclient)
- Desktop entry for GUI emacsclient

### Configuration

The daemon is **enabled by default** but can be controlled via the `enableDaemon` option:

**Default configuration** (daemon enabled):
```nix
programs.jotain = {
  enable = true;
  # enableDaemon = true;  # Default
};
```

With this configuration:
- `EDITOR = "emacsclient -t -a ''"` (terminal client, auto-start daemon)
- `VISUAL = "emacsclient -c -a ''"` (GUI client, auto-start daemon)
- Systemd service: `~/.config/systemd/user/emacs.service`
- Socket activation: `~/.config/systemd/user/emacs.socket` (Linux only)
- Desktop entry: `~/.local/share/applications/emacsclient.desktop`

**Disable daemon** (traditional Emacs):
```nix
programs.jotain = {
  enable = true;
  enableDaemon = false;
};
```

With daemon disabled:
- `EDITOR = "emacs -nw"` (direct terminal emacs)
- `VISUAL = "emacs"` (direct GUI emacs)
- No systemd service files created
- No emacsclient desktop entry

### Usage

**Managing the daemon** (Linux with systemd):
```bash
# Check daemon status
systemctl --user status emacs.service

# Start daemon manually
systemctl --user start emacs.service

# Stop daemon
systemctl --user stop emacs.service

# Restart daemon (after configuration changes)
systemctl --user restart emacs.service

# View daemon logs
journalctl --user -u emacs.service
```

**Using emacsclient**:
```bash
# Open file in terminal
emacsclient -t file.txt

# Open file in GUI frame
emacsclient -c file.txt

# Open file in existing frame
emacsclient -n file.txt

# Evaluate Emacs Lisp expression
emacsclient --eval '(+ 2 2)'

# Use $EDITOR (automatically uses emacsclient when daemon enabled)
export EDITOR="emacsclient -t -a ''"
git commit  # Opens in emacsclient
```

**Emacsclient flags:**
- `-t` or `-nw`: Open in terminal (no window)
- `-c`: Create new frame
- `-n`: Don't wait for editor to close (non-blocking)
- `-a ''`: Alternate editor (empty string starts daemon if not running)
- `-a emacs`: Alternate editor (explicitly starts emacs if daemon not available)

### Platform Support

- **Linux (systemd)**: Full support with socket activation
- **macOS (launchd)**: Full support via launchd service
- **Other platforms**: Daemon mode not supported (use `enableDaemon = false`)

### Troubleshooting

**Daemon not starting:**
- Check systemd status: `systemctl --user status emacs.service`
- View logs: `journalctl --user -u emacs.service -n 50`
- Verify service file exists: `ls ~/.config/systemd/user/emacs.service`
- Enable user lingering: `loginctl enable-linger $USER`

**Emacsclient can't connect:**
- Check daemon is running: `systemctl --user is-active emacs.service`
- Test connection: `emacsclient --eval '(+ 2 2)'`
- Check socket file: `ls /run/user/$UID/emacs/server` (or `~/.emacs.d/server`)
- Restart daemon: `systemctl --user restart emacs.service`

**Environment variables not set:**
- Check shell profile: `echo $EDITOR` and `echo $VISUAL`
- Re-login or source shell profile after home-manager activation
- Verify activation: `grep -i editor ~/.nix-profile/activate` (or home-manager generation)

**Daemon vs. non-daemon mode:**
- To temporarily bypass daemon: `emacs file.txt` (direct emacs, not emacsclient)
- To test without daemon: Set `enableDaemon = false` and rebuild
- Check if running as daemon: `M-x (daemonp)` returns `t` in daemon mode

**Configuration not loading:**
- Daemon caches configuration on startup
- Restart daemon after config changes: `systemctl --user restart emacs.service`
- Or rebuild and let home-manager restart: `home-manager switch`

### When to Disable Daemon

Consider disabling the daemon if:
- Debugging Emacs startup issues
- Developing/testing Emacs configuration
- Platform doesn't support systemd/launchd
- Prefer traditional Emacs workflow
- Running in constrained environments (containers, minimal systems)

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

This is a modular Emacs configuration using Nix for reproducible builds. The configuration follows a feature-based module system where each aspect is isolated in its own file.

### Module System
The configuration is split into logical modules loaded via `require` in init.el:
- **Core modules** (elisp/): Each handles a specific feature domain (completion, git, programming, etc.)
- **Utility libraries** (lisp/): Shared functionality and utility functions (app-launchers, utils)
- **Platform adaptations**: Automatic OS-specific configurations via elisp/platform.el detection

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
   - **emacs.nix**: Wraps Emacs with runtime dependencies (LSP servers, CLI tools, tree-sitter) and exposes them via `passthru`.
   - **nix/lib/runtime-deps.nix**: Centralized definition of all runtime dependencies (tree-sitter, fonts, LSP, CLI tools).
   - **config.nix**: Creates deployment package using `lib.fileset` to filter out development files (tests, nix files, etc.).
   - **flake.nix**: Defines packages, dev shells, checks, overlays, and home-manager module.

### Module Dependencies
- `platform.el` must load first (provides OS detection)
- `core.el` sets fundamental Emacs defaults
- Other modules can load in any order but may have soft dependencies (e.g., completion enhances programming)

### Home Manager Module (nix/modules/home/default.nix)
The home-manager module handles deployment of the Emacs configuration:
- Deploys configuration files to `~/.config/emacs` via XDG config files
- Installs Emacs with all packages via `programs.emacs`
- Provisions runtime dependencies (LSP servers, CLI tools, fonts) via `includeRuntimeDeps` option
- Sets environment variables (EDITOR, VISUAL)
- Configures fontconfig for installed fonts
- Controlled via `programs.jotain.enable` option
- When disabled, no configuration is deployed and no dependencies are installed
- Runtime dependencies can be individually controlled via `includeRuntimeDeps = false`

### Flake Outputs
- **packages.jotain**: Configuration files only (from config.nix via default.nix, filtered via fileset)
- **packages.emacs**: Emacs with all packages and runtime dependencies (from emacs.nix)
- **packages.emacs-dev**: Development version with additional dev tools (package-lint, flycheck)
- **overlays.default**: Provides `jotain`, `jotainEmacs`, and `jotainConfig` to nixpkgs
- **homeModules.default**: Home-manager module (from nix/modules/home/default.nix)
- **devShells.default**: Development environment with just, nixpkgs-fmt, statix, and isolated HOME

## Testing Approach

This project uses tiered testing for fast development feedback and comprehensive validation.

### Test Speed Tiers

Tests are organized by speed for progressive validation:

**Instant Checks (< 10 seconds)**
```bash
just check-instant    # Formatting + binary smoke + ERT smoke tests
just test-smoke       # ERT smoke tests only (< 1 second)
```

**Fast Validation (< 1 minute)**
```bash
just check-fast       # Instant checks + fast unit tests
just test-fast        # Fast ERT tests only (< 5 seconds, excludes slow filesystem tests)
```

**Full Local Testing (< 5 minutes, excludes VM)**
```bash
just check-full       # All checks except VM runtime tests (same as `nix flake check`)
just test             # Full ERT suite including slow tests
just test-all         # ERT + NMT tests (excludes VM)
```

**Complete Validation (includes VM, ~5-10 minutes)**
```bash
just check-all              # Everything including VM runtime tests
just test-all-plus-runtime  # Same as above (sets CI=1)
just test-runtime           # VM runtime test only
```

**Parallel Execution (faster on multi-core)**
```bash
just check-parallel  # Run all fast checks in parallel using -j auto
```

### ERT (Emacs Lisp Regression Testing)

Unit tests for Emacs Lisp code:
- Test files in `tests/` directory
- Named `test-*.el`
- Tests use tags for speed tiers: `smoke`, `fast`, `unit`, `integration`, `slow`
- Run specific tags with `just test-tag TAG`
- Tests are built into the Nix package via `passthru.tests` in default.nix

**Test Tiers:**
- `tests/test-smoke.el`: Ultra-fast smoke tests (< 1 second, no I/O)
- `tests/test-platform.el`, `tests/test-utils.el` (fast tests): Unit tests with minimal I/O
- `tests/test-init-loads.el`: Integration tests (loads full config, slow)
- All tests: `just test` or `nix build .#checks.x86_64-linux.emacs-tests`

**Tag Conventions:**
- `:tags '(smoke critical)` - Must pass, ultra-fast (< 1 second total)
- `:tags '(fast unit)` - Fast unit tests (no heavy I/O)
- `:tags '(unit)` - Standard unit tests
- `:tags '(integration slow)` - Integration tests (loads packages/config)
- `:tags '(filesystem slow)` - Tests with filesystem operations

When adding ERT tests:
1. Add tests in tests/test-feature.el with appropriate tags
2. Tests auto-load via tests/test-all.el (no manual registration needed)
3. Use fast tests for most unit testing, slow tests only when necessary
4. Run `just test-smoke` for instant feedback during development
5. Test naming convention: `(ert-deftest test-module/feature () ...)`

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
2. Tests are automatically included in `nix flake check`
3. Update nmt-tests/README.md with test description
4. Ensure tests pass with `just test-nmt`
5. Tests should use `set -euo pipefail` for strict error handling

### Runtime Validation

Comprehensive end-to-end test using nixosTest:
- Run with `just test-runtime` (starts a VM, 2-5 minutes)
- Validates actual Emacs execution, daemon, and client connectivity
- **Excluded from `nix flake check`** by default for faster local development
- **Automatically runs in CI** when `CI` environment variable is set
- Run locally with `CI=1 nix flake check` or `just test-all-plus-runtime`

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

### Configuration Files
- **init.el**: Main entry point that loads all configuration modules in order
- **early-init.el**: Pre-initialization settings (loaded before package system)
- **elisp/platform.el**: Platform detection library (must load first)
- **elisp/*.el**: Feature modules (core, ui, completion, programming, git, etc.)
- **lisp/**: Utility libraries (utils.el, app-launchers.el)

### Nix Build Files
- **flake.nix**: Main flake entry point with all outputs
- **default.nix**: Configuration package builder (creates jotain package)
- **emacs.nix**: Emacs wrapper with runtime dependencies and PATH setup
- **config.nix**: Creates filtered configuration package for deployment using lib.fileset
- **nix/lib/runtime-deps.nix**: Centralized runtime dependency definitions
- **nix/lib/dependencies.nix**: Automatic Emacs package dependency extraction
- **nix/modules/home/default.nix**: Home-manager module definition
- **nix/overlays/default.nix**: Nixpkgs overlay providing jotain packages
- **shell.nix**: Development shell with isolated environment

### Development Tools
- **justfile**: Development task runner with common commands
- **tests/**: ERT unit tests for Emacs Lisp code
- **nmt-tests/**: NMT integration tests for home-manager module

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