# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Jotain is a modular Emacs distribution managed with Nix Flakes. All Emacs packages are installed via Nix — never via MELPA/ELPA at runtime. The project exposes a `homeModules.default` for Home Manager integration.

## Commands

All commands are run via `just` (see `justfile`). Nix must be available.

### Testing

```bash
just test-smoke          # Ultra-fast smoke tests (< 1s)
just test-fast           # Fast unit tests (< 5s)
just test                # Full ERT test suite via Nix
just test-all            # All checks except VM runtime (formatting + smoke + fast + NMT)
just test-all-plus-runtime  # Includes VM nixosTest (CI only)
just test-tag TAG        # Run ERT tests by tag: smoke, fast, unit, integration
just test-nmt            # NMT home-manager module integration tests
just test-runtime        # VM-based runtime validation (slow)
```

### Building & Checking

```bash
just build               # Build Emacs package with Nix
just compile             # Byte-compile all .el files directly (no Nix)
just check               # Syntax check via Nix dry-run
just format              # Format Nix files (nixfmt)
```

### Development

```bash
just emacs-dev           # Run Emacs with project config in isolated .dev-home (safe)
just emacs-test-interactive  # Same, with console output
just emacs-clean         # Run without isolation (use with caution)
just clean               # Remove .elc, caches, and .dev-home
```

### Nix

```bash
nix flake update         # Update all flake inputs
nix flake update INPUT   # Update a specific input
just info-nix            # Show flake outputs
just info-checks         # Describe available checks
```

## Architecture

### Elisp modules (`elisp/`)

Each file covers a functional domain and is loaded by `init.el` in order:

| File | Purpose |
|------|---------|
| `core.el` | Base settings, keybindings, built-in modes |
| `ui.el` | Theme system (doom-themes, nord), theme toggling, daemon frame setup |
| `fonts.el` | Font selection and rendering |
| `completion.el` | Vertico, Corfu, Consult, Orderless |
| `programming.el` | Eglot (LSP), tree-sitter, DAP, language-specific modes |
| `git.el` | Magit integration |
| `writing.el` | Org-mode, documentation tools |
| `dashboard.el` | Startup screen |
| `platforms.el` | Linux/macOS/Android adaptations |
| `platform.el` | Platform detection helpers |
| `systems.el` | System administration tools |
| `per-project.el` | Project-local configuration via `.dir-locals.el` |
| `help.el` | Enhanced help system |
| `collaboration.el` | Collaborative editing |
| `android.el` | Android-specific features |
| `utils.el` | Shared utility functions |

### Nix build system

```
flake.nix
├── emacs.nix              # Builds Emacs 30 (PGTK) with all packages + runtime deps
│   ├── nix/lib/dependencies.nix   # Auto-extracts packages from use-package :ensure t
│   └── nix/lib/runtime-deps.nix  # LSP servers, fonts, CLI tools, tree-sitter grammars
├── default.nix            # jotain package (config files only)
└── nix/modules/home/default.nix  # Home Manager module (programs.jotain options)
```

**Key property**: `nix/lib/dependencies.nix` scans all `.el` files and automatically maps `use-package` declarations to nixpkgs package names. When adding a new package with `:ensure t`, it is picked up automatically if the name maps correctly; otherwise add a manual mapping in that file.

### Tree-sitter

Grammar paths are set in `early-init.el` from the `TREE_SITTER_DIR` environment variable, which is injected by `emacs.nix` at build time. Tree-sitter grammars are managed in `nix/lib/runtime-deps.nix`.

### Theme system (`elisp/ui.el`)

- `jotain-theme-light` maps to `doom-nord-light`; `jotain-theme-dark` maps to `nord`
- `jotain-ui--disable-all-themes` is advised before every `load-theme` call to prevent theme blending
- In daemon mode, themes are applied after frame creation via `server-after-make-frame-hook`
- Toggle keybinding: `C-c t`

### Testing architecture

- ERT tests live in `tests/`, loaded by `tests/test-all.el`
- Tests are tagged: `smoke`, `fast`, `unit`, `integration`, `slow`
- Tests run in an isolated home (`HOME=.dev-home`) to avoid contaminating user config
- NMT integration tests live in `nmt-tests/` and validate the Home Manager module
- The VM runtime test (`test-emacs-runtime`) only runs when `CI=1`

### Home Manager module options

| Option | Default | Description |
|--------|---------|-------------|
| `programs.jotain.enable` | `false` | Enable Jotain |
| `programs.jotain.enableDaemon` | `true` | Run Emacs as systemd/launchd service |
| `programs.jotain.includeRuntimeDeps` | `true` | Install LSP servers, fonts, CLI tools |
| `programs.jotain.extraPackages` | `epkgs: []` | Add extra Emacs packages |
