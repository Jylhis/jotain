# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Jotain is a modular Emacs distribution managed with Nix Flakes. All Emacs packages are installed via Nix — never via MELPA/ELPA at runtime. The project exposes a `homeModules.default` for Home Manager integration.

## Commands

All commands are run via `just` (see `justfile`). Enter the dev shell with `nix develop` to get all project tools.

### Testing

```bash
just test-smoke          # Ultra-fast smoke tests (< 1s)
just test-fast           # Fast unit tests (< 5s)
just test                # Full ERT test suite via Nix
just test-tag TAG        # Run ERT tests by tag (smoke, fast, unit, integration)
just test-all            # All checks except VM runtime (formatting + smoke + fast + NMT)
just test-all-plus-runtime  # Includes VM nixosTest (CI only)
just test-nmt            # NMT home-manager module integration tests
just test-runtime        # VM-based runtime validation (slow)
```

To run a specific NMT check directly:
```bash
nix build .#checks.x86_64-linux.test-module-enabled --print-build-logs
```

### Building & Checking

```bash
just build               # Build Emacs package with Nix
just compile             # Byte-compile all .el files directly (no Nix)
just check               # Syntax check via Nix dry-run
just format              # Format all files via treefmt-nix (nix fmt)
just check-instant       # Formatting + smoke only (< 10s)
just check-fast          # Above + fast unit tests (< 1min)
just check-parallel      # Run all checks in parallel (faster on multi-core)
```

### Development

```bash
nix run                  # Try Jotain in an isolated temp environment (no installation)
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

Each file covers a functional domain. Modules are loaded by `init.el` via `require` in this order:

| File | Purpose |
|------|---------|
| `platform.el` | Platform detection helpers (loaded first, used by other modules) |
| `core.el` | Base settings, keybindings, built-in modes |
| `fonts.el` | Font selection and rendering |
| `ui.el` | Theme system (doom-themes, nord), theme toggling, daemon frame setup |
| `dashboard.el` | Startup screen |
| `completion.el` | Vertico, Corfu, Consult, Orderless |
| `programming.el` | Eglot (LSP), tree-sitter, DAP, language-specific modes |
| `ai.el` | AI coding assistants (claude-code-ide) |
| `per-project.el` | Project-local configuration via `.dir-locals.el` |
| `writing.el` | Org-mode, documentation tools |
| `git.el` | Magit integration |
| `help.el` | Enhanced help system |
| `systems.el` | System administration tools |
| `platforms.el` | Linux/macOS/Android adaptations |
| `android.el` | Android-specific features (loaded conditionally) |

Not loaded by `init.el` directly: `utils.el` (required by `writing.el`), `collaboration.el` (unused).

**Module conventions**: Each module must `(provide 'module-name)` at the end and is loaded via `(require 'module-name)`. Load order in `init.el` matters — modules can depend on earlier ones.

### Nix build system

```
flake.nix                  # Entry point, uses flake-parts + emacs-overlay
├── emacs.nix              # Builds Emacs 30 (PGTK) with all packages + runtime deps
│   ├── nix/lib/dependencies.nix   # Auto-extracts packages from use-package declarations
│   └── nix/lib/runtime-deps.nix   # LSP servers, fonts, CLI tools, tree-sitter grammars
├── default.nix            # jotain package (copies init.el + early-init.el to share/jotain/)
├── shell.nix              # Dev shell (emacs-dev, jot wrappers, LSP servers, formatters)
├── nix/overlays/default.nix  # Overlay: builds jotain-modules Emacs package from elisp/
├── nix/modules/home/default.nix  # Home Manager module (programs.jotain options)
└── nix/modules/nixos/             # NixOS module (nixosModules.default)
```

The flake uses `nix-community/emacs-overlay` for bleeding-edge Emacs builds and packages. The overlay in `nix/overlays/default.nix` composes on top of it.

**Critical flow**: The overlay in `nix/overlays/default.nix` builds `elisp/` as an Emacs package called `jotain-modules` via `trivialBuild`. This package is included as a core dependency in `emacs.nix`. Custom Emacs packages not in nixpkgs (e.g., `claude-code-ide`) are also defined here.

**Dependency auto-extraction**: `nix/lib/dependencies.nix` uses pure Nix regex to scan all `.el` files for `(use-package PACKAGE-NAME` declarations and maps them to nixpkgs emacsPackages. When adding a new package:
- With `:ensure t` — auto-extracted if package name matches nixpkgs
- Name mismatch — add manual mapping in `packageNameMap`
- Built-in packages — map to `null` in `packageNameMap`
- Use `:nodep` in a `use-package` form to suppress auto-extraction
- Use `:ensure nil` to prevent extraction (for built-in features)

### Formatting

Formatting uses `treefmt-nix` configured in `flake.nix`. Running `just format` or `nix fmt` applies:
- `nixpkgs-fmt` for `.nix` files
- `shfmt` + `shellcheck` for shell scripts
- Emacs built-in `indent-region` for `.el` files

### Adding packages

1. Add a `use-package` configuration in the appropriate `elisp/*.el` file (auto-extracted if name matches)
2. For name mismatches, add a manual mapping in `nix/lib/dependencies.nix`
3. For packages not in nixpkgs, add a `trivialBuild` in `nix/overlays/default.nix`
4. For runtime tools (LSP servers, CLI tools, fonts, tree-sitter grammars), add to `nix/lib/runtime-deps.nix`

### User customization

`~/.config/emacs/custom.el` is auto-loaded if it exists and persists across rebuilds.

### Tree-sitter

Grammar paths are set in `early-init.el` from the `TREE_SITTER_DIR` environment variable, which is injected by `emacs.nix` at build time. All available grammars from nixpkgs are included automatically via `pkgs.tree-sitter.builtGrammars`.

### Theme system (`elisp/ui.el`)

- `jotain-theme-light` maps to `doom-nord-light`; `jotain-theme-dark` maps to `nord`
- `jotain-ui--disable-all-themes` is advised before every `load-theme` call to prevent theme blending
- In daemon mode, themes are applied after frame creation via `server-after-make-frame-hook`
- Toggle keybinding: `C-c t`

### Testing architecture

- ERT tests live in `tests/`, auto-discovered by `tests/test-all.el` (loads all `test-*.el` except `test-all.el`, `test-helpers.el`, `test-suite-*.el`)
- Tests are tagged: `smoke`, `fast`, `unit`, `integration`, `slow`
- Tests run in an isolated home (`HOME=.dev-home`) to avoid contaminating user config
- `test-helpers.el` mirrors `early-init.el` settings (use-package, tree-sitter) since tests run with `-Q`
- NMT integration tests live in `nmt-tests/` and validate the Home Manager module options
- The VM runtime test (`test-emacs-runtime`) only runs when `CI=1`
- `specs/` holds speckit feature specifications (spec.md, plan.md, tasks.md per feature), organized as `specs/NNN-feature-name/`

### Home Manager module options

| Option | Default | Description |
|--------|---------|-------------|
| `programs.jotain.enable` | `false` | Enable Jotain |
| `programs.jotain.package` | `pkgs.jotain` | The Jotain configuration package |
| `programs.jotain.enableDaemon` | `true` | Run Emacs as systemd/launchd service |
| `programs.jotain.includeRuntimeDeps` | `true` | Install LSP servers, fonts, CLI tools |
| `programs.jotain.extraPackages` | `epkgs: []` | Add extra Emacs packages |

### CI

GitHub Actions runs on push to main and PRs. Uses cachix (`jylhis` + `nix-community`) for binary caching. The workflow builds the default package and runs `nix flake check`.

