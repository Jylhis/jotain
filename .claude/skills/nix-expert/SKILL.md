---
name: nix-expert
description: Nix build system conventions for the jotain Emacs distribution. Auto-loaded when editing .nix files, flake configuration, overlays, or home-manager modules.
user-invocable: false
---

# Jotain Nix Build System Conventions

Apply these conventions when editing any `.nix` file in this project.

## Project Nix Structure

```
flake.nix
├── emacs.nix                       # Builds Emacs 30 (PGTK) with all packages + runtime deps
│   ├── nix/lib/dependencies.nix    # Auto-extracts packages from use-package declarations
│   └── nix/lib/runtime-deps.nix    # LSP servers, fonts, CLI tools, tree-sitter grammars
├── nix/overlays/                   # nixpkgs overlays (includes emacs-overlay)
├── default.nix                     # jotain package (config files only)
└── nix/modules/home/default.nix    # Home Manager module (programs.jotain options)
```

## Key Mechanism: Auto-Extraction

`nix/lib/dependencies.nix` scans all `elisp/*.el` files and automatically maps `use-package` declarations to nixpkgs package names. This means:

- Adding `(use-package foo :ensure)` in elisp automatically includes `foo` from nixpkgs
- Name mismatches require a manual mapping in `dependencies.nix`
- Use `:nodep` in a `use-package` form to suppress auto-extraction
- Runtime tools (LSP servers, CLI tools, fonts, tree-sitter grammars) go in `runtime-deps.nix`

## Home Manager Module

The project exposes `homeModules.default` with these options:

| Option | Default | Description |
|--------|---------|-------------|
| `programs.jotain.enable` | `false` | Enable Jotain |
| `programs.jotain.enableDaemon` | `true` | Run Emacs as systemd/launchd service |
| `programs.jotain.includeRuntimeDeps` | `true` | Install LSP servers, fonts, CLI tools |
| `programs.jotain.extraPackages` | `epkgs: []` | Add extra Emacs packages |

Module definition lives in `nix/modules/home/default.nix`.

## Build & Validation Commands

```bash
just build          # Build Emacs package with Nix
just check          # Syntax check via Nix dry-run
just format         # Format with nixfmt, deadnix, statix
just test-nmt       # NMT home-manager module integration tests
nix flake check     # Full flake evaluation
nix flake update    # Update all flake inputs
```

To run a specific NMT check:
```bash
nix build .#checks.x86_64-linux.test-module-enabled --print-build-logs
```

## Troubleshooting Builds

```bash
nix build .#emacs --show-trace --verbose   # Debug build
nix show-derivation .#emacs                # Check derivation
nix build .#emacs --keep-failed            # Keep failed build for inspection
```

## Conventions

- All Emacs packages are installed via Nix, never via MELPA/ELPA at runtime
- Tree-sitter grammars are managed in `runtime-deps.nix`; grammar paths set via `TREE_SITTER_DIR` env var in `emacs.nix`
- NMT integration tests live in `nmt-tests/` and validate the Home Manager module
- The VM runtime test (`test-emacs-runtime`) only runs when `CI=1`
