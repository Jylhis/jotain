# AGENTS.md

Guidance for AI coding agents working in this repository.

## What This Is

Jotain is a modular Emacs 30+ configuration managed entirely by Nix Flakes. All Emacs packages, LSP servers, fonts, and tree-sitter grammars are installed via Nix — never via `package.el` or MELPA at runtime. The project also provides a home-manager module so users can integrate Jotain declaratively into their NixOS/home-manager setups.

## Commands

All common tasks are defined in the `justfile`. Run `just` with no arguments to list them.

```bash
# Testing (fastest to slowest)
just test-smoke             # < 1 second, ultra-fast sanity checks
just test-fast              # < 5 seconds, unit tests excluding slow I/O
just test                   # Full ERT test suite
just test-all               # ERT + NMT home-manager module tests (excludes VM)
just test-all-plus-runtime  # Includes VM-based nixosTest

# Run tests by tag (smoke, fast, unit, integration, slow)
just test-tag <TAG>

# Build
just build                  # nix build .#default
just check                  # nix build --dry-run (syntax only)

# Formatting
just format                 # nixfmt, yamlfmt, actionlint, deadnix, statix

# Interactive dev (isolated, won't touch your real ~/.emacs.d)
just emacs-dev

# Nix
just update                 # Update all flake inputs
just update-input <INPUT>   # Update one input
```

For byte-compiling locally during development: `just compile`

For parallel CI-style checks: `just check-parallel`

## Architecture

### Emacs Lisp Layer (`elisp/`, `init.el`, `early-init.el`)

`init.el` is the entry point. It loads modules in order via `(require ...)`:

```
platform → core → fonts → ui → dashboard → completion →
programming → per-project → writing → git → help → systems → platforms → android?
```

Each file in `elisp/` is a self-contained module that `(provide 'module-name)`. Modules use `use-package` for declarative configuration. `use-package-always-ensure` is forced `nil` — packages must never be auto-installed.

`early-init.el` runs before frame creation: disables UI chrome, configures native compilation, sets `treesit-extra-load-path` from the `TREE_SITTER_DIR` env var injected by Nix.

### Nix Layer

| File/Dir | Purpose |
|---|---|
| `flake.nix` | All flake outputs (packages, checks, modules, devShell) |
| `default.nix` | Emacs package build definition |
| `emacs.nix` | Wraps Emacs with Jotain config + runtime env vars |
| `config.nix` | Config-only package (just the elisp files) |
| `nix/lib/dependencies.nix` | **Auto-extracts** Emacs package deps from `use-package` declarations using pure Nix regex |
| `nix/lib/runtime-deps.nix` | LSP servers, CLI tools, fonts, tree-sitter grammars |
| `nix/modules/home/` | Home-manager module (`programs.jotain.*`) |
| `nix/modules/nixos/` | NixOS module |
| `nix/overlays/` | nixpkgs overlays |

**Key mechanism:** `nix/lib/dependencies.nix` parses all `(use-package PACKAGE-NAME ...)` declarations at evaluation time and maps them to nixpkgs `emacsPackages`. Packages marked `:ensure nil` or `:nodep` are excluded. Built-ins (`project`, `diff-mode`, `xt-mouse`) are listed in `packageNameMap` with `null` values to skip them. When adding a new `use-package` for an external package, no manual wiring is needed — just write the `use-package` block and it's picked up automatically.

### Testing Layer

ERT tests live in `tests/`. They use tag-based filtering:

- `:tags '(smoke critical)` — runs in < 1s
- `:tags '(fast unit)` — fast I/O-free unit tests
- `:tags '(integration slow)` — allowed to do filesystem/process work

`tests/test-helpers.el` provides shared test utilities. `tests/test-all.el` is the loader — new test files must be `require`d there.

NMT tests in `nmt-tests/` validate the home-manager module using actual home-manager evaluation (no VM needed).

The runtime test (`test-emacs-runtime`) boots a NixOS VM and validates Emacs starts cleanly — it is excluded from `just test-all` and only runs in CI when `CI=1`.

## Adding Packages

1. Add a `use-package` block to the appropriate `elisp/*.el` file — dependency extraction is automatic.
2. If the nixpkgs name differs from the elisp package name, add a mapping in `nix/lib/dependencies.nix` → `packageNameMap`.
3. For runtime tools (LSP servers, CLI binaries, fonts, tree-sitter grammars), add to `nix/lib/runtime-deps.nix`.
4. Run `just test` to verify.

## Home-Manager Module Options

```nix
programs.jotain = {
  enable = true;
  enableDaemon = true;           # systemd/launchd service (default: true)
  includeRuntimeDeps = true;     # LSP servers, fonts, CLI tools (default: true)
  extraPackages = epkgs: [];     # Additional Emacs packages
};
```

User customizations go in `~/.config/emacs/custom.el` — this file persists across rebuilds.

## Key Constraints

- **No `package-archives`:** `package-archives` is set to `nil`. All packages come from Nix. Never add MELPA/ELPA entries.
- **Emacs 30+ only:** `init.el` declares `(emacs "30.2")`. Do not use APIs unavailable in Emacs 30.
- **Nix-managed tree-sitter:** Grammars are injected via `TREE_SITTER_DIR`. Do not call `treesit-install-language-grammar` manually.
- **Isolated dev env:** `just emacs-dev` sets `HOME` to `.dev-home/` inside the repo. The `.dev-home/` directory is gitignored and cleaned by `just clean`.
