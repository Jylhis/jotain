# Nix Dependencies Reference

Extended documentation for jotain's auto-extraction mechanism and manual mapping patterns.

## How Auto-Extraction Works

`nix/lib/dependencies.nix` scans all `elisp/*.el` files at build time and:

1. Finds every `(use-package NAME :ensure ...)` declaration
2. Maps `NAME` to a nixpkgs emacs package attribute
3. Includes the package in the Emacs build

The scanner uses simple pattern matching — it does not evaluate Elisp. This means:
- Commented-out `use-package` forms may still be picked up
- Dynamically-constructed package names won't be found
- The `:ensure` keyword (without `nil`) triggers extraction

## Manual Mappings

When the package name in `use-package` differs from the nixpkgs attribute, add a mapping:

```nix
# In nix/lib/dependencies.nix, in the manual mappings attrset:
{
  # use-package name = nixpkgs emacs package attribute
  "company" = "company";              # same name, explicit
  "lsp-mode" = "lsp-mode";           # hyphen preserved
  "markdown-mode" = "markdown-mode"; # usually match
  "some-custom-name" = "epkgs.actualNixpkgsName";  # name mismatch
}
```

### Common Naming Patterns in nixpkgs

| use-package name | nixpkgs attribute | Notes |
|-----------------|-------------------|-------|
| `magit` | `magit` | Direct match |
| `doom-themes` | `doom-themes` | Direct match |
| `vertico` | `vertico` | Direct match |
| `nix-mode` | `nix-mode` | Direct match |
| `treesit-auto` | `treesit-auto` | Direct match |

Use `nix search nixpkgs emacs` to find available packages.

## Suppressing Auto-Extraction

`:nodep` in a `use-package` form suppresses extraction for that specific package:

```elisp
;; This package is provided by a different mechanism:
(use-package some-package
  :ensure :nodep
  :config ...)

;; Or: package is built-in, no Nix installation needed
(use-package built-in-package
  :ensure nil
  :config ...)
```

Use `:nodep` when:
- The package comes from a custom overlay
- The package is a meta-package grouping multiple nixpkgs packages
- The package name would incorrectly match an unrelated nixpkgs package

## Runtime Dependencies

Runtime dependencies (not Emacs Lisp packages) go in `nix/lib/runtime-deps.nix`:

```nix
# Categories in runtime-deps.nix:
{
  lspServers = with pkgs; [ rust-analyzer pyright ];
  fonts = with pkgs; [ fira-code jetbrains-mono ];
  cliTools = with pkgs; [ ripgrep fd bat ];
  treeSitterGrammars = with pkgs.tree-sitter-grammars; [
    tree-sitter-nix
    tree-sitter-python
  ];
}
```

Tree-sitter grammar paths are injected into Emacs via the `TREE_SITTER_DIR` environment variable set in `emacs.nix`.

## Overlay Architecture

The emacs-overlay is applied in `nix/overlays/default.nix`. It provides:
- Up-to-date Emacs packages (ahead of nixpkgs stable)
- PGTK build of Emacs 30
- Binary cache for common configurations

If a package exists in both nixpkgs stable and emacs-overlay, the overlay version takes precedence.

## NMT Test Structure

NMT tests in `nmt-tests/` validate the Home Manager module:

```
nmt-tests/
├── module-enabled.nix     # Basic enable test
├── module-daemon.nix      # Daemon mode test
├── module-no-deps.nix     # includeRuntimeDeps=false test
└── module-extra-pkgs.nix  # extraPackages test
```

Each test file is a standalone Home Manager configuration that asserts specific file/service properties after module activation.
