---
name: add-package
description: Add a new Emacs package to the jotain distribution with proper use-package config and Nix integration.
argument-hint: "[package-name]"
disable-model-invocation: true
---

# Add Emacs Package

Add the package `$ARGUMENTS` to jotain following the standard procedure.

## Steps

1. **Determine the correct module** - Which `elisp/*.el` file should this package's configuration go in? Match by domain (completion, programming, git, writing, ui, etc.)

2. **Check if the package name maps to nixpkgs** - Search `nix/lib/dependencies.nix` for existing mappings. If the package name differs from the nixpkgs attribute name, add a manual mapping.

3. **Add the use-package form** in the chosen `elisp/*.el` file:
   ```elisp
   (use-package $ARGUMENTS
     :ensure
     :defer t    ; ALWAYS lazy-load
     :config
     ;; Configuration here
     )
   ```

4. **Check if runtime dependencies are needed** - If the package requires external tools (LSP servers, CLI tools, fonts, tree-sitter grammars), add them to `nix/lib/runtime-deps.nix`.

5. **Validate**:
   ```bash
   just compile      # Byte-compile check
   just test-smoke   # Smoke tests
   just check        # Nix dry-run
   ```

## Key Files

- `elisp/*.el` - Package configuration (use-package forms)
- `nix/lib/dependencies.nix` - Package name mappings (auto-extracted from use-package)
- `nix/lib/runtime-deps.nix` - Runtime tools, fonts, tree-sitter grammars
- `emacs.nix` - Main Emacs build expression

## Notes

- All packages are installed via Nix, never MELPA/ELPA at runtime
- `:ensure` in use-package triggers auto-extraction by `dependencies.nix`
- Use `:nodep` to suppress auto-extraction for a specific package
- Use `:ensure nil` for built-in packages (no Nix installation needed)
