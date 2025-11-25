# Jotain Library Functions

This directory contains reusable Nix library functions for the Jotain Emacs configuration.

## Overview

The library is organized into modular components:

- **dependencies.nix**: Automatic Emacs package extraction from use-package declarations
- **runtime-deps.nix**: External runtime dependencies (LSP servers, CLI tools, fonts, tree-sitter grammars)
- **default.nix**: Main entry point that exports all library functions

## Usage

Import the library in your Nix expressions:

```nix
let
  jotainLib = import ./nix/lib { inherit pkgs lib; };
in {
  # Access dependency extraction functions
  emacsPackages = jotainLib.dependencies.getPackagesForDirectory ./elisp epkgs;
  
  # Access runtime dependencies
  home.packages = jotainLib.runtimeDeps.allRuntimeDeps;
  fonts.packages = jotainLib.runtimeDeps.fonts;
}
```

## Dependencies Module

### Purpose

Automatically extracts Emacs package requirements from `use-package` declarations in Elisp files, eliminating the need to manually maintain package lists in both Elisp and Nix.

### Functions

- **extractUsePackages**: Extract package names from a single Elisp file
- **scanDirectory**: Find all `.el` files and extract packages from all of them
- **getPackagesForDirectory**: Get Emacs packages ready to use with `emacsWithPackages`
- **mapToNixpkgsName**: Map Elisp package names to nixpkgs equivalents
- **listPackageNames**: Debug utility to see what packages were found

### Example

```nix
let
  jotainLib = import ./nix/lib { inherit pkgs lib; };
  
  # Automatically get all packages from elisp/ directory
  myEmacs = (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs:
    jotainLib.dependencies.getPackagesForDirectory ./elisp epkgs
  );
in
  myEmacs
```

### Special Cases

The extractor automatically handles:

- **:ensure nil**: Packages marked with `:ensure nil` are excluded (built-in packages)
- **:nodep**: Custom annotation to exclude packages from extraction
- **packageNameMap**: Maps special cases (e.g., built-in packages, local libraries)

To exclude a package from automatic installation:

```elisp
;; Option 1: Use :ensure nil
(use-package project  ; Built-in to Emacs 30+
  :ensure nil
  :config ...)

;; Option 2: Use :nodep annotation
(use-package my-local-lib
  :nodep  ; Don't try to install this from nixpkgs
  :config ...)
```

## Runtime Dependencies Module

### Purpose

Centralizes all external runtime dependencies (non-Emacs-package dependencies) that the Emacs configuration needs to function properly.

### Categories

#### 1. Tree-sitter Grammars

Language grammars for tree-sitter syntax highlighting (17 languages):

```nix
runtimeDeps.treesitterGrammars
# => [ tree-sitter-bash tree-sitter-c tree-sitter-go ... ]
```

Languages included: bash, c, cpp, css, go, gomod, html, javascript, json, lua, markdown, nix, python, rust, tsx, typescript, yaml

#### 2. Fonts

Font packages referenced in `elisp/fonts.el` (6 packages):

```nix
runtimeDeps.fonts
# => [ nerdfonts inter source-serif-pro liberation_ttf noto-fonts-color-emoji dejavu_fonts ]
```

Includes:
- **Nerd Fonts**: JetBrainsMono, FiraCode, Iosevka, CascadiaCode, Hack
- **UI Fonts**: Inter
- **Serif Fonts**: Source Serif Pro, Liberation Serif
- **Emoji**: Noto Color Emoji
- **Fallbacks**: DejaVu fonts

#### 3. LSP Servers

Language servers for eglot (7 servers):

```nix
runtimeDeps.lspServers
# => [ nil gopls bash-language-server typescript-language-server yaml-language-server marksman clang-tools ]
```

Servers included:
- **nil**: Nix LSP
- **gopls**: Go LSP
- **bash-language-server**: Bash LSP
- **typescript-language-server**: TypeScript/JavaScript LSP
- **yaml-language-server**: YAML LSP
- **marksman**: Markdown LSP
- **clang-tools**: C/C++ LSP (clangd)

#### 4. CLI Tools

Command-line tools used by Emacs packages (9 tools):

```nix
runtimeDeps.cliTools
# => [ ripgrep fd git direnv zoxide sops 1password-cli multimarkdown libvterm ]
```

Tools included:
- **ripgrep**: Fast search (consult-ripgrep, xref)
- **fd**: Fast find (consult-fd)
- **git**: Version control (magit, diff-hl)
- **direnv**: Environment management (direnv-mode)
- **zoxide**: Smart directory navigation
- **sops**: Secrets management
- **1password-cli**: 1Password integration (op command)
- **multimarkdown**: Markdown rendering
- **libvterm**: Terminal emulator backend

### Convenience Aggregates

#### allRuntimeDeps

All CLI tools and LSP servers (16 packages):

```nix
home.packages = runtimeDeps.allRuntimeDeps;
```

Use for: Home Manager or NixOS system packages (excludes fonts which should go in fonts.packages)

#### allWithFonts

All runtime dependencies including fonts (22 packages):

```nix
home.packages = runtimeDeps.allWithFonts;
```

Use for: Complete home-manager installation (fonts in home.packages, not fonts.packages)

#### devEnv

LSP servers + essential development tools (11 packages):

```nix
devShells.default = pkgs.mkShell {
  buildInputs = runtimeDeps.devEnv;
};
```

Use for: Development shells and CI environments

### Adding New Dependencies

#### Adding a Tree-sitter Grammar

1. Find the grammar: `nix search nixpkgs tree-sitter-grammars`
2. Add to `treesitterGrammars` list in `runtime-deps.nix`
3. Grammar is automatically available to treesit-auto

#### Adding a Font

1. Find the font: `nix search nixpkgs <font-name>`
2. Add to `fonts` list in `runtime-deps.nix`
3. Update `elisp/fonts.el` if you want Emacs to prefer this font

#### Adding an LSP Server

1. Find the server: `nix search nixpkgs <language>-lsp`
2. Add to `lspServers` list in `runtime-deps.nix`
3. Configure in `elisp/programming.el` if custom eglot settings needed

#### Adding a CLI Tool

1. Find tool usage: `grep -r "executable-find.*tool-name" elisp/`
2. Find package: `nix search nixpkgs <tool-name>`
3. Add to `cliTools` list in `runtime-deps.nix`

### Compatibility Handling

The module includes automatic compatibility handling for package renames:

- Uses `optionalPackage` helper for packages that may not exist
- Handles nixpkgs renames automatically (e.g., `_1password` → `_1password-cli`)
- Filters out null values to prevent evaluation errors
- Conditional nerdfonts handling for different nixpkgs versions

## Integration Example

Complete example showing both modules working together:

```nix
# emacs.nix
{ pkgs, lib, ... }:

let
  jotainLib = import ./nix/lib { inherit pkgs lib; };
  
  # Automatic Emacs package extraction
  myEmacs = (pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (epkgs:
    # Core packages
    [ epkgs.use-package epkgs.no-littering ]
    # Auto-extracted from elisp/ directory
    ++ jotainLib.dependencies.getPackagesForDirectory ./elisp epkgs
  );

in {
  # Emacs with packages
  packages.emacs = myEmacs;
  
  # Runtime dependencies
  packages.runtime = jotainLib.runtimeDeps.allRuntimeDeps;
  packages.fonts = jotainLib.runtimeDeps.fonts;
}
```

## Testing

Validate the library evaluates correctly:

```bash
# Test dependency extraction
nix eval --impure --expr 'let lib = import ./nix/lib { pkgs = import <nixpkgs> {}; lib = (import <nixpkgs> {}).lib; }; in builtins.length (lib.dependencies.scanDirectory ./elisp)'

# Test runtime dependencies
nix eval --impure --expr 'let lib = import ./nix/lib { pkgs = import <nixpkgs> {}; lib = (import <nixpkgs> {}).lib; }; in { lsp = builtins.length lib.runtimeDeps.lspServers; cli = builtins.length lib.runtimeDeps.cliTools; }'

# See what packages were found
nix eval --impure --expr 'let lib = import ./nix/lib { pkgs = import <nixpkgs> {}; lib = (import <nixpkgs> {}).lib; }; in lib.dependencies.listPackageNames ./elisp'
```

## Files

- **default.nix**: Main entry point, exports dependencies and runtimeDeps
- **dependencies.nix**: Emacs package extraction from use-package
- **runtime-deps.nix**: External runtime dependencies categorization
- **runtime-deps-example.nix**: Example usage patterns for runtime-deps
- **README.md**: This file

## See Also

- Main project documentation: `../../CLAUDE.md`
- Home Manager module: `../modules/home/default.nix`
- Emacs package builder: `../../emacs.nix`
