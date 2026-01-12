# Runtime dependencies for Jotain Emacs configuration
#
# This file defines all external runtime dependencies that Emacs packages require,
# including tree-sitter grammars, fonts, LSP servers, and CLI tools.
#
# These dependencies are NOT Emacs packages - they are external programs and data
# that Emacs packages invoke at runtime. For Emacs Lisp packages, see dependencies.nix.
#
# Usage:
#   let
#     jotainLib = import ./nix/lib { inherit lib pkgs; };
#     runtimeDeps = jotainLib.runtimeDeps;
#   in {
#     # In home-manager or NixOS configuration:
#     home.packages = runtimeDeps.allRuntimeDeps;
#     fonts.packages = runtimeDeps.fonts;
#
#     # Or selectively:
#     home.packages = runtimeDeps.lspServers ++ runtimeDeps.cliTools;
#   }

{ lib, pkgs }:

let
  # Helper to check if package exists in pkgs and is not unfree
  # Returns the package or null if not found or unfree
  optionalPackage = name:
    let
      pkg = pkgs.${name} or null;
      # Try to evaluate the package, catching unfree errors
      evalResult = builtins.tryEval (
        if pkg == null then
          { success = false; value = null; }
        else
          { success = true; value = pkg.outPath; }
      );
    in
    if evalResult.success && pkg != null then
      pkg
    else
      null;

  # ============================================================================
  # TREE-SITTER GRAMMARS
  # ============================================================================
  # Tree-sitter provides fast, incremental parsing for syntax highlighting and
  # code analysis. These grammars are used by treesit-auto and tree-sitter modes.
  #
  # Referenced in:
  # - elisp/programming.el: treesit-auto configuration
  # - Emacs 29+ built-in treesit
  #
  # How to add new grammars:
  # 1. Find the grammar in nixpkgs: nix search nixpkgs tree-sitter-grammars
  # 2. Add to the list below: tree-sitter-grammars.tree-sitter-<language>
  # 3. Grammar will be automatically available to treesit-auto

  treesitterGrammars = with pkgs.tree-sitter-grammars; [
    # Core languages
    tree-sitter-bash
    tree-sitter-c
    tree-sitter-cpp
    tree-sitter-go
    tree-sitter-gomod
    tree-sitter-javascript
    tree-sitter-json
    tree-sitter-lua
    tree-sitter-markdown
    tree-sitter-nix
    tree-sitter-python
    tree-sitter-rust
    tree-sitter-typescript
    tree-sitter-yaml

    # Web development
    tree-sitter-tsx
    tree-sitter-css
    tree-sitter-html

    # Additional languages (add as needed)
    # tree-sitter-haskell
    # tree-sitter-java
    # tree-sitter-ruby
    # tree-sitter-toml
  ];

  # ============================================================================
  # FONTS
  # ============================================================================
  # Font packages referenced in elisp/fonts.el configuration.
  #
  # Font preferences (in order):
  # - Default/Fixed-pitch: JetBrainsMono, FiraCode, Iosevka, CascadiaCode Nerd Fonts
  # - Variable-pitch: Inter, SF Pro Text, Segoe UI
  # - Serif: Source Serif Pro, Liberation Serif
  # - Emoji: Noto Color Emoji
  #
  # How to add new fonts:
  # 1. Find font in nixpkgs: nix search nixpkgs <font-name>
  # 2. Add to appropriate category below
  # 3. Update elisp/fonts.el with font preferences if needed

  fonts = [

    pkgs.jetbrains-mono

    pkgs.fira-code
    pkgs.iosevka
    pkgs.cascadia-code
    pkgs.hack-font

    # UI and variable-pitch fonts
    # Used by: elisp/fonts.el j10s-fonts-variable-family
    pkgs.inter # Modern UI font (primary)

    # Serif fonts for formal writing
    # Used by: elisp/fonts.el j10s-fonts-serif-family, elisp/writing.el
    pkgs.source-serif-pro
    pkgs.liberation_ttf # Contains Liberation Serif

    # Emoji support
    # Used by: elisp/fonts.el j10s-fonts-setup-performance
    pkgs.noto-fonts-color-emoji
  ];

  # ============================================================================
  # LSP SERVERS
  # ============================================================================
  # Language Server Protocol servers for eglot integration.
  #
  # Referenced in:
  # - elisp/programming.el: eglot-server-programs configuration
  #
  # How to add new LSP servers:
  # 1. Find LSP server in nixpkgs: nix search nixpkgs <language>-lsp
  # 2. Add to list below with comment indicating language
  # 3. Configure in elisp/programming.el if custom settings needed

  lspServers = builtins.filter (x: x != null) [
    # Nix
    pkgs.nil # Nix LSP server (used for .nix files)

    # Go
    pkgs.gopls # Official Go LSP server

    # Bash
    pkgs.nodePackages.bash-language-server

    # Python (optional - uncomment if using Python development)
    # pkgs.python3Packages.python-lsp-server
    # pkgs.pyright  # Microsoft's Python LSP (alternative)

    # Rust (optional - usually handled by rustup)
    # pkgs.rust-analyzer

    # TypeScript/JavaScript
    pkgs.nodePackages.typescript-language-server

    # YAML
    pkgs.yaml-language-server

    # Docker
    pkgs.dockerfile-language-server # Docker language server for Dockerfiles

    # JSON
    # pkgs.nodePackages.vscode-langservers-extracted  # Provides jsonls

    # Markdown
    pkgs.marksman # Markdown LSP server

    # C/C++
    pkgs.clang-tools # Provides clangd LSP server

    # SonarLint (multi-language code analysis)
    # pkgs.sonarlint-ls # SonarLint language server for code quality analysis (broken build)

    # Additional LSP servers (add as needed):
    # pkgs.terraform-ls  # Terraform
    # pkgs.haskell-language-server  # Haskell
    # pkgs.lua-language-server  # Lua
  ];

  # ============================================================================
  # CLI TOOLS
  # ============================================================================
  # Command-line tools used by Emacs packages.
  # These tools are invoked via executable-find or shell-command.
  #
  # How to add new tools:
  # 1. Search elisp/*.el for (executable-find "tool-name") or shell-command usage
  # 2. Find package in nixpkgs: nix search nixpkgs <tool-name>
  # 3. Add to list below with comment indicating which package uses it

  cliTools = builtins.filter (x: x != null) (
    [
      # Search and navigation tools
      # Used by: elisp/completion.el (consult-ripgrep, consult-fd)
      # Used by: elisp/programming.el (xref-search-program)
      pkgs.ripgrep # Fast recursive grep (rg command)
      pkgs.fd # Fast find alternative (fd command)

      # Version control
      # Used by: elisp/git.el (magit, diff-hl)
      # NOTE: git is NOT included here - it's a system-level dependency that users
      # should manage separately (e.g., via programs.git.enable or home.packages).
      # Including it causes conflicts when users have custom git configurations
      # (git-with-svn, git-full, etc.). Magit will find git via PATH.

      # Directory environment management
      # Used by: elisp/programming.el (direnv-mode)
      pkgs.direnv

      # File navigation with frecency
      # Used by: elisp/completion.el (zoxide package)
      (optionalPackage "zoxide")

      # Secrets management
      # Used by: elisp/systems.el (sops package)
      (optionalPackage "sops")

      # 1Password CLI integration
      # Used by: elisp/systems.el (auth-source-1password)
      # NOTE: Commented out because it's unfree. Install manually if needed.
      # (optionalPackage "_1password-cli") # Provides 'op' command

      # Markdown rendering
      # Used by: elisp/programming.el (markdown-mode)
      (optionalPackage "multimarkdown")

      # Additional tools (uncomment as needed):
      # pkgs.sqlite  # Used by org-roam, forge
      # pkgs.graphviz  # Used by org-mode diagrams
      # pkgs.imagemagick  # Image manipulation for org-mode
      # pkgs.pandoc  # Universal document converter
      # pkgs.aspell  # Spell checker (or hunspell)
      # pkgs.hunspell  # Alternative spell checker
      # pkgs.dictionaries  # For flyspell dictionaries
    ]
    # Terminal emulator dependencies (Linux only)
    # Used by: elisp/programming.el (vterm package)
    # Note: libvterm is not available on Darwin (macOS) systems
    ++ lib.optionals pkgs.stdenv.isLinux [
      pkgs.libvterm # Required by emacs-vterm
    ]
  );

  # ============================================================================
  # CONVENIENCE AGGREGATES
  # ============================================================================
  # Pre-combined lists for common use cases

  # All runtime dependencies except fonts
  # Use this in home.packages or environment.systemPackages
  allRuntimeDeps =
    builtins.filter (x: x != null) (
      lspServers
      ++ cliTools
      # Note: treesitterGrammars are typically installed via Emacs package config
      # but can be added here if you want system-wide availability
    );

  # All runtime dependencies including fonts
  # Use this for complete system deployment
  allWithFonts =
    builtins.filter (x: x != null) (
      lspServers
      ++ cliTools
      ++ fonts
    );

  # Development environment (LSP + essential CLI tools)
  # Use this in nix develop shells or devShells
  # NOTE: git is expected to be provided by the system (not included here)
  devEnv =
    builtins.filter (x: x != null) (
      lspServers
      ++ [
        pkgs.ripgrep
        pkgs.fd
        pkgs.direnv
      ]
    );

in
{
  inherit
    fonts
    lspServers
    cliTools
    allRuntimeDeps
    allWithFonts
    devEnv;

  # Use camelCase for consistency with emacs.nix
  treeSitterGrammars = treesitterGrammars;
}
