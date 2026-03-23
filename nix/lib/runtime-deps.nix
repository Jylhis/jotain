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
  # We now use all available grammars from nixpkgs via pkgs.tree-sitter.allGrammars.
  # No manual addition is needed.

  treesitterGrammars =
    let
      # Some grammars in nixpkgs use mutable branch refs (e.g. "release") instead
      # of pinned commits, causing hash mismatches when upstream pushes new content.
      # Filter these out until they are fixed in nixpkgs.
      broken = [ "tree-sitter-quint" ];
      grammars = builtins.removeAttrs pkgs.tree-sitter.builtGrammars (broken ++ [ "recurseForDerivations" ]);
    in
    builtins.attrValues grammars;

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
    pkgs.nerd-fonts.jetbrains-mono
    pkgs.nerd-fonts.symbols-only
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
    pkgs.nil # Nix LSP server
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

  cliTools = builtins.filter (x: x != null) [
    pkgs.ripgrep
    pkgs.fd
    pkgs.direnv
  ];

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
