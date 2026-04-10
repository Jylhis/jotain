# overlay.nix — Nixpkgs overlay for Jotain Emacs.
#
# Adds:
#   jotainEmacs          — bare Emacs binary (emacs.nix defaults)
#   jotainEmacsPackages  — full distribution with use-package mapper + tree-sitter
#
# Usage:
#   import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; }
final: _prev:
let
  usePackage = import ./nix/use-package.nix { inherit (final) lib; };
  extraPackages = import ./nix/extra-packages.nix { pkgs = final; };
in
{
  jotainEmacs = import ./emacs.nix { pkgs = final; };

  jotainEmacsPackages = usePackage.emacsWithPackagesFromUsePackage {
    config = ./lisp;
    package = final.jotainEmacs;
    emacsPackagesFor = final.emacsPackagesFor;
    override = extraPackages;
    extraEmacsPackages = epkgs: [
      epkgs.claude-code-ide
      epkgs.combobulate
      epkgs.treesit-grammars.with-all-grammars
    ];
  };
}
