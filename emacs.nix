# Emacs with Jotain packages
{ pkgs
, lib
, devMode ? false
, ...
}:

let
  # Import our lib
  jotainLib = import ./nix/lib { inherit lib pkgs; };

  # Core packages always needed
  corePackages = epkgs: with epkgs; [
    use-package
    no-littering
  ];

  # Get packages from elisp directory if it exists
  autoPackages = epkgs:
    let
      elispDir = ./elisp;
    in
    if builtins.pathExists elispDir then
      jotainLib.dependencies.getPackagesForDirectory elispDir epkgs
    else
      [ ];

  # Development-only packages
  devPackages = epkgs: with epkgs; [
    package-lint
    flycheck
  ];

  # All packages
  allPackages = epkgs:
    corePackages epkgs
    ++ autoPackages epkgs
    ++ (if devMode then devPackages epkgs else [ ]);

  # Use Emacs 30 (or latest from overlay)
  baseEmacs = pkgs.emacs30-pgtk or pkgs.emacs-pgtk or pkgs.emacs;

in
(pkgs.emacsPackagesFor baseEmacs).emacsWithPackages allPackages
