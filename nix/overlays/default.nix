# Jotain overlays
{ inputs, ... }:

final: prev: {
  # Add jotain configuration package to pkgs
  jotain = final.callPackage ../.. { };

  # Add jotain Emacs package (Emacs with all dependencies)
  jotainEmacs = final.callPackage ../../emacs.nix { devMode = false; };

  # Custom Emacs package overrides can go here
  emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope (efinal: esuper: {
    # Example: Override a package
    # magit = esuper.magit.overrideAttrs (old: {
    #   # customizations
    # });
  });
}
