# Jotain overlays
{ inputs, ... }:

final: prev: {
  # Add jotain to pkgs
  jotain = final.callPackage ../.. { };

  # Custom Emacs package overrides can go here
  emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope (efinal: esuper: {
    # Example: Override a package
    # magit = esuper.magit.overrideAttrs (old: {
    #   # customizations
    # });
  });
}
