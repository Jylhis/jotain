# Jotain overlays
{ inputs, ... }:

final: prev: {
  # Add jotain configuration package to pkgs
  jotain = final.callPackage ../.. { };

  # Add jotain Emacs package (Emacs with all dependencies)
  jotainEmacs = final.callPackage ../../emacs.nix { devMode = false; };

  # Custom Emacs package overrides
  emacsPackagesFor = emacs: (prev.emacsPackagesFor emacs).overrideScope (efinal: esuper: {
    # Jotain elisp modules as a proper Emacs package
    jotain-modules = efinal.trivialBuild {
      pname = "jotain-modules";
      version = "0.1.0";
      src = ../../elisp;
      # Skip byte-compilation: android.el requires platform.el but comes
      # first alphabetically. Native compilation will JIT-compile at runtime.
      buildPhase = ":";
    };
  });
}
