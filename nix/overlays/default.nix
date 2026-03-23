# Jotain overlays
{ inputs, ... }:

final: prev: {
  # Workaround for https://github.com/NixOS/nixpkgs/issues/493679
  # jetbrains-mono depends on picosvg, whose test suite has floating-point
  # precision failures in v0.22.3 that block the build. Disable its checks
  # until the fix propagates to the nixpkgs revision we track.
  pythonPackagesExtensions = prev.pythonPackagesExtensions ++ [
    (_python-final: python-prev: {
      picosvg = python-prev.picosvg.overridePythonAttrs (_oldAttrs: {
        doCheck = false;
      });
    })
  ];

  # Add jotain configuration package to pkgs
  jotain = final.callPackage ../.. { };

  # Add jotain Emacs package (Emacs with all dependencies)
  jotainEmacs = final.callPackage ../../emacs.nix { devMode = false; };

}
