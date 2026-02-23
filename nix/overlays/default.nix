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

    # Claude Code IDE integration (not yet in nixpkgs)
    # https://github.com/manzaltu/claude-code-ide.el
    claude-code-ide = efinal.trivialBuild {
      pname = "claude-code-ide";
      version = "0.2.6";
      src = final.fetchFromGitHub {
        owner = "manzaltu";
        repo = "claude-code-ide.el";
        rev = "5f12e60c6d2d1802c8c1b7944bbdf935d5db1364";
        sha256 = "148xcrqff6khpwf8nnadcyvz8h6mk45xz1498k0wbzy80yzd2axn";
      };
      packageRequires = with efinal; [ websocket transient web-server ];
    };
  });
}
