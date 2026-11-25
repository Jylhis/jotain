final: _prev:
let
  inherit (final) lib;
  extraPackages = import ./extra-packages.nix { pkgs = final; };
  nixProvidedPackages = import ./nix-provided-packages.nix;

  runtimeDeps = import ./runtime-deps.nix {
    pkgs = final;
    pkgsWithOverlay = final;
  };

  # devenv.el and repo-review.el hold no use-package forms, and the Nix
  # elisp reader cannot parse devenv.el's `?─` character literal.
  lispFiles = lib.fileset.toList (
    lib.fileset.fileFilter (f: lib.hasPrefix "init-" f.name && f.hasExt "el") ../lisp
  );

  core = final.emacsWithPackagesFromUsePackage {
    config = lib.concatMapStringsSep "\n" builtins.readFile lispFiles;
    package = final.jotainEmacs;
    alwaysEnsure = true;
    override = extraPackages;
    extraEmacsPackages =
      epkgs: map (n: epkgs.${n}) nixProvidedPackages ++ [ epkgs.treesit-grammars.with-all-grammars ];
  };
in
{
  jotainEmacs = final.emacs-unstable-pgtk;

  eca = import ./eca-server.nix { pkgs = final; };

  likec4Lsp = import ./likec4-lsp.nix { pkgs = final; };

  jotainInfo = import ./info-manual.nix {
    pkgs = final;
    src = ../.;
  };

  # INFOPATH gets the bare directory: makeBinaryWrapper rejects an empty
  # PATH-like segment (GHSA-p7v3-pr2c-8584), and nixpkgs' site-start.el adds
  # the trailing ':' that keeps the built-in manuals visible (bug#81105).
  jotainEmacsPackages = import ./wrap-runtime-deps.nix {
    pkgs = final;
    package = core;
    name = "jotain-emacs-full";
    inherit runtimeDeps;
    spellDictionaries = with final.aspellDicts; [
      en
      fi
      de
      fr
    ];
    aspellDefault = true;
    infoDirs = [ "${final.jotainInfo}/share/info" ];
    passthru = (core.passthru or { }) // {
      inherit core;
      package = final.jotainEmacs;
      info = final.jotainInfo;
    };
  };

  jotainEmacsConfigured = import ./configured.nix {
    pkgs = final;
    package = final.jotainEmacsPackages;
  };
}
