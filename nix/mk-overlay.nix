{
  jylhisEmacsSrc ? null,
}:
final: _prev:
let
  usePackage = import ./use-package.nix { inherit (final) lib; };
  extraPackages = import ./extra-packages.nix { pkgs = final; };

  mkJotainEmacsPackages =
    {
      name,
      package,
    }:
    let
      core = usePackage.emacsWithPackagesFromUsePackage {
        config = ../lisp;
        inherit package;
        inherit (final) emacsPackagesFor;
        override = extraPackages;
        # `nix-ts-mode` is referenced directly (not guarded): init-lang-nix.el
        # declares it `:ensure nil`, so if a nixpkgs snapshot ever lacks it we
        # want a loud build failure rather than silently shipping a broken
        # autoload for `.nix' files. Every nixpkgs in [24.05, unstable] ships
        # it, so the 24.05+ override path is unaffected.
        extraEmacsPackages = epkgs: [
          epkgs.claude-code-ide
          epkgs.combobulate
          epkgs.jylhis-emacs-themes
          epkgs.nix-ts-mode
          epkgs.tagref
          epkgs.treesit-grammars.with-all-grammars
        ];
      };
    in
    final.runCommand name
      {
        nativeBuildInputs = [
          # Top-level `lndir` only exists on recent nixpkgs; on older
          # releases (24.05+) it lives under the xorg package set.
          (final.lndir or final.xorg.lndir)
          final.makeBinaryWrapper
        ];
        meta = (core.meta or { }) // {
          mainProgram = "emacs";
        };
        passthru = (core.passthru or { }) // {
          inherit core package;
          info = final.jotainInfo;
        };
      }
      ''
        mkdir -p $out
        lndir -silent ${core} $out

        # Re-wrap anything under bin/ that may launch Emacs, so the info
        # path propagates through `emacs', `emacsclient', the `.app'
        # bundle (macOS), and any helper tools that shell out to emacs.
        for prog in $out/bin/*; do
          [ -L "$prog" ] || continue
          orig=$(readlink -f "$prog")
          rm "$prog"
          makeBinaryWrapper "$orig" "$prog" \
            --suffix INFOPATH : "${final.jotainInfo}/share/info:"
        done
      '';
in
{
  jotainEmacs = import ../emacs.nix { pkgs = final; };

  # Terminal-only (`-nw`) build, used by the nix-on-droid module: Android
  # under proot is headless, so a GUI Emacs would only bloat the closure
  # with unusable X/Wayland libraries.
  jotainEmacsNoGui = import ../emacs.nix {
    pkgs = final;
    noGui = true;
  };

  jylhisEmacs = import ../emacs-jylhis.nix (
    {
      pkgs = final;
    }
    // final.lib.optionalAttrs (jylhisEmacsSrc != null) {
      src = jylhisEmacsSrc;
    }
  );

  sonarlintLs = final.sonarlint-ls;

  # Prebuilt ECA server binary for the eca-emacs client (lisp/init-ai.el).
  # Surfaced on the overlay so module-system / Home Manager consumers can put
  # it on the wrapper PATH.
  eca = import ./eca-server.nix { pkgs = final; };

  jotainInfo = import ./info-manual.nix {
    pkgs = final;
    src = ../.;
  };

  # Wrap the Emacs-with-packages output so jotain.info becomes
  # discoverable from `C-h i d' without touching user Elisp config.
  #
  # emacsWithPackages builds a wrapper whose share/info is a hard
  # symlink to the bare emacs.nix derivation, which we cannot mutate.
  # Instead we produce an outer derivation that (a) lndirs the core
  # wrapper verbatim, and (b) re-wraps the user-facing binaries to
  # prepend ${jotainInfo}/share/info to $INFOPATH.  The trailing ':'
  # tells Emacs's info-initialize to append Info-default-directory-list
  # so the built-in Emacs manuals stay visible.
  jotainEmacsPackages = mkJotainEmacsPackages {
    name = "jotain-emacs-full";
    package = final.jotainEmacs;
  };

  # Full distribution on the terminal-only base (nix-on-droid / headless).
  jotainEmacsPackagesNoGui = mkJotainEmacsPackages {
    name = "jotain-emacs-full-nox";
    package = final.jotainEmacsNoGui;
  };

  jylhisEmacsPackages = mkJotainEmacsPackages {
    name = "jylhis-emacs-full";
    package = final.jylhisEmacs;
  };
}
