# overlay.nix — Nixpkgs overlay for Jotain Emacs.
#
# Adds:
#   jotainEmacs           — bare Emacs binary (emacs.nix defaults)
#   jylhisEmacs           — bare Emacs from github:jylhis/emacs
#   jotainInfo            — Jotain manual (share/info/jotain.info + dir)
#   jotainEmacsPackages   — full distribution using jotainEmacs
#   jylhisEmacsPackages   — full distribution using jylhisEmacs
#
# Usage:
#   import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; }
final: _prev:
let
  usePackage = import ./nix/use-package.nix { inherit (final) lib; };
  extraPackages = import ./nix/extra-packages.nix { pkgs = final; };

  mkJotainEmacsPackages =
    {
      name,
      package,
    }:
    let
      core = usePackage.emacsWithPackagesFromUsePackage {
        config = ./lisp;
        inherit package;
        inherit (final) emacsPackagesFor;
        override = extraPackages;
        extraEmacsPackages = epkgs: [
          epkgs.claude-code-ide
          epkgs.combobulate
          epkgs.jylhis-emacs-themes
          epkgs.nix-ts-mode
          epkgs.treesit-grammars.with-all-grammars
        ];
      };
    in
    final.runCommand name
      {
        nativeBuildInputs = [
          final.lndir
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
  jotainEmacs = import ./emacs.nix { pkgs = final; };
  jylhisEmacs = import ./emacs-jylhis.nix { pkgs = final; };

  sonarlintLs = final.sonarlint-ls;

  dockerfileLs = final.dockerfile-language-server;

  jotainInfo = import ./nix/info-manual.nix {
    pkgs = final;
    src = ./.;
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

  jylhisEmacsPackages = mkJotainEmacsPackages {
    name = "jylhis-emacs-full";
    package = final.jylhisEmacs;
  };
}
