# overlay.nix — Nixpkgs overlay for Jotain Emacs.
#
# Adds:
#   jotainEmacs          — bare Emacs binary (emacs.nix defaults)
#   jotainInfo           — Jotain manual (share/info/jotain.info + dir)
#   jotainEmacsPackages  — full distribution: emacs wrapper + use-package
#                          mapper + tree-sitter + bundled Info manual
#
# Usage:
#   import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; }
final: _prev:
let
  usePackage = import ./nix/use-package.nix { inherit (final) lib; };
  extraPackages = import ./nix/extra-packages.nix { pkgs = final; };

  jotainEmacsPackagesCore = usePackage.emacsWithPackagesFromUsePackage {
    config = ./lisp;
    package = final.jotainEmacs;
    inherit (final) emacsPackagesFor;
    override = extraPackages;
    extraEmacsPackages = epkgs: [
      epkgs.claude-code-ide
      epkgs.combobulate
      epkgs.treesit-grammars.with-all-grammars
    ];
  };
in
{
  jotainEmacs = import ./emacs.nix { pkgs = final; };

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
  jotainEmacsPackages =
    final.runCommand "jotain-emacs-full"
      {
        nativeBuildInputs = [
          final.lndir
          final.makeBinaryWrapper
        ];
        meta = (jotainEmacsPackagesCore.meta or { }) // {
          mainProgram = "emacs";
        };
        passthru = (jotainEmacsPackagesCore.passthru or { }) // {
          core = jotainEmacsPackagesCore;
          info = final.jotainInfo;
        };
      }
      ''
        mkdir -p $out
        lndir -silent ${jotainEmacsPackagesCore} $out

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
}
