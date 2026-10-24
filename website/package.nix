{
  lib,
  pkgs,
}:
let
  inputs = import ../../nix/inputs.nix { };
  emacsPkgs = pkgs.extend (
    lib.composeExtensions inputs.emacs-overlay.overlays.default (
      import ../../etc/emacs/nix/mk-overlay.nix { }
    )
  );
  mkSite = args: import ./site.nix ({ pkgs = emacsPkgs; } // args);

  dsAssets = import ../../etc/emacs/nix/ds-assets.nix {
    pkgs = emacsPkgs;
    inherit (emacsPkgs) bun;
  };

  ds-in-sync =
    pkgs.runCommandLocal "check-ds-in-sync"
      {
        inherit dsAssets;
        vendoredDs = ./public/ds;
      }
      ''
        if ! diff -r "$dsAssets" "$vendoredDs"; then
          echo "" >&2
          echo "public/ds is out of sync with the jylhis/design rev pinned in" >&2
          echo "etc/emacs/nix/design-pin.nix. Re-vendor it with: just ds-sync" >&2
          exit 1
        fi
        touch $out
      '';
in
{
  default = mkSite { };
  checks = {
    inherit ds-in-sync;
    build = mkSite { withApiDoc = false; };
  };
}
