{
  lib,
  pkgs,
}:
let
  inputs = import ../../../nix/inputs.nix { };
  emacsPkgs = pkgs.extend (
    lib.composeExtensions inputs.emacs-overlay.overlays.default (
      import ../../../etc/emacs/nix/mk-overlay.nix { }
    )
  );
  mkSite = args: import ./site.nix ({ pkgs = emacsPkgs; } // args);

  dsAssets = import ../../../etc/emacs/nix/ds-assets.nix { pkgs = emacsPkgs; };

  ds-in-sync =
    pkgs.runCommandLocal "check-ds-in-sync"
      {
        inherit dsAssets;
        vendoredDs = ./public/ds;
      }
      ''
        if ! diff -r "$dsAssets" "$vendoredDs"; then
          echo "" >&2
          echo "public/ds is out of sync with the in-tree design system" >&2
          echo "(lib/design). Re-vendor it with: just ds-sync" >&2
          exit 1
        fi
        touch $out
      '';
in
# The site documents Emacs and the configuration. The third-party API
# reference is input it ingests for the deployed site, not part of the build.
(mkSite { withApiDoc = false; }).overrideAttrs (old: {
  passthru = (old.passthru or { }) // {
    withApiDoc = mkSite { };
    tests = {
      inherit ds-in-sync;
    };
  };
})
