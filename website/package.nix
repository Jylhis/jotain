{
  lib,
  pkgs,
}:
let
  inputs = import ../../../nix/inputs.nix { };
  emacsPkgs = pkgs.extend (
    lib.composeExtensions inputs.emacs-overlay.overlays.default (
      import ../../../projects/distros/emacs/nix/mk-overlay.nix { }
    )
  );
  mkSite = args: import ./site.nix ({ pkgs = emacsPkgs; } // args);

  dsAssets = import ../../../projects/distros/emacs/nix/ds-assets.nix { pkgs = emacsPkgs; };

  ds-in-sync = pkgs.jylhis.testers.testEqualFiles {
    assertion = "ds-in-sync";
    actual = dsAssets;
    expected = ./public/ds;
    postFailureMessage = ''
      public/ds is out of sync with the in-tree design system
      (projects/libs/design). Re-vendor it with: just ds-sync
    '';
  };
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
