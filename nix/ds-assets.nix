# The design-system assets vendored into website/public/ds, taken from the
# pinned jylhis/design rev (nix/design-pin.nix).
#
# website/public/ is deliberately a no-build-step shell — `npx wrangler dev`
# in website/ serves it directly — so these files are committed rather than
# assembled at build time.  This derivation is what makes that safe: `just
# ds-sync` copies from it, and the ds-in-sync flake check diffs the committed
# copy against it, so a stale vendored copy is a build failure instead of
# something nobody notices.
#
# The CSS files are generated upstream from tokens.json and carry "Do not edit
# by hand" headers — they are copied verbatim, with no local modifications.
{ pkgs }:
let
  pin = import ./design-pin.nix;
  src = pkgs.fetchFromGitHub { inherit (pin) owner repo rev sha256; };
in
pkgs.runCommandLocal "jylhis-ds-assets"
  {
    inherit src;
  }
  ''
    mkdir -p "$out/fonts"
    for f in tokens.css colors_and_type.css motion.css fonts.css; do
      cp "$src/$f" "$out/$f"
    done
    # Self-hosted woff2 slices, plus the OFL texts fonts.css points at.
    cp "$src"/fonts/*.woff2 "$src"/fonts/LICENSE-*.txt "$out/fonts/"
    chmod -R u+w "$out"
  ''
