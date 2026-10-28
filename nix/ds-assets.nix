# The design-system assets the jotain site serves at /ds/: the stylesheets
# and self-hosted fonts of the in-tree lib/design, taken from its npm package
# derivation (tracked sources + generated tokens.css/density.css).
# usr.sbin/jotain-site/site.nix copies this into public/ds at build time.
# Never import lib/design/default.nix here: it cycles through nix/inputs.nix.
{ pkgs }:
let
  design = pkgs.callPackage ../../../lib/design/nix/npm-package.nix { };
in
pkgs.runCommandLocal "jylhis-ds-assets" { inherit design; } ''
  mkdir -p "$out/fonts"
  for f in tokens.css density.css colors_and_type.css motion.css fonts.css; do
    cp "$design/$f" "$out/$f"
  done
  # The self-hosted woff2 slices, plus the OFL texts fonts.css points at.
  cp "$design"/fonts/*.woff2 "$design"/fonts/LICENSE-*.txt "$out/fonts/"
  chmod -R u+w "$out"
''
