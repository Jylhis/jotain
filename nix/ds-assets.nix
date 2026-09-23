# The design-system assets vendored into website/public/ds, taken from the
# pinned jylhis/design rev (nix/design-pin.nix).
#
# website/public/ is deliberately a no-build-step shell — a plain static
# server over website/public serves it directly — so these files are
# committed rather than assembled at build time.  This derivation is what
# makes that safe: `just
# ds-sync` copies from it, and the ds-in-sync flake check diffs the committed
# copy against it, so a stale vendored copy is a build failure instead of
# something nobody notices.
#
# Upstream 3.0.0 stops committing generated outputs, so the CSS files are
# split across two sources: tokens.css and density.css are generator outputs
# (the generator runs in-derivation; `tokens.core.json` is the source of
# truth), while colors_and_type.css, motion.css and fonts.css are committed
# files copied verbatim from the source tree.  The woff2/ttf fonts and their
# OFL texts ship in `fonts/` — fonts.css points at them with relative
# ./fonts/ URLs.  No local modifications to any of it.
{
  pkgs,
  bun,
}:
let
  pin = import ./design-pin.nix;
  src = pkgs.fetchFromGitHub {
    inherit (pin)
      owner
      repo
      rev
      sha256
      ;
  };
  generated =
    pkgs.runCommandLocal "jylhis-generated"
      {
        nativeBuildInputs = [ bun ];
      }
      ''
        cp -r ${src}/. "$TMP/src/"
        chmod -R u+w "$TMP/src"
        export HOME="$TMPDIR"
        cd "$TMP/src"
        bun scripts/generate.mjs --out "$out"
      '';
in
pkgs.runCommandLocal "jylhis-ds-assets"
  {
    inherit src generated;
  }
  ''
    mkdir -p "$out/fonts"
    for f in tokens.css density.css colors_and_type.css motion.css fonts.css; do
      case "$f" in
        tokens.css|density.css) cp "$generated/$f" "$out/$f" ;;
        *) cp "$src/$f" "$out/$f" ;;
      esac
    done
    # Self-hosted woff2/ttf slices, plus the OFL texts fonts.css points at.
    cp "$src"/fonts/*.woff2 "$src"/fonts/*.ttf "$src"/fonts/LICENSE-*.txt "$out/fonts/"
    chmod -R u+w "$out"
  ''
