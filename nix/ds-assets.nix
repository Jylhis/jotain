# The design-system assets vendored into website/public/ds, taken from the
# pinned jylhis/design rev (nix/design-pin.nix).
#
# website/public/ needs no build step, so these files are committed;
# `just ds-sync` copies from this derivation and the ds-in-sync check diffs
# the committed copy against it.
#
# tokens.css and density.css are generator outputs (upstream no longer
# commits them; `tokens.core.json` is the source of truth). The other CSS
# files and fonts/ (woff2/ttf plus OFL texts, referenced by fonts.css as
# ./fonts/) are copied verbatim, unmodified.
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
