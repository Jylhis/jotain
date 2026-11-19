# Design-system CSS and fonts for the site, from the jylhis/design revision
# that nix/extra-packages.nix pins for the Emacs themes.
#
# tokens.css and density.css are generator outputs; the other CSS files and
# fonts/ (referenced by fonts.css as ./fonts/) are copied verbatim.
{ pkgs }:
let
  themes =
    ((pkgs.emacsPackagesFor pkgs.jotainEmacs).overrideScope (
      import ./extra-packages.nix { inherit pkgs; }
    )).jylhis-emacs-themes;
  inherit (themes) src;
  generated =
    pkgs.runCommandLocal "jylhis-generated"
      {
        nativeBuildInputs = [ pkgs.bun ];
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
    inherit (themes) version;
  }
  ''
    mkdir -p "$out/fonts"
    for f in tokens.css density.css colors_and_type.css motion.css fonts.css; do
      case "$f" in
        tokens.css|density.css) cp "$generated/$f" "$out/$f" ;;
        *) cp "$src/$f" "$out/$f" ;;
      esac
    done
    cp "$src"/fonts/*.woff2 "$src"/fonts/*.ttf "$src"/fonts/LICENSE-*.txt "$out/fonts/"
    chmod -R u+w "$out"
  ''
