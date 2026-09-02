# The single place github.com/Jylhis/design is pinned.
#
# Consumed by:
#   nix/extra-packages.nix  → jylhis-emacs-themes (platforms/emacs)
#   nix/ds-assets.nix       → the CSS + woff2 vendored into website/public/ds
#   nix/checks.nix          → ds-in-sync (vendored copy == pinned upstream)
#   Justfile                → just ds-sync
#
# Both halves of the design system must move together: the Emacs themes and
# the website CSS are generated from the same token sources, so pinning them
# separately is how website/public/ds silently sat on v1 for months while the
# Emacs themes were bumped.
#
# CHANGELOG 2.0.0 (2026-09-01), the theming framework: a theme-independent
# core (`tokens.core.json`) plus swappable themes (`themes/<slug>.json`), each
# with a first-class light and dark mode selected by `data-theme` × `data-mode`.
# `tokens.json` is retired.  The Emacs themes move from `jylhis-{sheet,field}`
# to `jylhis-{survey,mono}-{light,dark}` — jotain loads the survey pair
# (survey/light is "Sheet", survey/dark is "Field"); see lisp/init-ui.el.
# Upstream has not cut a v2.0.0 tag yet, so the rev is the pin.
#
# Bumping: change rev + version here, then run `just ds-sync` to re-vendor the
# website assets.  The sha256 is the NAR hash of the unpacked tarball —
# `nix-prefetch-url --unpack https://github.com/Jylhis/design/archive/<rev>.tar.gz`.
{
  owner = "Jylhis";
  repo = "design";
  rev = "ec1bf783b229810e04d53830a810a6da95a3ec44";
  sha256 = "1ann8fzvfxfy2xl748czx4m087qhikzzch3r68dz23cra1bc36cl";
  version = "2.0.0-unstable-2026-09-01";
}
