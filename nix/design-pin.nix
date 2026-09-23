# The single place github.com/Jylhis/design is pinned.
#
# Consumed by:
#   nix/extra-packages.nix  → jylhis-emacs-themes (platforms/emacs)
#   nix/ds-assets.nix       → the CSS + fonts vendored into website/public/ds
#   nix/checks.nix          → ds-in-sync (vendored copy == pinned upstream)
#   Justfile                → just ds-sync
#
# Both halves of the design system must move together: the Emacs themes and
# the website CSS are generated from the same token sources, so pinning them
# separately is how website/public/ds silently sat on v1 for months while the
# Emacs themes were bumped.
#
# CHANGELOG 2.0.0 (2026-09-01), the theming framework: a theme-independent
# core (`tokens.core.json`) plus swappable themes (`themes/<slug>.json`).
#
# CHANGELOG 3.0.0 (2026-09-22), one theme: survey and mono merge into a
# single `jylhis` theme with first-class light/dark modes, selected by
# `data-mode` alone (`data-theme` is retired). Generated outputs rename from
# `jylhis-<theme>-<light|dark>` (×4) to `jylhis-<light|dark>` (×2), and
# upstream stops committing them — nix/extra-packages.nix and
# nix/ds-assets.nix run the generator in-derivation, mirroring upstream's
# own nix/generated.nix. jotain loads the jylhis-light/jylhis-dark pair;
# see lisp/init-ui.el.
#
# The 3.0.0 history was rewritten upstream (the old v2-era revs are not
# ancestors of this one), so this bump is a re-pin, not an increment.
# Upstream has not cut a v3.0.0 tag yet, so the rev is the pin.
#
# Bumping: change rev + version here, then run `just ds-sync` to re-vendor the
# website assets.  The sha256 is the NAR hash of the unpacked tarball —
# `nix-prefetch-url --unpack https://github.com/Jylhis/design/archive/<rev>.tar.gz`.
{
  owner = "Jylhis";
  repo = "design";
  rev = "7570df33a6b89484c7f0f5037cf3857973d47c1a";
  sha256 = "0bls361q0sg8af29z1xdmjdhigl9mzhjawx3qhq6dhvgzlmy8bw9";
  version = "3.0.0-unstable-2026-09-23";
}
