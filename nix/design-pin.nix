# The single place github.com/Jylhis/design is pinned.
#
# Consumed by:
#   nix/extra-packages.nix  → jylhis-emacs-themes (platforms/emacs)
#   nix/ds-assets.nix       → the CSS + woff2 vendored into website/public/ds
#   nix/checks.nix          → ds-in-sync (vendored copy == pinned upstream)
#   Justfile                → just ds-sync
#
# Both halves of the design system must move together: the Emacs themes and
# the website CSS are generated from the same tokens.json, so pinning them
# separately is how website/public/ds silently sat on v1 for months while the
# Emacs themes were bumped.
#
# v2 "The Survey" — CHANGELOG 1.0.0 (2026-07-25), a breaking retheme: Paper/
# Roast become Sheet/Field, copper becomes bronze, and the Literata +
# JetBrains Mono pairing gives way to Zilla Slab + Hanken Grotesk + IBM Plex
# Mono.  Upstream has not cut a v1.0.0 tag yet (newest is v0.4.0), so the rev
# is the pin.
#
# Bumping: change rev + version here, then run `just ds-sync` to re-vendor the
# website assets.  The sha256 is the NAR hash of the unpacked tarball —
# `nix-prefetch-url --unpack https://github.com/Jylhis/design/archive/<rev>.tar.gz`.
{
  owner = "Jylhis";
  repo = "design";
  rev = "452789cf0c6328280f4e33cb2d12929a23c69d36";
  sha256 = "0xg5jyb7xvvlzp62czp9c3d8fird6vgi690kzjsnhb1klf1m20sr";
  version = "1.0.0-unstable-2026-07-26";
}
