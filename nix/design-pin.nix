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
# separately would let website/public/ds drift from the Emacs themes.
#
# The design system ships one `jylhis` theme with first-class light/dark
# modes, selected by `data-mode` alone (`data-theme` is retired). Generated
# outputs are named `jylhis-<light|dark>` (×2), and upstream does not commit
# them, so nix/extra-packages.nix and nix/ds-assets.nix run the generator
# in-derivation, mirroring upstream's own nix/generated.nix. jotain loads the
# jylhis-light/jylhis-dark pair; see lisp/init-ui.el.
#
# Upstream has not cut a v3.0.0 tag, so the rev is the pin.
#
# Bumping: change rev + version here, then run `just ds-sync` to re-vendor the
# website assets.  The sha256 is the NAR hash of the unpacked tarball —
# `nix-prefetch-url --unpack https://github.com/Jylhis/design/archive/<rev>.tar.gz`.
{
  owner = "Jylhis";
  repo = "design";
  rev = "e24a796d090f56aced9a3f11a8763a3ba586b157";
  sha256 = "14mr1daw9djwmwyn745cdam9539mg0dwyhcnwp0v1i6ma9snqh4h";
  version = "3.0.0-unstable-2026-09-23";
}
