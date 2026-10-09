# The single place github.com/Jylhis/design is pinned.
#
# Consumed by:
#   nix/extra-packages.nix  → jylhis-emacs-themes (platforms/emacs)
#   nix/ds-assets.nix       → the CSS + fonts vendored into website/public/ds
#   nix/checks.nix          → ds-in-sync (vendored copy == pinned upstream)
#   Justfile                → just ds-sync
#
# The Emacs themes and the website CSS are generated from the same token
# sources, so one pin keeps them in step.
#
# Upstream does not commit the generated `jylhis-<light|dark>` outputs, so
# nix/extra-packages.nix and nix/ds-assets.nix run the generator
# in-derivation, mirroring upstream's nix/generated.nix. jotain loads the
# jylhis-light/jylhis-dark pair (lisp/init-ui.el).
#
# There is no v3.0.0 tag yet, so the rev is the pin.
#
# Bump with `just update-pins design`, or by hand: change rev + version,
# then run `just ds-sync`. sha256 is the NAR hash of the unpacked tarball:
# `nix-prefetch-url --unpack https://github.com/Jylhis/design/archive/<rev>.tar.gz`.
{
  owner = "Jylhis";
  repo = "design";
  rev = "e24a796d090f56aced9a3f11a8763a3ba586b157";
  sha256 = "14mr1daw9djwmwyn745cdam9539mg0dwyhcnwp0v1i6ma9snqh4h";
  version = "3.0.0-unstable-2026-09-23";
}
