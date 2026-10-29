# The design-system assets the jotain site serves at /ds/: the stylesheets
# and self-hosted fonts of the in-tree lib/design (lib/design/nix/web-assets.nix).
# The committed usr.sbin/jotain-site/public/ds mirrors it.
{ pkgs }: import ../../../lib/design/nix/web-assets.nix { inherit pkgs; }
