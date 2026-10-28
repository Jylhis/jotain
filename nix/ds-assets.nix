# The design-system assets the jotain site serves at /ds/: the stylesheets
# and self-hosted fonts of the in-tree lib/design (lib/design/nix/web-assets.nix).
# usr.sbin/jotain-site/site.nix copies this into public/ds at build time.
{ pkgs }: import ../../../lib/design/nix/web-assets.nix { inherit pkgs; }
