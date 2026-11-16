# The design-system assets the emacs-docs site serves at /ds/: the stylesheets
# and self-hosted fonts of the design system (nix/web-assets.nix there).
# The committed projects/services/emacs-docs/public/ds mirrors it.
{
  pkgs,
  design ? (import ./world.nix { }).design,
}:
import (design + "/nix/web-assets.nix") { inherit pkgs; }
