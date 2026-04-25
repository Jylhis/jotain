# nix/treefmt.nix — Shared formatter programs configuration.
#
# Consumed by both flake.nix (via treefmt-nix) and devenv.nix (via
# devenv's treefmt module) so formatting rules live in one place.
{
  nixfmt.enable = true;
  deadnix.enable = true;
  statix.enable = true;
}
