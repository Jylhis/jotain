# nix/treefmt.nix — Shared formatter programs configuration.
#
# Consumed by flake.nix (via treefmt-nix): the `nix fmt` formatter, the
# `formatting` check and the `treefmt` in the dev shell.
{
  nixfmt.enable = true;
  deadnix.enable = true;
  statix.enable = true;
}
