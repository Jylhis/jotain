# default.nix — non-flake entry point of the Emacs configuration.
#
# It evaluates to one derivation: the full distribution for `system`
# (packages.default = emacsConfigPackages). The other outputs come from
# the root entry point.
#
#   nix-build                              # full distribution (emacsConfigPackages)
#   nix-build emacs.nix                    # bare Emacs (cache-parity build)
#   nix-build emacs.nix --arg noGui true   # variant build (terminal-only)
{
  system ? builtins.currentSystem,
}:
let
  inputs = import ../../../nix/inputs.nix { };
in
inputs.emacs-config.packages.${system}.default
