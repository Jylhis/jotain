# default.nix — non-flake entry point of the Emacs configuration.
#
# It evaluates to one derivation: the full distribution for `system`
# (packages.default = emacsConfigPackages), built from the inputs of
# flake.lock. The other outputs come from nix/outputs.nix.
#
#   nix-build                              # full distribution (emacsConfigPackages)
#   nix-build emacs.nix                    # bare Emacs (cache-parity build)
#   nix-build emacs.nix --arg noGui true   # variant build (terminal-only)
{
  system ? builtins.currentSystem,
}:
(import ./nix/off-flake.nix).packages.${system}.default
