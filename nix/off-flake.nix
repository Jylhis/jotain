# The outputs of nix/outputs.nix for the entry points that run without the
# flake command: the inputs of flake.lock stand in for the flake inputs.
let
  s = import ./locked.nix;
  world = import ./world.nix { };
in
import ./outputs.nix (
  {
    nixpkgs = {
      outPath = s.nixpkgs;
      lib = import "${s.nixpkgs}/lib";
    };
    emacs-overlay.overlays.default = import s.emacs-overlay;
    treefmt-nix.lib = import s.treefmt-nix;
  }
  // world
)
