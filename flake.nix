{
  description = "Jotain — GNU Emacs 30+ configuration with Nix build layer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/4c1018dae018162ec878d42fec712642d214fdfa";
  };

  outputs =
    { self, nixpkgs, ... }:
    {
      overlays.default = import ./overlay.nix;

      homeManagerModules.default = import ./module.nix;
    };
}
