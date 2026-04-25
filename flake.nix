{
  description = "Jotain — GNU Emacs 30+ configuration with Nix build layer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/4c1018dae018162ec878d42fec712642d214fdfa";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      treefmt-nix,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      pkgsFor =
        system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        };
      treefmtEval =
        system:
        treefmt-nix.lib.evalModule (pkgsFor system) {
          projectRootFile = "flake.nix";
          programs = import ./nix/treefmt.nix;
        };
    in
    {
      overlays.default = import ./overlay.nix;

      homeManagerModules.default = import ./module.nix;
      nixosModules.default = import ./module-system.nix;
      darwinModules.default = import ./module-system.nix;

      lib = import ./nix/use-package.nix { inherit (nixpkgs) lib; };

      packages = forAllSystems (system: {
        default = (pkgsFor system).jotainEmacsPackages;
        emacs = (pkgsFor system).jotainEmacs;
        info = (pkgsFor system).jotainInfo;
        docs = import ./nix/options-doc.nix {
          pkgs = pkgsFor system;
          src = self;
        };
      });

      formatter = forAllSystems (system: (treefmtEval system).config.build.wrapper);

      checks = forAllSystems (
        system:
        import ./nix/checks.nix {
          pkgs = pkgsFor system;
          src = self;
          treefmtCheck = (treefmtEval system).config.build.check self;
        }
      );
    };
}
