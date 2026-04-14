{
  description = "Jotain — GNU Emacs 30+ configuration with Nix build layer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/4c1018dae018162ec878d42fec712642d214fdfa";

  };

  outputs =
    { self, nixpkgs, ... }:
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
    in
    {
      overlays.default = import ./overlay.nix;

      homeManagerModules.default = import ./module.nix;

      packages = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
        in
        {
          default = pkgs.jotainEmacsPackages;
          emacs = pkgs.jotainEmacs;
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
        in
        {
          # Package builds
          packages-default = self.packages.${system}.default;
          packages-emacs = self.packages.${system}.emacs;

          # Nix formatting
          formatting =
            pkgs.runCommandLocal "check-nixfmt"
              {
                nativeBuildInputs = [
                  pkgs.nixfmt
                  pkgs.findutils
                ];
                src = self;
              }
              ''
                find $src -name '*.nix' -not -path '*/npins/*' -exec nixfmt --check {} +
                touch $out
              '';

          # Static analysis
          statix =
            pkgs.runCommandLocal "check-statix"
              {
                nativeBuildInputs = [ pkgs.statix ];
                src = self;
              }
              ''
                cd $src
                statix check .
                touch $out
              '';

          # Dead code detection
          deadnix =
            pkgs.runCommandLocal "check-deadnix"
              {
                nativeBuildInputs = [ pkgs.deadnix ];
                src = self;
              }
              ''
                cd $src
                deadnix --fail --exclude npins .
                touch $out
              '';
        }
      );
    };
}
