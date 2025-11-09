{
  description = "Jotain - A NixOS-native Emacs distribution with automatic dependency management";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts.url = "github:hercules-ci/flake-parts";

    systems.url = "github:nix-systems/default";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { self, nixpkgs, emacs-overlay, flake-parts, systems, treefmt-nix, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        treefmt-nix.flakeModule
      ];

      systems = import systems;

      flake = {
        overlays.default = import ./nix/overlays { inherit inputs; };

        nixosModules = {
          default = import ./nix/modules/nixos;
        };

        homeManagerModules = {
          default = import ./nix/modules/home-manager;
        };
      };

      perSystem = { config, self', inputs', pkgs, system, lib, ... }: {
        _module.args.pkgs = import nixpkgs {
          inherit system;
          overlays = [
            emacs-overlay.overlays.default
            self.overlays.default
          ];
        };

        packages = {
          default = self'.packages.jotain;
          jotain = pkgs.callPackage ./nix/packages/jotain { };

          emacs-dev = pkgs.callPackage ./default.nix {
            devMode = true;
          };

          emacs = pkgs.callPackage ./default.nix {
            devMode = false;
          };
        };

        devShells = {
          default = pkgs.callPackage ./shell.nix {
            jotainEmacs = self'.packages.emacs-dev;
          };
        };

        checks = {
          formatting = config.treefmt.build.check self;
        };

        treefmt = {
          programs.nixpkgs-fmt.enable = true;

          programs.shfmt = {
            enable = true;
            indent_size = 2;
          };

          projectRootFile = "flake.nix";

          settings.global.excludes = [
            # Git
            ".git/**"
            # Nix build artifacts
            "result"
            "result-*"
            # Development artifacts
            ".dev-home/**"
            # Test artifacts
            "tests/.gitignore"
            # Lock files
            "flake.lock"
          ];

          # Custom formatters
          settings.formatter = {
            # Emacs Lisp formatter using Emacs built-in indentation
            elisp = {
              command = pkgs.writeShellScriptBin "format-elisp" ''
                for file in "$@"; do
                  ${pkgs.emacs}/bin/emacs --batch \
                    -l elisp-mode \
                    "$file" \
                    --eval '(indent-region (point-min) (point-max))' \
                    -f save-buffer 2>/dev/null
                done
              '';
              includes = [ "*.el" ];
            };
          };
        };

        formatter = config.treefmt.build.wrapper;
      };
    };
}
