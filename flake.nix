{
  description = "Jotain - A NixOS-native Emacs distribution with automatic dependency management";

  nixConfig = {
    extra-substituters = [
      "https://jylhis.cachix.org"
      "https://nix-community.cachix.org"
      "https://devenv.cachix.org"
    ];
    extra-trusted-public-keys = [
      "jylhis.cachix.org-1:SIAw5iWjXRhLAmejqPy0PGuqH6bjCHIFVF9CiHmHRpE="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts.url = "github:hercules-ci/flake-parts";

    systems.url = "github:nix-systems/default-linux";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    devenv-root = {
      url = "file+file:///dev/null";
      flake = false;
    };
  };

  outputs = inputs @ { self, nixpkgs, emacs-overlay, flake-parts, systems, treefmt-nix, home-manager, devenv, devenv-root, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        treefmt-nix.flakeModule
        devenv.flakeModule
      ];

      systems = import systems;

      flake = {
        overlays.default = import ./nix/overlays { inherit inputs; };

        nixosModules = {
          default = import ./nix/modules/nixos;
        };

        homeModules = {
          default = import ./nix/modules/home;
        };
      };

      perSystem = { config, self', inputs', pkgs, system, lib, ... }: {
        _module.args.pkgs = import nixpkgs {
          inherit system;
          overlays = [
            emacs-overlay.overlays.default
            self.overlays.default
          ];
          config.allowUnfree = true;
        };

        packages = {
          default = self'.packages.jotain;
          jotain = pkgs.callPackage ./nix/package.nix { };

          emacs-dev = pkgs.callPackage ./emacs.nix {
            devMode = true;
          };

          emacs = pkgs.callPackage ./emacs.nix {
            devMode = false;
          };

          # Wrapper for `nix run` - try Jotain without installation
          emacs-run = pkgs.writeShellApplication {
            name = "emacs-run";
            text = ''
              # Create temporary isolated home
              TEMP_HOME=$(mktemp -d)
              trap 'rm -rf "$TEMP_HOME"' EXIT

              # Set up XDG isolation
              export XDG_CONFIG_HOME="$TEMP_HOME/.config"
              export XDG_DATA_HOME="$TEMP_HOME/.local/share"
              export XDG_CACHE_HOME="$TEMP_HOME/.cache"
              export XDG_STATE_HOME="$TEMP_HOME/.local/state"

              # Create emacs config directory
              mkdir -p "$XDG_CONFIG_HOME/emacs"

              # Copy configuration files
              cp -r ${self'.packages.jotain}/share/jotain/* "$XDG_CONFIG_HOME/emacs/"

              # Run Emacs with isolated config
              exec ${self'.packages.emacs}/bin/emacs --init-directory="$XDG_CONFIG_HOME/emacs" "$@"
            '';
          };
        };

        devenv.shells.default = {
          devenv.root =
            let
              devenvRootFileContent = builtins.readFile devenv-root.outPath;
            in
            pkgs.lib.mkIf (devenvRootFileContent != "") devenvRootFileContent;

          imports = [ ./devenv.nix ];
        };

        apps = {
          default = {
            type = "app";
            program = "${self'.packages.emacs-run}/bin/emacs-run";
            meta.description = "Try Jotain Emacs in an isolated temporary environment";
          };
        };

        checks =
          let
            jotainPackage = self'.packages.jotain;
          in
          {
            formatting = config.treefmt.build.check self;
            smoke-test = jotainPackage.smoke-test;
            fast-tests = jotainPackage.fast-tests;
            tests = jotainPackage.tests;
          };

        treefmt = {
          programs.nixpkgs-fmt.enable = true;
          programs.shellcheck.enable = true;
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
