{
  description = "Jotain - A NixOS-native Emacs distribution with automatic dependency management";

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
  };

  outputs = inputs @ { self, nixpkgs, emacs-overlay, flake-parts, systems, treefmt-nix, home-manager, ... }:
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
          jotain = pkgs.callPackage ./default.nix { };

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

        devShells = {
          default = pkgs.callPackage ./shell.nix {
            jotainEmacs = self'.packages.emacs-dev;
          };
        };

        apps = {
          default = {
            type = "app";
            program = "${self'.packages.emacs-run}/bin/emacs-run";
          };
        };

        checks =
          let
            # Import NMT tests
            nmtTests = pkgs.callPackage ./nmt-tests {
              inherit home-manager;
              homeModule = self.homeModules.default;
            };

            # Import runtime test
            runtimeTests = pkgs.callPackage ./nmt-tests/runtime.nix {
              inherit home-manager;
              homeModule = self.homeModules.default;
              emacsPackage = self'.packages.emacs;
            };

            # Get passthru tests from the jotain package
            jotainPackage = self'.packages.jotain;

            # Base checks (always run)
            baseChecks = {
              formatting = config.treefmt.build.check self;

              # ERT tests from passthru
              smoke-test = jotainPackage.smoke-test;
              fast-tests = jotainPackage.fast-tests;
              tests = jotainPackage.tests;

              # NMT home-manager module tests
              inherit (nmtTests)
                test-module-enabled
                test-module-disabled
                test-runtime-deps-enabled
                test-runtime-deps-disabled
                test-daemon-disabled;
            };

            # Optional runtime test (only in CI)
            runtimeCheck = lib.optionalAttrs (builtins.getEnv "CI" != "") {
              inherit (runtimeTests) test-emacs-runtime;
            };
          in
          baseChecks // runtimeCheck;

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
