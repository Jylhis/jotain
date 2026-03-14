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

    devenv-root = {
      url = "file+file:///dev/null";
      flake = false;
    };

    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix2container = {
      url = "github:nlewo/nix2container";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mk-shell-bin.url = "github:rrbutani/nix-mk-shell-bin";
  };

  outputs = inputs @ { self, nixpkgs, emacs-overlay, flake-parts, systems, treefmt-nix, home-manager, devenv, ... }:
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

      perSystem = { config, self', inputs', pkgs, system, lib, ... }:
        let
          jotainEmacs = self'.packages.emacs-dev;

          devJot = pkgs.writeShellScriptBin "jot" ''
            #!/usr/bin/env bash

            # Dynamically find project root (look for flake.nix)
            PROJECT_ROOT="$PWD"
            while [ ! -f "$PROJECT_ROOT/flake.nix" ] && [ "$PROJECT_ROOT" != "/" ]; do
              PROJECT_ROOT="$(dirname "$PROJECT_ROOT")"
            done

            if [ ! -f "$PROJECT_ROOT/flake.nix" ]; then
              echo "Error: Could not find project root (no flake.nix found)"
              exit 1
            fi

            # Use local sources
            export JOTAIN_DEV_MODE=1
            export JOTAIN_ROOT="$PROJECT_ROOT"
            export JOTAIN_ELISP_DIR="$PROJECT_ROOT/elisp"
            export JOTAIN_CLI_DIR="$PROJECT_ROOT/cli"

            # Prepend local elisp so local files shadow the Nix-built jotain-modules
            EMACSLOADPATH="$PROJECT_ROOT/elisp:''${EMACSLOADPATH:-}"
            export EMACSLOADPATH

            # Isolated user directory
            export JOTAIN_DEV_HOME="$PROJECT_ROOT/.dev-home"
            mkdir -p "$JOTAIN_DEV_HOME"

            # Override XDG paths to isolate from system
            export XDG_CONFIG_HOME="$JOTAIN_DEV_HOME/.config"
            export XDG_DATA_HOME="$JOTAIN_DEV_HOME/.local/share"
            export XDG_CACHE_HOME="$JOTAIN_DEV_HOME/.cache"
            export XDG_STATE_HOME="$JOTAIN_DEV_HOME/.local/state"

            mkdir -p "$XDG_CONFIG_HOME/emacs"
            mkdir -p "$XDG_DATA_HOME/emacs"
            mkdir -p "$XDG_CACHE_HOME/emacs"
            mkdir -p "$XDG_STATE_HOME/emacs"

            # Use development Emacs
            export PATH="${jotainEmacs}/bin:$PATH"

            # Run local CLI script if it exists, otherwise show help
            if [ -f "$PROJECT_ROOT/cli/jot" ]; then
              exec bash "$PROJECT_ROOT/cli/jot" "$@"
            else
              echo "CLI not yet created. Use 'emacs' to launch development Emacs."
              exit 1
            fi
          '';

          devEmacsWrapper = pkgs.writeShellScriptBin "emacs-dev" ''
            #!/usr/bin/env bash

            # Dynamically find project root (look for flake.nix)
            PROJECT_ROOT="$PWD"
            while [ ! -f "$PROJECT_ROOT/flake.nix" ] && [ "$PROJECT_ROOT" != "/" ]; do
              PROJECT_ROOT="$(dirname "$PROJECT_ROOT")"
            done

            if [ ! -f "$PROJECT_ROOT/flake.nix" ]; then
              echo "Error: Could not find project root (no flake.nix found)"
              exit 1
            fi

            # Use local sources
            export JOTAIN_DEV_MODE=1
            export JOTAIN_ROOT="$PROJECT_ROOT"
            export JOTAIN_ELISP_DIR="$PROJECT_ROOT/elisp"

            # Prepend local elisp so local files shadow the Nix-built jotain-modules
            EMACSLOADPATH="$PROJECT_ROOT/elisp:''${EMACSLOADPATH:-}"
            export EMACSLOADPATH

            # Isolated user directory
            export JOTAIN_DEV_HOME="$PROJECT_ROOT/.dev-home"
            export XDG_CONFIG_HOME="$JOTAIN_DEV_HOME/.config"
            export XDG_DATA_HOME="$JOTAIN_DEV_HOME/.local/share"
            export XDG_CACHE_HOME="$JOTAIN_DEV_HOME/.cache"
            export XDG_STATE_HOME="$JOTAIN_DEV_HOME/.local/state"

            mkdir -p "$XDG_CONFIG_HOME/emacs"
            mkdir -p "$XDG_DATA_HOME/emacs"
            mkdir -p "$XDG_CACHE_HOME/emacs"
            mkdir -p "$XDG_STATE_HOME/emacs"

            # Copy template files (don't symlink, as we'll modify init.el)
            if [ -f "$PROJECT_ROOT/early-init.el" ]; then
              ln -sfn "$PROJECT_ROOT/early-init.el" "$XDG_CONFIG_HOME/emacs/early-init.el"
            fi

            if [ -f "$PROJECT_ROOT/init.el" ]; then
              ln -sfn "$PROJECT_ROOT/init.el" "$XDG_CONFIG_HOME/emacs/init.el"
            fi

            # Create symlink for elisp directory so template's user-emacs-directory paths work
            ln -sfn "$PROJECT_ROOT/elisp" "$XDG_CONFIG_HOME/emacs/elisp"
            ln -sfn "$PROJECT_ROOT/config" "$XDG_CONFIG_HOME/emacs/config"

            # Run Emacs with explicit init directory to ensure XDG location is used
            exec ${jotainEmacs}/bin/emacs --init-directory="$XDG_CONFIG_HOME/emacs" "$@"
          '';

          mcpLanguageServerNix = pkgs.writeShellScriptBin "mcp-language-server-nix" ''
            exec ${pkgs.mcp-language-server}/bin/mcp-language-server \
              --workspace "$PWD" \
              --lsp "${pkgs.nil}/bin/nil"
          '';
        in
        {
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

          devenv.shells.default = {
            devenv.root =
              let
                devenvRootFileContent = builtins.readFile inputs.devenv-root.outPath;
              in
              if devenvRootFileContent != "" then devenvRootFileContent
              else toString ./.; # fallback for nix develop --no-pure-eval

            packages = [
              jotainEmacs
              devJot
              devEmacsWrapper
              mcpLanguageServerNix
              pkgs.nil
              pkgs.bash-language-server
              pkgs.mcp-nixos
              pkgs.mcp-language-server
              pkgs.nixpkgs-fmt
              pkgs.shfmt
              pkgs.shellcheck
              pkgs.ripgrep
              pkgs.fd
              pkgs.git
              pkgs.nix-prefetch-git
              pkgs.just
            ];

            env = {
              JOTAIN_DEV_MODE = "1";
            };

            enterShell = ''
              echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
              echo "  Jotain Development Environment"
              echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
              echo ""
              echo "Development mode enabled - using local sources"
              echo ""
              echo "Commands:"
              echo "  emacs-dev        - Run Emacs (isolated, local sources)"
              echo "  jot              - Run Jotain CLI (local sources)"
              echo "  just test        - Run tests"
              echo "  just build       - Build package"
              echo "  just check       - Run flake checks"
              echo ""
              echo "Isolation:"
              echo "  User dir: .dev-home/ (isolated from system)"
              echo "  Sources:  $PWD/elisp"
              echo ""
              echo "Changes to elisp/ and cli/ take effect immediately!"
              echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

              export JOTAIN_ROOT="$PWD"
              export JOTAIN_ELISP_DIR="$PWD/elisp"
              export JOTAIN_CLI_DIR="$PWD/cli"

              # Prepend local CLI to PATH for direct access
              export PATH="$PWD/cli:$PATH"

              # Add .dev-home to .gitignore if not already there
              if [ -f .gitignore ] && ! grep -q "^\.dev-home" .gitignore 2>/dev/null; then
                echo ".dev-home/" >> .gitignore
              fi

              # Create .dev-home directory
              mkdir -p .dev-home
            '';

            git-hooks.hooks = {
              shellcheck.enable = true;
              treefmt = {
                enable = true;
                settings.treefmt = config.treefmt.build.wrapper;
              };
            };
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
              ".devenv/**"
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
