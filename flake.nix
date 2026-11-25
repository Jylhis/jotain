{
  description = "Jotain — GNU Emacs 31+ configuration with Nix build layer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      treefmt-nix,
      emacs-overlay,
      ...
    }:
    let
      systems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      pkgsFor =
        system:
        import nixpkgs {
          inherit system;
          overlays = [
            emacs-overlay.overlays.default
            self.overlays.default
          ];
        };
      treefmtEval =
        system:
        treefmt-nix.lib.evalModule (pkgsFor system) {
          projectRootFile = "flake.nix";
          programs = import ./nix/treefmt.nix;
        };
      # emacs-overlay sits underneath so module installs resolve the same
      # Emacs and epkgs snapshot as `packages.default`.
      moduleOverlay = nixpkgs.lib.composeExtensions emacs-overlay.overlays.default self.overlays.default;
    in
    {
      overlays.default = import ./nix/mk-overlay.nix;

      homeManagerModules.default =
        { ... }:
        {
          imports = [ ./module.nix ];
          _module.args.jotainOverlay = moduleOverlay;
        };
      nixosModules.default =
        { ... }:
        {
          imports = [ ./module-system.nix ];
          _module.args.jotainOverlay = moduleOverlay;
        };

      packages = forAllSystems (system: {
        default = (pkgsFor system).jotainEmacsPackages;
        emacs = (pkgsFor system).jotainEmacs;
        inherit ((pkgsFor system)) eca;
        likec4-lsp = (pkgsFor system).likec4Lsp;
        info = (pkgsFor system).jotainInfo;
        # No `src = self`: lib.fileset rejects the string-like flake source.
        site = import ./nix/site.nix {
          pkgs = pkgsFor system;
        };
      });

      # Outputs `nix flake check` does not build.
      legacyPackages = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
          langEval = import ./nix/lang-eval.nix { inherit pkgs; };
        in
        {
          # The scope the distribution is built from, for `nix-update --flake`
          # and `nix build .#emacs-packages.<name>`.
          emacs-packages = (pkgs.emacsPackagesFor pkgs.jotainEmacs).overrideScope (
            import ./nix/extra-packages.nix { inherit pkgs; }
          );

          # For `just run-built-fast`. Built against `.core`, like the
          # elisp-compile check.
          config-compiled = import ./nix/config-compiled.nix {
            inherit pkgs;
            emacs = pkgs.jotainEmacsPackages.core;
            nativeCompile = true;
          };

          inherit (langEval) lang-eval-doc lang-eval-matrix lang-eval-live;
        }
      );

      devShells = forAllSystems (system: {
        default = import ./nix/dev-shell.nix {
          pkgs = pkgsFor system;
          treefmt = (treefmtEval system).config.build.wrapper;
        };
      });

      formatter = forAllSystems (system: (treefmtEval system).config.build.wrapper);

      checks = forAllSystems (
        system:
        import ./nix/checks.nix {
          pkgs = pkgsFor system;
          src = self;
          treefmtCheck = (treefmtEval system).config.build.check self;
          devShell = self.devShells.${system}.default;
        }
      );
    };
}
