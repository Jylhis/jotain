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
    # Supplies the git-based variants used by emacs.nix: emacs-git
    # (master), emacs-unstable (latest release tag), emacs-igc
    # (feature/igc3). The default mainline build uses nixpkgs' emacs31
    # and does not need the overlay.
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    jylhis-emacs.url = "github:jylhis/emacs/dev";
    # Android (Termux/proot) Nix environment. Only consumed by
    # `nixOnDroidModules` / the example `nixOnDroidConfigurations`; other
    # outputs do not depend on it.
    nix-on-droid = {
      url = "github:nix-community/nix-on-droid/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      treefmt-nix,
      emacs-overlay,
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
    in
    {
      overlays.default = import ./nix/mk-overlay.nix {
        jylhisEmacsSrc = inputs."jylhis-emacs";
      };

      homeManagerModules.default =
        { ... }:
        {
          imports = [ ./module.nix ];
          _module.args.jotainOverlay = self.overlays.default;
        };
      nixosModules.default =
        { ... }:
        {
          imports = [ ./module-system.nix ];
          _module.args.jotainOverlay = self.overlays.default;
        };
      darwinModules.default = self.nixosModules.default;
      nixOnDroidModules.default =
        { ... }:
        {
          imports = [ ./module-nix-on-droid.nix ];
          _module.args.jotainOverlay = self.overlays.default;
        };

      lib = import ./nix/use-package.nix { inherit (nixpkgs) lib; };

      packages = forAllSystems (system: {
        default = (pkgsFor system).jotainEmacsPackages;
        emacs = (pkgsFor system).jotainEmacs;
        emacs-jylhis = (pkgsFor system).jylhisEmacs;
        jylhis-emacs = (pkgsFor system).jylhisEmacs;
        info = (pkgsFor system).jotainInfo;
        docs = import ./nix/options-doc.nix {
          pkgs = pkgsFor system;
          src = self;
        };
        packages-doc = import ./nix/packages-doc.nix {
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

      # Example nix-on-droid configuration wiring up the Jotain module.
      # nix-on-droid runs on aarch64-linux (Android under proot), so this
      # builds/activates only on-device or via aarch64 emulation —
      # `nix flake check` does not realise it (unknown output type), and
      # CI is x86_64-only. Copy this shape into your own nix-on-droid
      # flake and run `nix-on-droid switch --flake .#default`.
      nixOnDroidConfigurations.default = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
        pkgs = import nixpkgs {
          system = "aarch64-linux";
          overlays = [ inputs.nix-on-droid.overlays.default ];
        };
        modules = [
          self.nixOnDroidModules.default
          { services.jotain.enable = true; }
        ];
      };
    };
}
