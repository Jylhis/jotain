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
    # Emacs 31 is not yet in nixpkgs; pull it from emacs-overlay's
    # emacs-git attribute (current master). The overlay also supplies
    # emacs-unstable / emacs-igc / emacs-*-pgtk variants used by
    # emacs.nix.
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    jylhis-emacs.url = "github:jylhis/emacs/dev";
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
          # self.overlays.default already composes emacs-overlay, so it is
          # the only overlay needed here.
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
      # Compose emacs-overlay first so `emacs-git` (and the other
      # overlay-supplied variants emacs.nix builds on) exist in `final`
      # before mk-overlay.nix references them. This makes the exported
      # overlay self-contained: NixOS / nix-darwin / Home Manager
      # consumers that apply only `self.overlays.default` still get a
      # working `jotainEmacs`/`jotainEmacsPackages`.
      overlays.default = nixpkgs.lib.composeManyExtensions [
        emacs-overlay.overlays.default
        (import ./nix/mk-overlay.nix {
          jylhisEmacsSrc = inputs."jylhis-emacs";
        })
      ];

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

      lib = import ./nix/use-package.nix { inherit (nixpkgs) lib; };

      packages = forAllSystems (system: {
        default = (pkgsFor system).jotainEmacsPackages;
        emacs = (pkgsFor system).jotainEmacs;
        jylhis-emacs = (pkgsFor system).jylhisEmacs;
        jylhis-emacs-packages = (pkgsFor system).jylhisEmacsPackages;
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
    };
}
