{
  description = "Jotain — GNU Emacs 31+ configuration with Nix build layer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # nixpkgs-unstable (26.11) dropped x86_64-darwin; 26.05 is its last
    # supported release. Pinned for that one platform only — every other
    # system uses the unstable channel above. See `nixpkgsFor`.
    nixpkgs-x86_64-darwin.url = "github:NixOS/nixpkgs/nixpkgs-26.05-darwin";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Supplies the git-based variants used by emacs.nix: emacs-git
    # (master), emacs-unstable (Emacs 31 release branch — the default
    # base for jotainEmacs, see nix/mk-overlay.nix), emacs-igc
    # (feature/igc3). The "mainline" variant uses nixpkgs' default
    # emacs attribute and does not need the overlay.
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Only the source tree is consumed (mk-overlay.nix passes it to
    # emacs-jylhis.nix as `src`); `flake = false` keeps its transitive
    # inputs (nixpkgs, emacs-overlay, …) out of flake.lock.
    jylhis-emacs = {
      url = "github:jylhis/emacs/dev";
      flake = false;
    };
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
        # x86_64-darwin is pinned to nixpkgs-26.05-darwin (see the
        # nixpkgs-x86_64-darwin input); unstable dropped the platform.
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      # x86_64-darwin uses the 26.05 pin (unstable dropped it); every other
      # system uses nixpkgs-unstable.
      nixpkgsFor = system: if system == "x86_64-darwin" then inputs."nixpkgs-x86_64-darwin" else nixpkgs;
      pkgsFor =
        system:
        import (nixpkgsFor system) {
          inherit system;
          overlays = [
            emacs-overlay.overlays.default
            self.overlays.default
          ];
        };
      # Same as pkgsFor but with the overlay configured to bundle only the
      # grammars this config uses. Kept out of the default overlay so
      # packages.default / the system modules stay full-grammar cache hits.
      pkgsForLite =
        system:
        import (nixpkgsFor system) {
          inherit system;
          overlays = [
            emacs-overlay.overlays.default
            (import ./nix/mk-overlay.nix {
              jylhisEmacsSrc = inputs."jylhis-emacs";
              curatedGrammars = true;
            })
          ];
        };
      treefmtEval =
        system:
        treefmt-nix.lib.evalModule (pkgsFor system) {
          projectRootFile = "flake.nix";
          programs = import ./nix/treefmt.nix;
        };
      # Overlay handed to the HM / NixOS / nix-darwin / nix-on-droid module
      # outputs. emacs-overlay is composed underneath the jotain overlay so
      # module installs resolve the emacs-git/unstable/igc bases and their
      # epkgs from the same snapshot `packages.default` (and CI) build —
      # otherwise module-built distributions would silently diverge from
      # the flake-built, cachix-cached one. Consumer trade-off: consumers
      # following jotain's nixpkgs pin get jylhis-cachix hits; consumers
      # overriding nixpkgs with their own (24.05+) lose Hydra-cached epkgs
      # and build the overlay's MELPA snapshot locally instead.
      moduleOverlay = nixpkgs.lib.composeExtensions emacs-overlay.overlays.default self.overlays.default;
    in
    {
      overlays.default = import ./nix/mk-overlay.nix {
        jylhisEmacsSrc = inputs."jylhis-emacs";
      };

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
      darwinModules.default = self.nixosModules.default;
      nixOnDroidModules.default =
        { ... }:
        {
          imports = [ ./module-nix-on-droid.nix ];
          _module.args.jotainOverlay = moduleOverlay;
        };

      lib = import ./nix/use-package.nix { inherit (nixpkgs) lib; };

      packages = forAllSystems (system: {
        default = (pkgsFor system).jotainEmacsPackages;
        # Full distribution with a curated grammar subset (~26 vs ~275):
        # smaller closure, far less to build from source. Opt-in.
        emacs-lite = (pkgsForLite system).jotainEmacsPackages;
        emacs = (pkgsFor system).jotainEmacs;
        # Bare Emacs on nixpkgs' default attr (Emacs 30, Hydra-cached) —
        # the pre-switch default, kept as an escape hatch now that
        # jotainEmacs defaults to the unstable (Emacs 31 pretest) variant.
        emacs-mainline = import ./emacs.nix {
          pkgs = pkgsFor system;
          variant = "mainline";
        };
        # Full terminal-only distribution (noGui Emacs + packages +
        # grammars) — same attribute the nix-on-droid module ships.
        # `just run-built` launches this on aarch64-linux.
        emacs-nox = (pkgsFor system).jotainEmacsPackagesNoGui;
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
        site = import ./nix/site.nix {
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
          {
            services.jotain.enable = true;
            # nix-on-droid's system.stateVersion has no default; set it so
            # this example evaluates as a complete, switchable config.
            system.stateVersion = "24.05";
          }
        ];
      };
    };
}
