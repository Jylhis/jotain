{
  description = "Jotain — GNU Emacs 31+ configuration with Nix build layer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # nixpkgs-unstable (26.11) dropped x86_64-darwin; 26.05 is the last
    # release supporting it. Used for that platform only (see `nixpkgsFor`).
    nixpkgs-x86_64-darwin.url = "github:NixOS/nixpkgs/nixpkgs-26.05-darwin";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # The git-based emacs.nix variants: emacs-git (master), emacs-unstable
    # (newest release or pretest tag, currently 31.1; the jotainEmacs
    # default) and emacs-igc (feature/igc3). "mainline" is nixpkgs' own
    # emacs and needs no overlay.
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Android (proot) Nix environment, used only by the example
    # `nixOnDroidConfigurations`.
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
        # on nixpkgs-26.05-darwin (see the nixpkgs-x86_64-darwin input)
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
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
      treefmtEval =
        system:
        treefmt-nix.lib.evalModule (pkgsFor system) {
          projectRootFile = "flake.nix";
          programs = import ./nix/treefmt.nix;
        };
      # Overlay for the module outputs. emacs-overlay sits underneath so
      # module installs resolve the same Emacs bases and epkgs snapshot as
      # `packages.default` (and CI's cachix artifacts). Consumers that
      # override nixpkgs (24.05+) lose those cache hits and build the
      # overlay's MELPA snapshot locally.
      moduleOverlay = nixpkgs.lib.composeExtensions emacs-overlay.overlays.default self.overlays.default;
    in
    {
      overlays.default = import ./nix/mk-overlay.nix { };

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

      # Two Emacs builds per platform: `emacs` (pgtk GUI on Linux, patched
      # NS GUI on Darwin; wrapped by `default`, the full distribution) and
      # the terminal-only `emacs-nox`. emacs.nix asserts other GUIs away.
      packages = forAllSystems (system: {
        default = (pkgsFor system).jotainEmacsPackages;
        emacs = (pkgsFor system).jotainEmacs;
        # Terminal-only distribution: what the nix-on-droid module ships
        # and what `just run-built` launches on aarch64-linux.
        emacs-nox = (pkgsFor system).jotainEmacsPackagesNoGui;
        # Already bundled into every distribution; exposed for direct builds.
        likec4-lsp = (pkgsFor system).likec4Lsp;
        info = (pkgsFor system).jotainInfo;
        docs = import ./nix/options-doc.nix {
          pkgs = pkgsFor system;
          src = self;
        };
        packages-doc = import ./nix/packages-doc.nix {
          pkgs = pkgsFor system;
          src = self;
        };
        # Docstring-level API reference for every bundled package, mounted
        # at /help/api/ by nix/site.nix. Heavy: one batch Emacs per package.
        emacs-api-doc = import ./nix/emacs-api-doc.nix {
          pkgs = pkgsFor system;
          src = self;
        };
        # No `src = self`: lib.fileset (used by site.nix and
        # info-manual.nix) rejects the string-like flake source. The
        # default src (../.) is the same tree as a path.
        site = import ./nix/site.nix {
          pkgs = pkgsFor system;
        };
        # PR CI's site build: the full site minus the heavy /help/api/
        # reference, which only the deploy path builds.
        site-preview = import ./nix/site.nix {
          pkgs = pkgsFor system;
          withApiDoc = false;
        };
        # Expected contents of website/public/ds (`just ds-sync`, ds-in-sync).
        ds-assets = import ./nix/ds-assets.nix {
          pkgs = pkgsFor system;
          inherit ((pkgsFor system)) bun;
        };
      });

      # legacyPackages holds outputs `nix flake check` must not build
      # (it builds every `packages` output but skips these); `nix build
      # .#<name>` still resolves them.
      legacyPackages = forAllSystems (
        system:
        let
          langEval = import ./nix/lang-eval.nix {
            pkgs = pkgsFor system;
          };
          emacsPackageSet = import ./nix/emacs-package-set.nix {
            pkgs = pkgsFor system;
          };
        in
        {
          # Every bundled Emacs package, e.g. `nix build
          # .#emacs-packages.magit`. The eval-only emacs-packages-eval check
          # gates that every declared name resolves.
          emacs-packages = emacsPackageSet.byName;

          # AOT-compiled config (.elc + store .eln) for `just run-built-fast`
          # only; ~50-150 MB of .eln. Built against `.core`, like the
          # daemon's compiledConfig and the elisp-compile check (see
          # nix/config-compiled.nix).
          config-compiled = import ./nix/config-compiled.nix {
            pkgs = pkgsFor system;
            emacs = (pkgsFor system).jotainEmacsPackages.core;
            nativeCompile = true;
          };

          # Per-language IDE-feature evaluation (nix/lang-eval.nix). The
          # matrix loads the full config and the live probe bundles language
          # servers; the cheap gate is checks.lang-eval-doc-in-sync.
          #   nix build .#lang-eval-doc      (registry -> language-support.mdx)
          #   nix build .#lang-eval-matrix   (live config-introspection matrix)
          #   nix build .#lang-eval-live     (end-to-end LSP probe subset)
          inherit (langEval) lang-eval-doc lang-eval-matrix lang-eval-live;
        }
      );

      formatter = forAllSystems (system: (treefmtEval system).config.build.wrapper);

      checks = forAllSystems (
        system:
        import ./nix/checks.nix {
          pkgs = pkgsFor system;
          src = self;
          treefmtCheck = (treefmtEval system).config.build.check self;
        }
      );

      # Example nix-on-droid config (aarch64-linux). `nix flake check`
      # does not build it (unknown output type). Copy this shape into your
      # own flake and run `nix-on-droid switch --flake .#default`.
      nixOnDroidConfigurations.default = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
        pkgs = import nixpkgs {
          system = "aarch64-linux";
          overlays = [ inputs.nix-on-droid.overlays.default ];
        };
        modules = [
          self.nixOnDroidModules.default
          {
            services.jotain.enable = true;
            # No default upstream; required for a switchable config.
            system.stateVersion = "24.05";
          }
        ];
      };
    };
}
