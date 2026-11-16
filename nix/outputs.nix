# The outputs of the Emacs distribution: overlay, modules, checks, packages and
# formatter. One function serves the monorepo (nix/inputs.nix), the flake
# (flake.nix) and the off-flake entry points (default.nix, overlay.nix).
#
#   nixpkgs, emacs-overlay, treefmt-nix  flake-shaped inputs
#   kit                                  a path shaped like platform/kit
#   design                               a path shaped like projects/libs/design
{
  nixpkgs,
  emacs-overlay,
  treefmt-nix,
  kit,
  design,
  systems ? [
    "x86_64-linux"
    "aarch64-linux"
  ],
}:
let
  root = ../.;
  overlay = import ./mk-overlay.nix { inherit design; };
  moduleOverlay = nixpkgs.lib.composeExtensions emacs-overlay.overlays.default overlay;
  pkgsFor =
    system:
    import nixpkgs {
      inherit system;
      overlays = [
        emacs-overlay.overlays.default
        overlay
      ];
      config.allowUnfree = true;
    };
  mkModule =
    path:
    { ... }:
    {
      imports = [ path ];
      _module.args.emacsConfigOverlay = moduleOverlay;
    };
  treefmtEvalFor =
    system:
    treefmt-nix.lib.evalModule (pkgsFor system) {
      projectRootFile = "justfile";
      programs = import ./treefmt.nix;
    };
  kitSource = import (kit + "/source.nix");
  checkSource = builtins.path {
    path = root;
    name = baseNameOf (toString root);
    filter = p: t: kitSource.filter p t && !(baseNameOf p == "jylhis" && dirOf p == toString root);
  };
in
{
  overlays.default = overlay;
  homeManagerModules.default = mkModule ../module.nix;
  nixosModules.default = mkModule ../module-system.nix;

  checks = nixpkgs.lib.genAttrs systems (
    system:
    let
      pkgs = pkgsFor system;
    in
    import ./checks.nix {
      inherit pkgs kit design;
      src = root;
      testEqualFiles = pkgs.callPackage (kit + "/testers/test-equal-files") { };
      treefmtCheck = (treefmtEvalFor system).config.build.check checkSource;
    }
  );

  packages = nixpkgs.lib.genAttrs systems (
    system:
    let
      pkgs = pkgsFor system;
    in
    {
      default = pkgs.emacsConfigPackages;
      emacs = pkgs.emacsConfig;
      emacs-nox = pkgs.emacsConfigPackagesNoGui;
      docs = import ./options-doc.nix {
        inherit pkgs;
        src = root;
      };
      packages-doc = import ./packages-doc.nix {
        inherit pkgs;
        src = root;
      };
      ds-assets = import ./ds-assets.nix { inherit pkgs design; };
      info = pkgs.emacsConfigInfo;
    }
  );

  formatter = nixpkgs.lib.genAttrs systems (system: (treefmtEvalFor system).config.build.wrapper);
}
