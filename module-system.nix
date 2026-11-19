# module-system.nix — NixOS module for Jotain Emacs.
#
# Applies the overlay and adds Jotain Emacs to environment.systemPackages.
# The system module starts no daemon; for a per-user one use the Home Manager
# module (module.nix), which runs it through services.emacs.
#
# Usage:
#
#   imports = [ jotain.nixosModules.default ];
#   services.jotain.enable = true;
#
# services.jotain.users.<user> builds the programs of one user (the wrapped
# `emacs`, EDITOR and VISUAL, `jotctl`, the dictionaries) and installs them in
# that user's profile. The Home Manager module (module.nix) of the user then
# writes the config files and the daemon unit, and takes the programs from here
# (osConfig.services.jotain.users.<user>.built).
args@{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.jotain;
  system = config;
  jotainOverlay = args.jotainOverlay or (import ./overlay.nix);
  # An externally supplied nixpkgs.pkgs ignores nixpkgs.overlays, and the users
  # option works without services.jotain.enable, so extend pkgs when it lacks
  # the overlay.
  pkgsWithOverlay = if pkgs ? jotainEmacsPackages then pkgs else pkgs.extend jotainOverlay;
  selectedPackage = if cfg.package != null then cfg.package else pkgsWithOverlay.jotainEmacsPackages;
  sharedOptions = import ./nix/module-options.nix { inherit lib; };

  # No init directory: nothing here deploys a config, so Emacs resolves it as usual.
  systemPrograms = import ./nix/user-programs.nix {
    inherit pkgs pkgsWithOverlay cfg;
    package = selectedPackage;
    initDirectory = null;
  };

  wrappedPackage = import ./nix/wrap-runtime-deps.nix {
    inherit pkgs;
    inherit (systemPrograms) runtimeDeps;
    package = selectedPackage;
    spellDictionaries = cfg.spell.dictionaries;
  };

  userModule =
    { name, config, ... }:
    let
      home = system.users.users.${name}.home;
    in
    {
      options = {
        enable = lib.mkEnableOption "the Jotain programs of this user in their profile";

        inherit (sharedOptions)
          package
          nativeCompile
          sonarlint
          dockerfileLsp
          onePassword
          sops
          claudeCode
          spell
          ;

        initDirectory = lib.mkOption {
          type = lib.types.str;
          default = "${home}/.config/emacs";
          defaultText = lib.literalExpression ''"<home>/.config/emacs"'';
          description = "Where Home Manager deploys the config; the wrapped emacs pins --init-directory here.";
        };

        built = lib.mkOption {
          type = lib.types.raw;
          readOnly = true;
          internal = true;
          description = "The programs (nix/user-programs.nix), read by the Home Manager module.";
          default = import ./nix/user-programs.nix {
            inherit pkgs pkgsWithOverlay;
            inherit (config) initDirectory;
            cfg = config;
            package = if config.package != null then config.package else selectedPackage;
          };
        };
      };
    };

  enabledUsers = lib.filterAttrs (_: u: u.enable) cfg.users;
  inherit (systemPrograms) editorScript visualScript fontPackages;
in
{
  options.services.jotain = {
    enable = lib.mkEnableOption "the Jotain Emacs configuration";

    users = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule userModule);
      default = { };
      # Out of the generated reference: the Info manual is part of the Emacs
      # package, so documenting the options here would change its store path.
      visible = false;
      description = ''
        The Jotain programs by user, installed in the user's profile
        (`users.users.<user>.packages`). Independent of `services.jotain.enable`,
        which installs one Emacs for every user.
      '';
    };

    inherit (sharedOptions)
      package
      defaultEditor
      sonarlint
      dockerfileLsp
      onePassword
      sops
      claudeCode
      spell
      ;
  };

  config = lib.mkMerge [
    {
      users.users = lib.mapAttrs (_: u: {
        packages = [
          u.built.package
          u.built.editorScript
          u.built.visualScript
          u.built.jotctlScript
          # hiPrio so the wrapped `emacs` shadows the unwrapped binary that ships
          # inside the package.
          (lib.hiPrio u.built.emacsWrapper)
        ]
        ++ u.spell.dictionaries;
      }) enabledUsers;
    }
    (lib.mkIf cfg.enable {
      nixpkgs.overlays = [ jotainOverlay ];
      environment.systemPackages = [
        wrappedPackage
        editorScript
        visualScript
      ]
      # jinx dictionaries; the wrapper's ASPELL_CONF is what jinx reads.
      ++ cfg.spell.dictionaries;
      fonts.packages = fontPackages;
      environment.variables = lib.mkIf cfg.defaultEditor {
        EDITOR = lib.mkOverride 900 "${lib.getBin editorScript}/bin/jotain-editor";
        VISUAL = lib.mkOverride 900 "${lib.getBin visualScript}/bin/jotain-visual";
      };
    })
  ];
}
