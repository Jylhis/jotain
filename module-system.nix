# module-system.nix — NixOS module for Jotain Emacs.
#
# Applies the overlay and adds Jotain Emacs to environment.systemPackages.
# For a per-user daemon, use the Home Manager module (module.nix).
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
  # A host whose pkgs already carry the overlay skips a second nixpkgs
  # fixpoint.
  pkgsWithOverlay = if pkgs ? jotainEmacsPackages then pkgs else pkgs.extend jotainOverlay;
  selectedPackage = if cfg.package != null then cfg.package else pkgsWithOverlay.jotainEmacsPackages;

  # Shared runtime binaries plus the opt-in tools (mirrors module.nix).
  runtimeDeps =
    import ./nix/runtime-deps.nix { inherit pkgs pkgsWithOverlay; }
    ++ lib.optional cfg.sonarlint.enable pkgs.sonarlint-ls
    ++ lib.optional cfg.dockerfileLsp.enable pkgs.dockerfile-language-server
    ++ lib.optional cfg.onePassword.enable pkgs._1password-cli
    ++ lib.optional cfg.sops.enable pkgs.sops
    ++ lib.optional cfg.claudeCode.enable pkgs.claude-code;

  wrappedPackage = import ./nix/wrap-runtime-deps.nix {
    inherit pkgs runtimeDeps;
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

        package = lib.mkOption {
          type = lib.types.nullOr lib.types.package;
          default = null;
          defaultText = lib.literalExpression "config.services.jotain.package or the full distribution";
          description = "Custom Jotain Emacs package for this user.";
        };

        nativeCompile.enable = lib.mkEnableOption "AOT native compilation of the config (see module.nix)";

        initDirectory = lib.mkOption {
          type = lib.types.str;
          default = "${home}/.config/emacs";
          defaultText = lib.literalExpression ''"<home>/.config/emacs"'';
          description = "Where Home Manager deploys the config; the wrapped emacs pins --init-directory here.";
        };

        sonarlint.enable = lib.mkEnableOption "the SonarLint language server on the wrapper PATH";
        dockerfileLsp.enable = lib.mkEnableOption "the Dockerfile language server on the wrapper PATH";
        onePassword.enable = lib.mkEnableOption "the 1Password CLI on the wrapper PATH";
        sops.enable = lib.mkEnableOption "the sops CLI on the wrapper PATH";
        claudeCode.enable = lib.mkEnableOption "the Claude Code CLI on the wrapper PATH";

        spell.dictionaries = lib.mkOption {
          type = with lib.types; listOf package;
          default = [ ];
          description = ''
            Aspell dictionaries for jinx. A non-empty list replaces the bundled
            set in this user's wrapped emacs; the empty default keeps it.
          '';
        };

        built = lib.mkOption {
          type = lib.types.raw;
          readOnly = true;
          internal = true;
          description = "The programs (nix/user-programs.nix), read by the Home Manager module.";
          default = import ./nix/user-programs.nix {
            inherit pkgs pkgsWithOverlay;
            inherit (config) initDirectory;
            package = if config.package != null then config.package else selectedPackage;
            nativeCompile = config.nativeCompile.enable;
            spellDictionaries = config.spell.dictionaries;
            tools = {
              sonarlint = config.sonarlint.enable;
              dockerfileLsp = config.dockerfileLsp.enable;
              onePassword = config.onePassword.enable;
              sops = config.sops.enable;
              claudeCode = config.claudeCode.enable;
            };
          };
        };
      };
    };

  enabledUsers = lib.filterAttrs (_: u: u.enable) cfg.users;

  # EDITOR: terminal emacsclient.
  editorScript = import ./nix/editor-script.nix {
    inherit pkgs;
    package = wrappedPackage;
  };

  # VISUAL: GUI emacsclient frame.
  visualScript = pkgs.writeShellScriptBin "jotain-visual" ''
    exec ${lib.getBin wrappedPackage}/bin/emacsclient \
      --create-frame \
      --alternate-editor=${lib.getBin wrappedPackage}/bin/emacs \
      -- \
      "$@"
  '';
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

    package = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      defaultText = lib.literalExpression "null";
      description = ''
        Custom Jotain Emacs package to use. Leave unset for the full
        distribution (`jotainEmacsPackages`).
      '';
    };

    defaultEditor = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = false;
      description = ''
        Whether to configure {command}`emacsclient` as the default
        editor using the {env}`EDITOR` and {env}`VISUAL`
        environment variables.
      '';
    };

    sonarlint = {
      enable = lib.mkEnableOption "SonarLint language server ({command}`M-x jotain-sonarlint`)";
    };

    dockerfileLsp = {
      enable = lib.mkEnableOption "Dockerfile language server ({command}`docker-langserver`), auto-attached by Eglot in {command}`dockerfile-mode`";
    };

    spell = {
      dictionaries = lib.mkOption {
        type = with lib.types; listOf package;
        default = [ ];
        example = lib.literalExpression "[ pkgs.aspellDicts.en pkgs.aspellDicts.fi ]";
        description = ''
          Aspell dictionary packages for jinx spell-checking
          (lisp/init-writing.el). A non-empty list replaces the bundled
          en/fi/de/fr set: the wrapped emacs gets an ASPELL_CONF that points
          at an {command}`aspellWithDicts` built from these packages. The
          packages also go into the system profile. The empty default keeps
          the bundled dictionaries.
        '';
      };
    };

    onePassword = {
      enable = lib.mkEnableOption ''
        the 1Password CLI ({command}`op`) on the wrapper PATH, the backend
        for {command}`auth-source-1password` (lisp/init-systems.el) that
        resolves credentials for gptel, {command}`forge`, smtpmail, etc.
        from the vault. Pulls the unfree `_1password-cli` package, so it
        needs `allowUnfree`
      '';
    };

    sops = {
      enable = lib.mkEnableOption ''
        the {command}`sops` CLI on the wrapper PATH, required by
        {command}`sops.el` (lisp/init-systems.el) for transparent
        encrypt/decrypt of SOPS-managed files
      '';
    };

    claudeCode = {
      enable = lib.mkEnableOption ''
        the Claude Code CLI ({command}`claude`) on the wrapper PATH, the
        external agent {command}`claude-code-ide` (lisp/init-ai.el) drives.
        Pulls the unfree `claude-code` package, so it needs `allowUnfree`
      '';
    };
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
        pkgsWithOverlay.eca
      ]
      # jinx dictionaries; the wrapper's ASPELL_CONF is what jinx reads.
      ++ cfg.spell.dictionaries;
      # Colour-emoji fallback for lisp/init-ui.el's fontsets, BlexMono (the
      # default face and nerd-icons font, as in module.nix), and the design
      # system's families (Fira Sans is the variable-pitch face).
      fonts.packages = [
        pkgs.noto-fonts-color-emoji
        pkgs.nerd-fonts.blex-mono
        pkgs.jost
        pkgs.fira-sans
        pkgs.ibm-plex
      ];
      environment.variables = lib.mkIf cfg.defaultEditor {
        EDITOR = lib.mkOverride 900 "${lib.getBin editorScript}/bin/jotain-editor";
        VISUAL = lib.mkOverride 900 "${lib.getBin visualScript}/bin/jotain-visual";
      };
    })
  ];
}
