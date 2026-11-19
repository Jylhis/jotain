# module.nix: Home Manager module for Jotain. The daemon is home-manager's
# services.emacs, running the wrapped Jotain Emacs; this module adds the config
# deployment, the wrapper PATH tools and the daemon environment.
#
#   imports = [ /path/to/jotain/module.nix ];
#   services.jotain = {
#     enable = true;
#     environmentFile = "/run/secrets/jotain-env";
#   };
args@{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.jotain;
  jotainOverlay = args.jotainOverlay or (import ./overlay.nix);
  # A host whose pkgs already carry the overlay skips a second nixpkgs
  # fixpoint.
  pkgsWithOverlay = if pkgs ? jotainEmacsPackages then pkgs else pkgs.extend jotainOverlay;
  # With services.jotain.users.<user> on in NixOS (module-system.nix) the system
  # builds the programs and installs them; this module takes them from there.
  osConfig = args.osConfig or { };
  system = osConfig.services.jotain.users.${config.home.username} or { enable = false; };

  # Where xdg.configFile installs the config. --init-directory is pinned
  # here so a stray ~/.emacs.d/ is never picked up.
  initDirectory = "${config.xdg.configHome}/emacs";

  ecaConfig = import ./nix/eca-config.nix { inherit lib pkgs; };
  sharedOptions = import ./nix/module-options.nix { inherit lib; };

  built =
    if system.enable then
      system.built
    else
      import ./nix/user-programs.nix {
        inherit
          pkgs
          pkgsWithOverlay
          initDirectory
          cfg
          ;
        package = if cfg.package != null then cfg.package else pkgsWithOverlay.jotainEmacsPackages;
      };
  inherit (built)
    package
    compiledConfig
    daemonPackage
    emacsWrapper
    editorScript
    visualScript
    jotctlScript
    fontPackages
    ;

  # Aliases after https://rahuljuliato.com/posts/launching-emacs-terminal :
  # `emd` foreground daemon, `em` terminal client, `emg` GUI client.
  shellAliasMap = {
    "${cfg.shellAliases.prefix}emd" = "${emacsWrapper}/bin/emacs --fg-daemon";
    "${cfg.shellAliases.prefix}em" = "${lib.getBin editorScript}/bin/jotain-editor";
    "${cfg.shellAliases.prefix}emg" = "${lib.getBin visualScript}/bin/jotain-visual";
  };
in
{
  options.services.jotain = {
    enable = lib.mkEnableOption "the Jotain Emacs daemon";

    inherit (sharedOptions)
      package
      nativeCompile
      defaultEditor
      sonarlint
      dockerfileLsp
      onePassword
      sops
      claudeCode
      spell
      ;

    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      example = "/run/secrets/jotain-env";
      description = ''
        Path to a {manpage}`systemd.exec(5)`-style environment file
        (`VAR=value` lines) loaded into the Jotain daemon's environment
        and inherited by every subprocess Emacs spawns. Use it for
        credentials read from the environment, such as gptel's
        {env}`OPENROUTER_API_KEY` / {env}`ANTHROPIC_API_KEY` /
        {env}`GEMINI_API_KEY` (lisp/init-ai.el) and the {command}`eca`
        server's provider keys. Point it at a runtime secret path
        (sops-nix, agenix, …); it is read at daemon start and never
        copied into the Nix store. It becomes the service's
        {var}`EnvironmentFile`. For auth-source alternatives see
        {option}`services.jotain.onePassword.enable` and
        {option}`services.jotain.authSources`.
      '';
    };

    authSources = lib.mkOption {
      type = with lib.types; listOf str;
      default = [ ];
      example = [ "/run/secrets/authinfo" ];
      description = ''
        Extra authinfo/netrc file paths prepended to Emacs's
        {var}`auth-sources`, ahead of {file}`~/.authinfo(.gpg)`. Point them
        at runtime secret files (sops-nix, agenix, …); only the *paths*
        reach the daemon, via the {env}`JOTAIN_AUTH_SOURCES` variable read
        by lisp/init-systems.el. auth-source consumers (gptel,
        {command}`forge`, smtpmail, circe) then resolve credentials from
        them; the {command}`eca` server, which reads only its environment,
        gets its provider keys exported from auth-source before each
        session (see {file}`lisp/init-ai.el`).
      '';
    };

    eca = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = cfg.eca.openrouter.enable || cfg.eca.settings != { };
        defaultText = lib.literalExpression "eca.openrouter.enable || eca.settings != { }";
        example = true;
        description = ''
          Whether to install {file}`~/.config/eca/config.json` for the
          {command}`eca` server (the AI pair-programming backend in
          lisp/init-ai.el). Enabled automatically when
          {option}`services.jotain.eca.openrouter.enable` is set or
          {option}`services.jotain.eca.settings` is non-empty.
        '';
      };

      openrouter.enable = lib.mkEnableOption ''
        the default OpenRouter provider in the generated {command}`eca`
        config. The provider and model catalogue come from
        {file}`config/eca/config.json` (kept in sync with gptel's models in
        lisp/init-ai.el); the API key is read at runtime from
        {env}`OPENROUTER_API_KEY` via eca's `''${env:…}` interpolation, so
        no secret reaches the Nix store. Supply it through
        {option}`services.jotain.environmentFile`. gptel defaults to
        Claude on the direct Anthropic API regardless of this option
      '';

      settings = lib.mkOption {
        type = ecaConfig.settingsType;
        default = { };
        example = lib.literalExpression ''
          {
            providers.anthropic = {
              api = "anthropic";
              key = "''${env:ANTHROPIC_API_KEY}";
              models."claude-sonnet-4.6" = { };
            };
          }
        '';
        description = ''
          Freeform {command}`eca` configuration, rendered to
          {file}`~/.config/eca/config.json` and deep-merged over the default
          OpenRouter provider (these values win). Any eca key is expressible
          (`providers`, `models`, `rules`, `mcpServers`, `behavior`, …). Use
          eca's `''${env:VAR}` syntax for secrets, supplied through
          {option}`services.jotain.environmentFile`, so none reach the Nix
          store.
        '';
      };
    };

    shellAliases = {
      enable = lib.mkEnableOption "shell aliases for the Jotain daemon and clients";

      prefix = lib.mkOption {
        type = lib.types.str;
        default = "";
        example = "j";
        description = ''
          Optional prefix to namespace the {command}`emd` / {command}`em` /
          {command}`emg` aliases (e.g. set to `"j"` to get {command}`jemd`,
          {command}`jem`, {command}`jemg`).
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    services.emacs = {
      enable = true;
      package = lib.mkDefault daemonPackage;
    };

    home.sessionVariables = lib.mkIf cfg.defaultEditor {
      EDITOR = "${lib.getBin editorScript}/bin/jotain-editor";
      VISUAL = "${lib.getBin visualScript}/bin/jotain-visual";
    };

    programs = {
      bash.shellAliases = lib.mkIf cfg.shellAliases.enable shellAliasMap;
      zsh.shellAliases = lib.mkIf cfg.shellAliases.enable shellAliasMap;
      fish.shellAliases = lib.mkIf cfg.shellAliases.enable shellAliasMap;
    };
    fonts.fontconfig.enable = true;

    # The programs come from the system where services.jotain.users.<user> is on.
    home.packages =
      lib.optionals (!system.enable) (
        [
          package
          editorScript
          visualScript
          jotctlScript
          # hiPrio so the wrapped `emacs` shadows the unwrapped binary
          # that ships inside the package.
          (lib.hiPrio emacsWrapper)
        ]
        ++ cfg.spell.dictionaries
      )
      ++ fontPackages;

    # Entry files and lisp/ come from compiledConfig: an .eln is named
    # after a hash of its source path, so sources from any other store path
    # (e.g. ./init.el) would always miss. templates/ is for `tempel-path'
    # (lisp/init-snippets.el).
    xdg.configFile = {
      "emacs/early-init.el".source = "${compiledConfig}/early-init.el";
      "emacs/early-init.elc".source = "${compiledConfig}/early-init.elc";
      "emacs/init.el".source = "${compiledConfig}/init.el";
      "emacs/init.elc".source = "${compiledConfig}/init.elc";
      "emacs/lisp".source = "${compiledConfig}/lisp";
      "emacs/templates".source = ./templates;
    }
    // lib.optionalAttrs cfg.eca.enable {
      # ${env:…} references resolve at runtime from the daemon environment
      # (environmentFile), so no secret is written to the store.
      "eca/config.json".source = ecaConfig.mkConfigFile {
        includeOpenRouter = cfg.eca.openrouter.enable;
        inherit (cfg.eca) settings;
      };
    };

    systemd.user.services.emacs.Service =
      lib.optionalAttrs (cfg.environmentFile != null) {
        EnvironmentFile = cfg.environmentFile;
      }
      // lib.optionalAttrs (cfg.authSources != [ ]) {
        Environment = [ "JOTAIN_AUTH_SOURCES=${lib.concatStringsSep ":" cfg.authSources}" ];
      };
  };
}
