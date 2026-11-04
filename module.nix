# module.nix — Home Manager module for the Emacs daemon.
#
# Import this module from your home-manager configuration and enable:
#
#   imports = [ /path/to/etc/emacs/module.nix ];
#
#   services.emacsConfig = {
#     enable = true;
#     defaultEditor = true;
#     client.enable = true;
#     eca.openrouter.enable = true;   # eca OpenRouter provider (config.json)
#     environmentFile = "/run/secrets/emacs-config-env";  # OPENROUTER_API_KEY=… etc.
#   };
#
# Modelled after home-manager's services.emacs.
args@{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.emacsConfig;
  emacsConfigOverlay = args.emacsConfigOverlay or (import ./overlay.nix);
  # A host whose pkgs already carry the overlay (the jylhis hosts) skips a
  # second nixpkgs fixpoint.
  pkgsWithOverlay = if pkgs ? emacsConfigPackages then pkgs else pkgs.extend emacsConfigOverlay;
  selectedPackage = if cfg.package != null then cfg.package else pkgsWithOverlay.emacsConfigPackages;
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  startWithSession =
    if cfg.startWithUserSession == "graphical" then true else cfg.startWithUserSession;

  emacsBinPath = "${selectedPackage}/bin";
  emacsVersion = lib.getVersion selectedPackage;

  clientWMClass = if lib.versionAtLeast emacsVersion "28" then "Emacsd" else "Emacs";

  # Workaround for https://debbugs.gnu.org/47511
  needsSocketWorkaround = lib.versionOlder emacsVersion "28" && cfg.socketActivation.enable;

  # Match the default socket path so emacsclient works without wrapping.
  socketDir = "%t/emacs";
  socketPath = "${socketDir}/server";

  # Desktop entry for the Emacs client (adapted from upstream emacs.desktop).
  clientDesktopItem = pkgs.writeTextDir "share/applications/emacs-config-client.desktop" (
    lib.generators.toINI { } {
      "Desktop Entry" = {
        Type = "Application";
        Exec = "${emacsBinPath}/emacsclient ${lib.concatStringsSep " " cfg.client.arguments} %F";
        Terminal = false;
        Name = "Emacs Client (config)";
        Icon = "emacs";
        Comment = "Edit text";
        GenericName = "Text Editor";
        MimeType = "text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;";
        Categories = "Development;TextEditor;";
        Keywords = "Text;Editor;";
        StartupWMClass = clientWMClass;
      };
    }
  );

  # Where xdg.configFile installs the config. --init-directory is pinned
  # here so a stray ~/.emacs.d/ is never picked up.
  initDirectory = "${config.xdg.configHome}/emacs";

  # Byte-compiled config, so the daemon loads .elc instead of
  # interpreting .el (and deferred native compilation, which never fires
  # for plain .el loads, can fill var/eln-cache).
  #
  # Same derivation as nix/checks.nix' `elisp-compile', so on the default
  # configuration `home-manager switch' substitutes CI's cachix artifact.
  # A custom `services.emacsConfig.package' or nixpkgs pin builds it locally.
  # `.core' is the inner emacsWithPackages result (see
  # nix/config-compiled.nix); the `or' covers a custom package.
  compiledConfig = import ./nix/config-compiled.nix {
    inherit pkgs;
    emacs = selectedPackage.core or selectedPackage;
    nativeCompile = cfg.nativeCompile.enable;
  };

  # Generator for ~/.config/eca/config.json (see nix/eca-config.nix).
  ecaConfig = import ./nix/eca-config.nix { inherit lib pkgs; };

  # Shared runtime binaries (nix/runtime-deps.nix) plus the opt-in tools,
  # prepended to PATH in emacsWrapper, since the systemd user
  # service does not inherit the login-shell PATH.
  runtimeDeps =
    import ./nix/runtime-deps.nix { inherit pkgs pkgsWithOverlay; }
    ++ lib.optional cfg.devenv.enable pkgs.devenv
    ++ lib.optional cfg.sonarlint.enable pkgs.sonarlint-ls
    ++ lib.optional cfg.dockerfileLsp.enable pkgs.dockerfile-language-server
    ++ lib.optional cfg.onePassword.enable pkgs._1password-cli
    ++ lib.optional cfg.sops.enable pkgs.sops
    ++ lib.optional cfg.claudeCode.enable pkgs.claude-code;

  # Colour-emoji fallback for the `emoji' / `symbol' fontsets
  # (lisp/init-ui.el).
  emojiFontPackages = lib.optional isLinux pkgs.noto-fonts-color-emoji;

  # Nerd Font glyphs for the icon stack. BlexMono is the first entry in
  # `my-font-preferences' (lisp/init-ui.el); keep the two in step.
  # Fira Sans is the variable-pitch (body) face of the design system; Jost and
  # IBM Plex Mono are its display and mono faces.
  iconFontPackages = [
    pkgs.nerd-fonts.blex-mono
    pkgs.jost
    pkgs.fira-sans
    pkgs.ibm-plex
  ];

  runtimePath = lib.makeBinPath runtimeDeps;

  # `emacs` with --init-directory always set, for the daemon and any
  # interactive invocation.
  emacsWrapper = pkgs.writeShellScriptBin "emacs" ''
    export PATH=${runtimePath}''${PATH:+:$PATH}
    ${lib.optionalString cfg.nativeCompile.enable ''
      export EMACS_CONFIG_ELN_PATH=${compiledConfig}/share/emacs/native-lisp
    ''}
    exec ${emacsBinPath}/emacs --init-directory=${lib.escapeShellArg initDirectory} "$@"
  '';

  # EDITOR fallback without a daemon; via emacsWrapper to keep
  # --init-directory.
  editorFallback = pkgs.writeShellScript "emacs-config-editor-fallback" ''
    exec ${emacsWrapper}/bin/emacs -nw -- "$@"
  '';

  # EDITOR — terminal-friendly emacsclient (works over SSH, in git commit, etc.)
  editorScript = pkgs.writeShellScriptBin "emacs-config-editor" ''
    exec ${lib.getBin selectedPackage}/bin/emacsclient \
      --tty \
      --alternate-editor=${editorFallback} \
      -- \
      "$@"
  '';

  # VISUAL — opens a GUI emacsclient frame.
  visualScript = pkgs.writeShellScriptBin "emacs-config-visual" ''
    exec ${lib.getBin selectedPackage}/bin/emacsclient \
      --create-frame \
      --alternate-editor=${emacsWrapper}/bin/emacs \
      -- \
      "$@"
  '';

  # `emacsctl <start|stop|status|restart|logs>`: drives the systemd user
  # service.
  emacsctlScript = pkgs.writeShellScriptBin "emacsctl" ''
    case "''${1:-status}" in
      start)   exec systemctl --user start emacs-config ;;
      stop)    exec systemctl --user stop emacs-config ;;
      restart) exec systemctl --user restart emacs-config ;;
      status)  exec systemctl --user status emacs-config ;;
      logs)    exec journalctl --user -u emacs-config -f ;;
      *) echo "usage: emacsctl {start|stop|status|restart|logs}" >&2; exit 2 ;;
    esac
  '';

  systemdWantedBy =
    if cfg.startWithUserSession == "graphical" then "graphical-session.target" else "default.target";

  # Aliases after https://rahuljuliato.com/posts/launching-emacs-terminal :
  # `emd` foreground daemon, `em` terminal client, `emg` GUI client.
  shellAliasMap = {
    "${cfg.shellAliases.prefix}emd" = "${emacsWrapper}/bin/emacs --fg-daemon";
    "${cfg.shellAliases.prefix}em" = "${lib.getBin editorScript}/bin/emacs-config-editor";
    "${cfg.shellAliases.prefix}emg" = "${lib.getBin visualScript}/bin/emacs-config-visual";
  };
in
{
  options.services.emacsConfig = {
    enable = lib.mkEnableOption "the Emacs daemon";

    package = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      defaultText = lib.literalExpression "null";
      description = ''
        Custom Emacs package to use. Leave unset for the full
        distribution (`emacsConfigPackages`: pgtk/Wayland GUI).
      '';
    };

    nativeCompile.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      example = true;
      description = ''
        AOT native-compile the config into the store, so the
        daemon loads `.eln` for `init.el` and `lisp/` instead of
        JIT-compiling into `var/eln-cache` after every deploy that
        touches `lisp/` (which moves the store path and invalidates
        that cache). `early-init.el` always runs from `.elc`: its
        `.eln` lookup happens before it extends
        `native-comp-eln-load-path`.

        Emacs `realpath()`s the source before hashing it into the
        `.eln` name (src/comp.c, Bug#44701), so the Home Manager
        symlinks resolve to the path the AOT step compiled. Off by
        default for the cost: a native-compilation pass whenever the
        config rebuilds, and roughly 50-150 MB of `.eln`.
      '';
    };

    extraOptions = lib.mkOption {
      type = with lib.types; listOf str;
      default = [ ];
      example = [
        "-f"
        "exwm-enable"
      ];
      description = ''
        Extra command-line arguments to pass to {command}`emacs` when
        starting the daemon.
      '';
    };

    client = {
      enable = lib.mkEnableOption "generation of the Emacs client desktop file";

      arguments = lib.mkOption {
        type = with lib.types; listOf str;
        default = [ "-c" ];
        description = ''
          Command-line arguments to pass to {command}`emacsclient`.
        '';
      };
    };

    socketActivation = {
      enable = lib.mkEnableOption "systemd socket activation for the service";
    };

    startWithUserSession = lib.mkOption {
      type = with lib.types; either bool (enum [ "graphical" ]);
      default = !cfg.socketActivation.enable;
      defaultText = lib.literalExpression "!config.services.emacsConfig.socketActivation.enable";
      example = "graphical";
      description = ''
        Whether to launch the service with the systemd user session.
        If `true`, the service is started by `default.target`.
        If `"graphical"`, it is started by `graphical-session.target`.
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

    environmentFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      example = "/run/secrets/emacs-config-env";
      description = ''
        Path to a {manpage}`systemd.exec(5)`-style environment file
        (`VAR=value` lines) loaded into the daemon's environment
        and inherited by every subprocess Emacs spawns. Use it for
        credentials read from the environment, such as gptel's
        {env}`OPENROUTER_API_KEY` / {env}`ANTHROPIC_API_KEY` /
        {env}`GEMINI_API_KEY` (lisp/init-ai.el) and the {command}`eca`
        server's provider keys. Point it at a runtime secret path
        (sops-nix, agenix, …); it is read at daemon start and never
        copied into the Nix store. It becomes the service's
        {var}`EnvironmentFile`. For auth-source alternatives see
        {option}`services.emacsConfig.onePassword.enable` and
        {option}`services.emacsConfig.authSources`.
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
        reach the daemon, via the {env}`EMACS_CONFIG_AUTH_SOURCES` variable read
        by lisp/init-systems.el. auth-source consumers (gptel,
        {command}`forge`, smtpmail, circe) then resolve credentials from
        them; the {command}`eca` server, which reads only its environment,
        gets its provider keys exported from auth-source before each
        session (see {file}`lisp/init-ai.el`).
      '';
    };

    onePassword = {
      enable = lib.mkEnableOption ''
        the 1Password CLI ({command}`op`) on the wrapper PATH, the backend
        for {command}`auth-source-1password` (lisp/init-systems.el) that
        resolves credentials for gptel, {command}`forge`, smtpmail, etc.
        from the vault when they are not supplied through
        {option}`services.emacsConfig.environmentFile`. Pulls the unfree
        `_1password-cli` package, so it needs `allowUnfree`
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
        external agent {command}`claude-code-ide` (lisp/init-ai.el, `C-c q`)
        drives. Pulls the unfree `claude-code` package, so it needs
        `allowUnfree`
      '';
    };

    sonarlint = {
      enable = lib.mkEnableOption "SonarLint language server ({command}`M-x my-sonarlint`)";
    };

    devenv = {
      enable = lib.mkEnableOption ''
        the {command}`devenv` CLI on the wrapper PATH, for the native
        environment loader (`devenv-env-global-mode`, lisp/devenv.el)
        under daemons whose login shell does not export it. Opt-in
        because exec-path-from-shell normally finds the user's own
        devenv, and `pkgs.devenv` bundles its own nix, which can
        version-skew against per-project devenv installs
      '';
    };

    spell = {
      dictionaries = lib.mkOption {
        type = with lib.types; listOf package;
        default = [ pkgs.aspellDicts.en ];
        defaultText = lib.literalExpression "[ pkgs.aspellDicts.en ]";
        example = lib.literalExpression "[ pkgs.aspellDicts.en pkgs.aspellDicts.fi ]";
        description = ''
          Aspell dictionary packages for jinx spell-checking
          (lisp/init-writing.el). Installed into the profile, where
          libaspell's NIX_PROFILES patch finds them at runtime and
          enchant's aspell backend hands them to jinx.
        '';
      };
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
          {option}`services.emacsConfig.eca.openrouter.enable` is set or
          {option}`services.emacsConfig.eca.settings` is non-empty.
        '';
      };

      openrouter.enable = lib.mkEnableOption ''
        the default OpenRouter provider in the generated {command}`eca`
        config. The provider and model catalogue come from
        {file}`config/eca/config.json` (kept in sync with gptel's models in
        lisp/init-ai.el); the API key is read at runtime from
        {env}`OPENROUTER_API_KEY` via eca's `''${env:…}` interpolation, so
        no secret reaches the Nix store. Supply it through
        {option}`services.emacsConfig.environmentFile`. gptel defaults to
        OpenRouter regardless of this option
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
          {option}`services.emacsConfig.environmentFile`, so none reach the Nix
          store.
        '';
      };
    };

    shellAliases = {
      enable = lib.mkEnableOption "shell aliases for the daemon and clients";

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

    dockerfileLsp = {
      enable = lib.mkEnableOption "Dockerfile language server ({command}`docker-langserver`), auto-attached by Eglot in {command}`dockerfile-mode`";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !cfg.socketActivation.enable || isLinux;
        message = "services.emacsConfig.socketActivation.enable is only supported on Linux/systemd.";
      }
    ];

    home.sessionVariables = lib.mkIf cfg.defaultEditor {
      EDITOR = "${lib.getBin editorScript}/bin/emacs-config-editor";
      VISUAL = "${lib.getBin visualScript}/bin/emacs-config-visual";
    };

    programs = {

      bash.shellAliases = lib.mkIf cfg.shellAliases.enable shellAliasMap;
      zsh.shellAliases = lib.mkIf cfg.shellAliases.enable shellAliasMap;
      fish.shellAliases = lib.mkIf cfg.shellAliases.enable shellAliasMap;
    };
    fonts.fontconfig.enable = lib.mkIf isLinux true;

    home.packages = [
      selectedPackage
      editorScript
      visualScript
      emacsctlScript
      # hiPrio so the wrapped `emacs` shadows the unwrapped binary
      # that ships inside the selected package.
      (lib.hiPrio emacsWrapper)
    ]
    ++ cfg.spell.dictionaries
    ++ emojiFontPackages
    ++ iconFontPackages
    ++ lib.optional (cfg.client.enable && pkgs.stdenv.hostPlatform.isLinux) (
      lib.hiPrio clientDesktopItem
    );

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

    systemd.user.services.emacs-config = lib.mkIf isLinux (
      {
        Unit = {
          Description = "Emacs text editor";
          Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";

          After = lib.optional (cfg.startWithUserSession == "graphical") "graphical-session.target";
          PartOf = lib.optional (cfg.startWithUserSession == "graphical") "graphical-session.target";

          # Avoid killing the session, which may be full of unsaved buffers.
          X-RestartIfChanged = false;
        }
        // lib.optionalAttrs needsSocketWorkaround {
          RefuseManualStart = true;
        };

        Service = {
          Type = "notify";

          # Wrap in a login shell so Emacs inherits the user's
          # environment ($PATH, $NIX_PROFILES, etc.).
          ExecStart = ''${pkgs.runtimeShell} -l -c "${emacsWrapper}/bin/emacs --fg-daemon${lib.optionalString cfg.socketActivation.enable "=${lib.escapeShellArg socketPath}"} ${lib.escapeShellArgs cfg.extraOptions}"'';

          # Emacs exits with status 15 after SIGTERM.
          SuccessExitStatus = 15;

          Restart = "on-failure";
        }
        // lib.optionalAttrs needsSocketWorkaround {
          ExecStartPost = "${pkgs.coreutils}/bin/chmod --changes -w ${socketDir}";
          ExecStopPost = "${pkgs.coreutils}/bin/chmod --changes +w ${socketDir}";
        }
        // lib.optionalAttrs (cfg.environmentFile != null) {
          EnvironmentFile = cfg.environmentFile;
        }
        // lib.optionalAttrs (cfg.authSources != [ ]) {
          Environment = [ "EMACS_CONFIG_AUTH_SOURCES=${lib.concatStringsSep ":" cfg.authSources}" ];
        };
      }
      // lib.optionalAttrs startWithSession {
        Install = {
          WantedBy = [ systemdWantedBy ];
        };
      }
    );

    systemd.user.sockets.emacs-config = lib.mkIf (isLinux && cfg.socketActivation.enable) {
      Unit = {
        Description = "Emacs text editor";
        Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs/";
      };

      Socket = {
        ListenStream = socketPath;
        FileDescriptorName = "server";
        SocketMode = "0600";
        DirectoryMode = "0700";
        # Prevents the service from immediately restarting after stop,
        # due to `server-force-stop' in `kill-emacs-hook' calling
        # `server-running-p', which opens the socket file.
        FlushPending = true;
      };

      Install = {
        WantedBy = [ "sockets.target" ];
        RequiredBy = [ "emacs-config.service" ];
      };
    };
  };
}
