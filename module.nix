# module.nix — Home Manager module for the Jotain Emacs daemon.
#
# Import this module from your home-manager configuration and enable:
#
#   imports = [ /path/to/jotain/module.nix ];
#
#   services.jotain = {
#     enable = true;
#     defaultEditor = true;
#     client.enable = true;
#     openrouter.enable = true;  # eca OpenRouter provider; needs OPENROUTER_API_KEY
#   };
#
# Modelled after the home-manager services.emacs module, but uses the
# Jotain-built Emacs and `jotain` naming throughout.
args@{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.jotain;
  jotainOverlay = args.jotainOverlay or (import ./overlay.nix);
  pkgsWithOverlay = pkgs.extend jotainOverlay;
  selectedPackage = if cfg.package != null then cfg.package else pkgsWithOverlay.jotainEmacsPackages;
  inherit (pkgs.stdenv.hostPlatform) isLinux;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
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
  clientDesktopItem = pkgs.writeTextDir "share/applications/jotain-client.desktop" (
    lib.generators.toINI { } {
      "Desktop Entry" = {
        Type = "Application";
        Exec = "${emacsBinPath}/emacsclient ${lib.concatStringsSep " " cfg.client.arguments} %F";
        Terminal = false;
        Name = "Jotain Client";
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

  # Path the Jotain config is installed to via xdg.configFile below.
  # Pinning --init-directory to this location prevents Emacs from
  # falling back to a stray ~/.emacs.d/ on the user's machine.
  initDirectory = "${config.xdg.configHome}/emacs";

  # Byte-compiled copy of the Jotain config, so the daemon executes the
  # same .elc artifact the elisp-compile flake check verifies instead of
  # interpreting raw .el on every start. Loading .elc is also what lets
  # deferred native compilation produce .eln files into the writable
  # var/eln-cache — JIT native comp never triggers for plain .el loads.
  #
  # This *is* nix/checks.nix' `elisp-compile': both call the same
  # nix/config-compiled.nix, so on the default configuration they are the
  # same store path and `home-manager switch' substitutes CI's artifact
  # from cachix instead of running Emacs locally. They diverge under a
  # custom `services.jotain.package' or a different nixpkgs pin —
  # inherent, and only costs the old behaviour of building it here.
  #
  # `.core' is the inner emacsWithPackages result: the outer wrapper adds
  # a runtime PATH, INFOPATH and ASPELL_CONF, none of which a batch
  # byte-compile reads, and depending on it would drag jotainInfo (and so
  # every `;;; @doc' block and docs/*.mdx page) into this derivation. The
  # `or' covers a user-supplied `services.jotain.package'.
  compiledConfig = import ./nix/config-compiled.nix {
    inherit pkgs;
    emacs = selectedPackage.core or selectedPackage;
    nativeCompile = cfg.nativeCompile.enable;
  };

  # Runtime dependencies the Elisp config invokes unconditionally,
  # factored into nix/runtime-deps.nix so module-system.nix and
  # module-nix-on-droid.nix satisfy the same contract. Prepending these
  # to PATH in the wrapper keeps them available regardless of launch
  # context — notably launchd on macOS, which doesn't inherit the
  # user's login-shell PATH.
  runtimeDeps =
    import ./nix/runtime-deps.nix { inherit pkgs pkgsWithOverlay; }
    ++ lib.optional cfg.devenv.enable pkgs.devenv
    ++ lib.optional cfg.sonarlint.enable pkgs.sonarlint-ls
    ++ lib.optional cfg.dockerfileLsp.enable pkgs.dockerfile-language-server;

  # Colour-emoji fallback for the `emoji' / `symbol' fontsets wired in
  # lisp/init-ui.el.  macOS ships Apple Color Emoji system-wide, so the
  # Nix font would just bloat the closure there.
  emojiFontPackages = lib.optional isLinux pkgs.noto-fonts-color-emoji;

  # Nerd Font glyphs for the icon stack (nerd-icons, doom-modeline,
  # corfu/marginalia margins, dirvish, ibuffer). BlexMono (IBM Plex Mono
  # patched) is the first entry in `jotain-font-preferences'
  # (lisp/init-ui.el), so the icons match the default editor face out of
  # the box; keep the two in step when either changes.
  iconFontPackages = [ pkgs.nerd-fonts.blex-mono ];

  runtimePath = lib.makeBinPath runtimeDeps;

  # Wrapper around `emacs` that always passes --init-directory, so the
  # daemon and any interactive `emacs` invocation pick up Jotain
  # regardless of Emacs's user-emacs-directory discovery order.
  emacsWrapper = pkgs.writeShellScriptBin "emacs" ''
    export PATH=${runtimePath}''${PATH:+:$PATH}
    ${lib.optionalString cfg.nativeCompile.enable ''
      export JOTAIN_ELN_PATH=${compiledConfig}/share/emacs/native-lisp
    ''}
    exec ${emacsBinPath}/emacs --init-directory=${lib.escapeShellArg initDirectory} "$@"
  '';

  # Fallback script for EDITOR when the daemon is not running.
  # Goes through emacsWrapper so --init-directory is preserved.
  editorFallback = pkgs.writeShellScript "jotain-editor-fallback" ''
    exec ${emacsWrapper}/bin/emacs -nw -- "$@"
  '';

  # EDITOR — terminal-friendly emacsclient (works over SSH, in git commit, etc.)
  editorScript = pkgs.writeShellScriptBin "jotain-editor" ''
    exec ${lib.getBin selectedPackage}/bin/emacsclient \
      --tty \
      --alternate-editor=${editorFallback} \
      -- \
      "$@"
  '';

  # VISUAL — opens a GUI emacsclient frame.
  visualScript = pkgs.writeShellScriptBin "jotain-visual" ''
    exec ${lib.getBin selectedPackage}/bin/emacsclient \
      --create-frame \
      --alternate-editor=${emacsWrapper}/bin/emacs \
      -- \
      "$@"
  '';

  # launchd agent label — Home Manager prefixes user agents with
  # "org.nix-community.home."; the agent below is named `jotain`.
  launchdLabel = "org.nix-community.home.jotain";
  launchdPlist = "${config.home.homeDirectory}/Library/LaunchAgents/${launchdLabel}.plist";

  # Cross-platform daemon control. `jotctl <start|stop|status|restart|logs>`
  # drives launchd on macOS and the systemd user service on Linux — the same
  # daemon defined by launchd.agents.jotain / systemd.user.services.jotain.
  jotctlScript = pkgs.writeShellScriptBin "jotctl" (
    if isDarwin then
      ''
        label=${launchdLabel}
        plist=${lib.escapeShellArg launchdPlist}
        uid=$(${pkgs.coreutils}/bin/id -u)
        target="gui/$uid/$label"
        case "''${1:-status}" in
          start)   launchctl bootstrap "gui/$uid" "$plist" 2>/dev/null \
                     || launchctl kickstart "$target" ;;
          stop)    launchctl bootout "$target" ;;
          restart) launchctl kickstart -k "$target" ;;
          status)  launchctl print "$target" ;;
          logs)    echo "macOS launchd does not capture Jotain stdout (no StandardOutPath set); showing launchd state:" >&2
                   launchctl print "$target" ;;
          *) echo "usage: jotctl {start|stop|status|restart|logs}" >&2; exit 2 ;;
        esac
      ''
    else
      ''
        case "''${1:-status}" in
          start)   exec systemctl --user start jotain ;;
          stop)    exec systemctl --user stop jotain ;;
          restart) exec systemctl --user restart jotain ;;
          status)  exec systemctl --user status jotain ;;
          logs)    exec journalctl --user -u jotain -f ;;
          *) echo "usage: jotctl {start|stop|status|restart|logs}" >&2; exit 2 ;;
        esac
      ''
  );

  systemdWantedBy =
    if cfg.startWithUserSession == "graphical" then "graphical-session.target" else "default.target";

  # Short aliases inspired by https://rahuljuliato.com/posts/launching-emacs-terminal :
  # `emd` brings up a foreground daemon, `em` connects a terminal client, `emg`
  # connects a graphical client. All three reuse the wrappers already built
  # above so --init-directory and runtime PATH stay consistent.
  shellAliasMap = {
    "${cfg.shellAliases.prefix}emd" = "${emacsWrapper}/bin/emacs --fg-daemon";
    "${cfg.shellAliases.prefix}em" = "${lib.getBin editorScript}/bin/jotain-editor";
    "${cfg.shellAliases.prefix}emg" = "${lib.getBin visualScript}/bin/jotain-visual";
  };
in
{
  options.services.jotain = {
    enable = lib.mkEnableOption "the Jotain Emacs daemon";

    package = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      defaultText = lib.literalExpression "null";
      description = ''
        Custom Jotain Emacs package to use. Leave this unset to use the
        cache-friendly default build from `emacs.nix`.

        This is also how to install the curated tree-sitter grammar set
        (~26 instead of ~275) — set it to the flake's
        `packages.<system>.emacs-lite`. Both grammar sets are `linkFarm`s
        over the *same* per-grammar store paths, so that swap is purely a
        closure/download saving: no parser is ever recompiled either way.
      '';
    };

    nativeCompile.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      example = true;
      description = ''
        AOT native-compile the Jotain config into the store, so the
        daemon loads `.eln` for `init.el` and the `lisp/` modules from
        `services.jotain`'s own derivation instead of JIT-compiling
        into `var/eln-cache` after every deploy (every deploy that
        touches `lisp/` moves the store path, which invalidates the
        JIT cache). `early-init.el` is structurally excluded: its
        `.eln` lookup happens before the load path is extended, so it
        always runs from `.elc` — with or without this option.

        The store-`.eln`-through-symlinks mechanism is sound: Emacs
        `realpath()`s the source before hashing it into the `.eln`
        name (src/comp.c, Bug#44701), so the HM symlinks resolve to
        the exact path the AOT step compiled against. Off by default
        only because enabling it adds a native-compilation pass to
        every activation that rebuilds the config, and roughly
        50–150 MB of `.eln` to the closure.
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
      enable = lib.mkEnableOption "generation of Jotain client desktop file";

      arguments = lib.mkOption {
        type = with lib.types; listOf str;
        default = [ "-c" ];
        description = ''
          Command-line arguments to pass to {command}`emacsclient`.
        '';
      };
    };

    socketActivation = {
      enable = lib.mkEnableOption "systemd socket activation for the Jotain service";
    };

    startWithUserSession = lib.mkOption {
      type = with lib.types; either bool (enum [ "graphical" ]);
      default = !cfg.socketActivation.enable;
      defaultText = lib.literalExpression "!config.services.jotain.socketActivation.enable";
      example = "graphical";
      description = ''
        Whether to launch the Jotain service with the systemd user session.
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

    sonarlint = {
      enable = lib.mkEnableOption "SonarLint language server ({command}`M-x jotain-sonarlint`)";
    };

    devenv = {
      enable = lib.mkEnableOption ''
        the {command}`devenv` CLI on the wrapper PATH, for the native
        environment loader (`devenv-env-global-mode`, lisp/devenv.el)
        under launchd/systemd daemons whose login shell does not export
        it. Opt-in because exec-path-from-shell normally finds the
        user's own devenv, and `pkgs.devenv` bundles its own nix and
        can version-skew against per-project devenv installs
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

    openrouter = {
      enable = lib.mkEnableOption ''
        the OpenRouter provider for {command}`eca` by installing
        {file}`~/.config/eca/config.json`. Requires {env}`OPENROUTER_API_KEY`
        in the environment. gptel already defaults to OpenRouter
        regardless of this option
      '';
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

    dockerfileLsp = {
      enable = lib.mkEnableOption "Dockerfile language server ({command}`docker-langserver`), auto-attached by Eglot in {command}`dockerfile-mode`";
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !cfg.socketActivation.enable || isLinux;
        message = "services.jotain.socketActivation.enable is only supported on Linux/systemd.";
      }
    ];

    home.sessionVariables = lib.mkIf cfg.defaultEditor {
      EDITOR = "${lib.getBin editorScript}/bin/jotain-editor";
      VISUAL = "${lib.getBin visualScript}/bin/jotain-visual";
    };

    programs.bash.shellAliases = lib.mkIf cfg.shellAliases.enable shellAliasMap;
    programs.zsh.shellAliases = lib.mkIf cfg.shellAliases.enable shellAliasMap;
    programs.fish.shellAliases = lib.mkIf cfg.shellAliases.enable shellAliasMap;

    fonts.fontconfig.enable = lib.mkIf isLinux true;

    home.packages = [
      selectedPackage
      editorScript
      visualScript
      jotctlScript
      # hiPrio so the wrapped `emacs` shadows the unwrapped binary
      # that ships inside the selected package.
      (lib.hiPrio emacsWrapper)
    ]
    ++ cfg.spell.dictionaries
    ++ emojiFontPackages
    ++ iconFontPackages
    ++ lib.optional (cfg.client.enable && pkgs.stdenv.isLinux) (lib.hiPrio clientDesktopItem);

    # Install the Jotain Emacs configuration into ~/.config/emacs so the
    # daemon picks up early-init.el, init.el, the lisp/ modules, and the
    # tempel snippet templates (lisp/init-snippets.el resolves
    # `tempel-path' against user-emacs-directory). lisp/ and the two
    # entry files come from compiledConfig, so the daemon loads .elc
    # (with the .el kept alongside); the .elc entries for early-init and
    # init are separate because repointing only "emacs/lisp" would leave
    # the entry files interpreted.
    #
    # The .el entry files must come from compiledConfig too, not from
    # ./early-init.el and ./init.el: those are *different* store paths,
    # and a native-compiled .eln is named after a hash of its source
    # path. Serving the sources from anywhere other than where the AOT
    # step compiled them would give a permanent .eln miss. In practice
    # this matters for init.el; early-init.el's AOT .eln is structurally
    # unreachable regardless — its eln lookup happens in Fload *before*
    # early-init.el runs, i.e. before the code that adds JOTAIN_ELN_PATH
    # to native-comp-eln-load-path has executed, so early-init always
    # loads its .elc (a known Emacs limitation, same reason its JIT eln
    # in var/eln-cache never loads either). Serving it from
    # compiledConfig is still right: one source of truth, and find-file
    # agreement with the .elc next to it. (lisp/ was already correct —
    # it is a single symlink to ${compiledConfig}/lisp.)
    xdg.configFile = {
      "emacs/early-init.el".source = "${compiledConfig}/early-init.el";
      "emacs/early-init.elc".source = "${compiledConfig}/early-init.elc";
      "emacs/init.el".source = "${compiledConfig}/init.el";
      "emacs/init.elc".source = "${compiledConfig}/init.elc";
      "emacs/lisp".source = "${compiledConfig}/lisp";
      "emacs/templates".source = ./templates;
    }
    // lib.optionalAttrs cfg.openrouter.enable {
      # OpenRouter provider for the eca server (lisp/init-ai.el). The key is
      # read from $OPENROUTER_API_KEY at runtime via eca's ${env:…} syntax,
      # so no secret is written to the store.
      "eca/config.json".source = ./config/eca/config.json;
    };

    systemd.user.services.jotain = lib.mkIf isLinux (
      {
        Unit = {
          Description = "Jotain Emacs text editor";
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
        };
      }
      // lib.optionalAttrs startWithSession {
        Install = {
          WantedBy = [ systemdWantedBy ];
        };
      }
    );

    systemd.user.sockets.jotain = lib.mkIf (isLinux && cfg.socketActivation.enable) {
      Unit = {
        Description = "Jotain Emacs text editor";
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
        RequiredBy = [ "jotain.service" ];
      };
    };

    launchd.agents.jotain = lib.mkIf isDarwin {
      enable = true;
      config = {
        ProgramArguments = [
          "${emacsWrapper}/bin/emacs"
          "--fg-daemon"
        ]
        ++ cfg.extraOptions;
        RunAtLoad = true;
        KeepAlive = {
          Crashed = true;
          SuccessfulExit = false;
        };
      };
    };
  };
}
