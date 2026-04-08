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
#   };
#
# Modelled after the home-manager services.emacs module, but uses the
# Jotain-built Emacs and `jotain` naming throughout.
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.jotain;

  emacsBinPath = "${cfg.package}/bin";
  emacsVersion = lib.getVersion cfg.package;

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

  # Wrapper script that launches emacsclient with a sensible default
  # (--create-frame) when invoked without arguments.
  editorScript = pkgs.writeShellScriptBin "jotain-editor" ''
    exec ${lib.getBin cfg.package}/bin/emacsclient "''${@:---create-frame}"
  '';
in
{
  options.services.jotain = {
    enable = lib.mkEnableOption "the Jotain Emacs daemon";

    package = lib.mkOption {
      type = lib.types.package;
      default = import ./default.nix { inherit pkgs; };
      defaultText = lib.literalExpression "import ./default.nix { inherit pkgs; }";
      description = "The Jotain Emacs package to use.";
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
      default = false;
      example = true;
      description = ''
        Whether to configure {command}`emacsclient` as the default
        editor using the {env}`EDITOR` and {env}`VISUAL`
        environment variables.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.sessionVariables = lib.mkIf cfg.defaultEditor {
      EDITOR = "${lib.getBin editorScript}/bin/jotain-editor";
      VISUAL = "${lib.getBin editorScript}/bin/jotain-editor";
    };

    home.packages = [
      cfg.package
      editorScript
    ]
    ++ lib.optional (cfg.client.enable && pkgs.stdenv.isLinux) (lib.hiPrio clientDesktopItem);

    systemd.user.services.jotain = {
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
        ExecStart = ''${pkgs.runtimeShell} -l -c "${emacsBinPath}/emacs --fg-daemon${lib.optionalString cfg.socketActivation.enable "=${lib.escapeShellArg socketPath}"} ${lib.escapeShellArgs cfg.extraOptions}"'';

        # Emacs exits with status 15 after SIGTERM.
        SuccessExitStatus = 15;

        Restart = "on-failure";
      }
      // lib.optionalAttrs needsSocketWorkaround {
        ExecStartPost = "${pkgs.coreutils}/bin/chmod --changes -w ${socketDir}";
        ExecStopPost = "${pkgs.coreutils}/bin/chmod --changes +w ${socketDir}";
      };
    }
    // lib.optionalAttrs (cfg.startWithUserSession != false) {
      Install = {
        WantedBy = [
          (if cfg.startWithUserSession == true then "default.target" else "graphical-session.target")
        ];
      };
    };

    systemd.user.sockets.jotain = lib.mkIf cfg.socketActivation.enable {
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

    launchd.agents.jotain = {
      enable = true;
      config = {
        ProgramArguments = [
          "${cfg.package}/bin/emacs"
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
