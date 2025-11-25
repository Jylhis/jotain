{ config, lib, pkgs, ... }:

let
  cfg = config.programs.jotain;

  # Use jotainEmacs from overlay (which handles emacs.nix and dependencies)
  jotainEmacs = pkgs.jotainEmacs;

  # Get runtime dependencies directly from passthru (exposed by emacs.nix)
  # Provide defaults in case they're not available
  lspServers = jotainEmacs.lspServers or [ ];
  cliTools = jotainEmacs.cliTools or [ ];
  fonts = jotainEmacs.fonts or [ ];
  treeSitterGrammars = jotainEmacs.treeSitterGrammars or [ ];
  allRuntimeDeps = jotainEmacs.allRuntimeDeps or [ ];

in
{
  options.programs.jotain = {
    enable = lib.mkEnableOption "Jotain Emacs distribution";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.jotain;
      defaultText = lib.literalExpression "pkgs.jotain";
      description = "The Jotain configuration package to use";
    };

    extraPackages = lib.mkOption {
      type = lib.types.functionTo (lib.types.listOf lib.types.package);
      default = _: [ ];
      defaultText = lib.literalExpression "epkgs: [ ]";
      description = "Extra packages to install alongside Jotain Emacs";
    };

    includeRuntimeDeps = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Include runtime dependencies (LSP servers, CLI tools, fonts) in the user environment.

        When enabled, this option installs:
        - LSP servers (gopls, nil, etc.)
        - CLI tools (ripgrep, fd)
        - Nerd Fonts and other font packages

        These dependencies are exposed via the Emacs package's passthru.runtimeDeps attribute
        and are made available in home.packages and properly configured for fontconfig.

        Set to false if you prefer to manage these dependencies separately or don't need them.
      '';
    };

    enableDaemon = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Enable Emacs daemon and configure emacsclient for editor commands.

        When enabled:
        - Starts Emacs as a systemd user service (Linux) or launchd service (macOS)
        - Sets EDITOR/VISUAL environment variables to use emacsclient
        - Enables socket activation for faster startup (Linux only)
        - Creates desktop entry for emacsclient

        When disabled, Emacs runs as a regular application without daemon mode.
        This is useful for debugging, development, or platforms without systemd support.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # Install Emacs via the programs.emacs module
    programs.emacs = {
      enable = true;
      package = jotainEmacs;
      extraPackages = cfg.extraPackages;
    };

    # Configure Emacs daemon service when enabled
    services.emacs = lib.mkIf cfg.enableDaemon {
      enable = true;
      # package is inherited from programs.emacs.package automatically
      socketActivation.enable = pkgs.stdenv.isLinux;
      client = {
        enable = true; # Only create desktop entry when daemon is enabled
        arguments = [ "-c" "-a" "" ];
      };
    };

    # Install configuration files and runtime dependencies
    home.packages = [ cfg.package ]
      ++ lib.optionals cfg.includeRuntimeDeps (
      lspServers
        ++ cliTools
        ++ fonts
    );

    # Configure fonts for fontconfig when runtime deps are included
    fonts.fontconfig.enable = lib.mkIf cfg.includeRuntimeDeps true;

    # Set Emacs as default editor
    # Use emacsclient when daemon is enabled, direct emacs otherwise
    home.sessionVariables = lib.mkMerge [
      # When daemon is disabled, use direct emacs
      (lib.mkIf (!cfg.enableDaemon) {
        EDITOR = "emacs -nw";
        VISUAL = "emacs";
      })
      # When daemon is enabled, use emacsclient
      (lib.mkIf cfg.enableDaemon {
        EDITOR = "emacsclient -t -a ''";
        VISUAL = "emacsclient -c -a ''";
      })
    ];

    # XDG configuration
    xdg.configFile."emacs/early-init.el".source = "${cfg.package}/share/jotain/early-init.el";
    xdg.configFile."emacs/init.el".text = ''
      ;;; init.el --- Jotain Emacs configuration (Nix deployment) -*- lexical-binding: t; -*-

      ;; Add Jotain elisp directory to load path
      (add-to-list 'load-path "${cfg.package}/share/emacs/site-lisp/jotain")

      ;; Load platform detection first
      (require 'platform)

      ;; Package.el provides metadata only - packages are pre-installed by Nix
      (require 'package)
      (setq package-archives nil)           ; Disable archives BEFORE initialization
      (setq package-archive-priorities nil) ; Disable archive priorities
      (setq package-vc-heuristic-alist nil) ; Disable VC package detection (Emacs 29+)
      (package-initialize)                  ; Initialize with no remote archives

      ;; Store automatic customization options elsewhere
      (setq custom-file (locate-user-emacs-file "custom.el"))
      (when (file-exists-p custom-file) (load custom-file))

      ;; Load configuration modules
      (require 'core)        ; Core Emacs settings and built-ins
      (require 'fonts)       ; Font configuration and management
      (require 'ui)          ; UI and appearance
      (require 'completion)  ; Modern completion framework
      (require 'programming) ; Programming and development tools
      (require 'per-project) ; Thing to help with project specific setups
      (require 'writing)     ; Org-mode and documentation
      (require 'git)         ; Git and version control
      (require 'help)        ; Enhanced help system
      (require 'ai)          ; AI integrations
      (require 'systems)     ; System administration tools

      ;; Load platform-specific configurations
      (require 'platforms)   ; General platform adaptations
      (when platform-android-p (require 'android)) ; Enhanced Android support

      (require 'app-launchers)

      ;; Additional GC optimizations from Doom Emacs patterns
      ;; Trigger GC when idle for 5 seconds
      (run-with-idle-timer 5 t #'garbage-collect)

      ;; Prevent GC during minibuffer operations (completion!)
      (add-hook 'minibuffer-setup-hook
                (lambda () (setq gc-cons-threshold most-positive-fixnum)))
      (add-hook 'minibuffer-exit-hook
                (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

      (provide 'init)
      ;;; init.el ends here
    '';
  };
}
