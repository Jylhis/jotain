{ config, lib, pkgs, ... }:

let
  cfg = config.programs.jotain;

in
{
  options.programs.jotain = {
    enable = lib.mkEnableOption "Jotain Emacs distribution";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.jotain;
      defaultText = lib.literalExpression "pkgs.jotain";
      description = "The Jotain package to use";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    # Set Emacs as default editor
    home.sessionVariables = {
      EDITOR = "emacs -nw";
      VISUAL = "emacs";
    };

    # XDG configuration
    xdg.configFile."emacs/early-init.el".source = "${cfg.package}/share/jotain/early-init.el";
    xdg.configFile."emacs/init.el".text = ''
      ;;; init.el --- Jotain Emacs configuration (Nix deployment) -*- lexical-binding: t; -*-

      ;; Add Jotain elisp directory to load path
      (add-to-list 'load-path "${cfg.package}/share/emacs/site-lisp/jotain")

      ;; Load platform detection first
      (require 'platform)

      ;; Ensure package system is available
      (require 'package)
      (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
      (package-initialize)

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
