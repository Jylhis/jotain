;;; init.el --- Emacs configuration file -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; URL: https://github.com/Jylhis/jotain
;; Version: 2.0
;; Package-Requires: ((emacs "30.2"))

;;; Commentary:
;; Modern modular Emacs configuration for enhanced development experience.
;; Configuration is split into logical modules in the elisp/ directory.

;;; Code:

;; Add config directory to load path
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

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
(require 'dashboard)   ; Startup dashboard
(require 'completion)  ; Modern completion framework
(require 'programming) ; Programming and development tools
(require 'per-project) ; Thing to help with project specific setups
(require 'writing)     ; Org-mode and documentation
(require 'git)         ; Git and version control
(require 'help)        ; Enhanced help system
(require 'systems)     ; System administration tools

;; Load platform-specific configurations
(require 'platforms)   ; General platform adaptations
(when platform-android-p (require 'android)) ; Enhanced Android support



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
