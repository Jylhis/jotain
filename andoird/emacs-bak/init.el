;;; init.el --- Emacs configuration file -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; URL: https://github.com/Jylhis/jotain
;; Version: 2.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Modern modular Emacs configuration for enhanced development experience.
;; Configuration is split into logical modules in the elisp/ directory.

;;; Code:

;; Add config directory to load path
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Store automatic customization options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'core)        ; Core Emacs settings and built-ins
(require 'ui)          ; UI and appearance


;; (require 'fonts)       ; Font configuration and management

(require 'completion)  ; Modern completion framework
;; (require 'programming) ; Programming and development tools
;; (require 'per-project) ; Thing to help with prroject specific setups
(require 'writing)     ; Org-mode and documentation
(require 'git)         ; Git and version control
(require 'help)        ; Enhanced help system
;; (require 'systems)     ; System administration tools


;; FIXME: android ctrl+space not functioning properly
(require 'android) ; Enhanced Android support




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
