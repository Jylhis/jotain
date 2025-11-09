;;; jotain-packages.el --- Package management for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Sets up use-package and configures package management.
;; In Jotain, packages are managed by Nix, so we configure use-package
;; to work with Nix-provided packages.

;;; Code:

(message "Loading jotain packages")
(require 'use-package)

;; Configure use-package
(setopt use-package-always-ensure nil)  ; Don't auto-install - Nix manages packages
;; (setopt      use-package-always-defer nil)   ; We'll explicitly defer where needed
(setopt      use-package-expand-minimally nil)
(setopt      use-package-compute-statistics nil)
(setopt      use-package-verbose nil)

;; Enable :diminish keyword support if diminish is available
;; (when (locate-library "diminish")
  ;; (require 'diminish nil t))

;; Enable :bind keyword support (built-in to use-package)
;; (setq use-package-hook-name-suffix nil)

(provide 'jotain-packages)
;;; jotain-packages.el ends here
