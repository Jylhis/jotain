;;; jotain.el --- Jotain Emacs Distribution -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas
;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience
;; URL: https://github.com/Jylhis/jotain

;;; Commentary:
;; Main entry point for the Jotain Emacs distribution.
;; Provides a modern, modular Emacs configuration with Nix integration.
;;
;; Handles development mode detection and loads core modules in order.
;; All configuration is in elisp/ directory with jotain-* prefix.

;;; Code:

(defgroup jotain nil
  "Jotain Emacs distribution."
  :group 'emacs
  :prefix "jotain-")

(defcustom jotain-dev-mode nil
  "Enable development mode for Jotain.
When non-nil, enables additional debugging features and reloads modules."
  :type 'boolean
  :group 'jotain)

(defun jotain--detect-dev-mode ()
  "Detect if running in development mode.
Checks the JOTAIN_DEV_MODE environment variable."
  (when (getenv "JOTAIN_DEV_MODE")
    (setq jotain-dev-mode t)))

(defun jotain-initialize ()
  "Initialize Jotain distribution.
Loads all modules in the correct order:
1. jotain-core - Core Emacs settings
2. jotain-editor - Editor features (completion, UI)
3. jotain-prog - Programming tools
4. jotain-tools - Git, Org, help, etc."
  (jotain--detect-dev-mode)
  (when jotain-dev-mode
    (message "Jotain: Running in development mode"))

  ;; Load modules in order
  (require 'jotain-core)
  (require 'jotain-editor)
  (require 'jotain-prog)
  (require 'jotain-tools))

;; Initialize on load
(jotain-initialize)

(provide 'jotain)
;;; jotain.el ends here
