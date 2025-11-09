;;; jotain.el --- Jotain Emacs Distribution -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas
;; Author: Markus Jylhänkangas
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience
;; URL: https://github.com/jotain/jotain

;;; Commentary:
;; Main entry point for the Jotain Emacs distribution.  Handles development
;; mode detection, module loading, and integration with the Nix environment.

;;; Code:

(message "Load Jotain")

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
  "Initialize Jotain distribution"
  (jotain--detect-dev-mode)
    (when jotain-dev-mode
      (message "Jotain: Running in development mode"))

    (require 'jotain-core)
	(require 'jotain-packages)
	(require 'jotain-editor-core)
	(require 'jotain-prog-nix)
	(require 'jotain-tools-git)
  )

(jotain-initialize)
(provide 'jotain)
;;; jotain.el ends here
