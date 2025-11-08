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

(defconst jotain-version "0.1.0"
  "Jotain version string.")

(defgroup jotain nil
  "Jotain Emacs distribution."
  :group 'emacs
  :prefix "jotain-")

(defcustom jotain-dev-mode nil
  "Enable development mode for Jotain.
When non-nil, enables additional debugging features and reloads modules."
  :type 'boolean
  :group 'jotain)

(defvar jotain-modules '()
  "List of loaded Jotain modules.")

(defun jotain--detect-dev-mode ()
  "Detect if running in development mode.
Checks the JOTAIN_DEV_MODE environment variable."
  (when (getenv "JOTAIN_DEV_MODE")
    (setq jotain-dev-mode t)))

(defun jotain-load-module (module)
  "Load a Jotain MODULE.
MODULE should be a symbol like 'jotain-core or 'jotain-completion-vertico."
  (unless (memq module jotain-modules)
    (require module)
    (push module jotain-modules)
    (when jotain-dev-mode
      (message "Jotain: Loaded module %s" module))))

(defun jotain-reload-module (module)
  "Reload a Jotain MODULE.
Useful during development to test changes without restarting Emacs."
  (interactive
   (list (intern (completing-read "Reload module: "
                                  jotain-modules nil t))))
  (unload-feature module t)
  (setq jotain-modules (delq module jotain-modules))
  (jotain-load-module module)
  (message "Reloaded module: %s" module))

(defun jotain-initialize ()
  "Initialize the Jotain distribution.
Loads all core modules and features in the correct order."
  ;; Detect development mode
  (jotain--detect-dev-mode)
  
  (when jotain-dev-mode
    (message "Jotain: Running in development mode"))
  
  ;; Core modules - load first
  (jotain-load-module 'jotain-lib)
  (jotain-load-module 'jotain-paths)
  (jotain-load-module 'jotain-packages)
  (jotain-load-module 'jotain-gc)
  (jotain-load-module 'jotain-core)
  
  ;; UI modules
  (jotain-load-module 'jotain-ui-themes)
  (jotain-load-module 'jotain-ui-fonts)
  (jotain-load-module 'jotain-ui-modeline)
  
  ;; Completion stack - order matters
  (jotain-load-module 'jotain-completion-orderless)
  (jotain-load-module 'jotain-completion-vertico)
  (jotain-load-module 'jotain-completion-marginalia)
  (jotain-load-module 'jotain-completion-consult)
  (jotain-load-module 'jotain-completion-corfu)
  (jotain-load-module 'jotain-completion-embark)
  
  ;; Editor
  (jotain-load-module 'jotain-editor-core)
  
  ;; Programming languages
  (jotain-load-module 'jotain-programming-elisp)
  (jotain-load-module 'jotain-programming-nix)
  (jotain-load-module 'jotain-programming-shell)
  
  (when jotain-dev-mode
    (message "Jotain: Initialization complete (%d modules loaded)"
             (length jotain-modules))))

;; Initialize Jotain
(jotain-initialize)

(provide 'jotain)
;;; jotain.el ends here
