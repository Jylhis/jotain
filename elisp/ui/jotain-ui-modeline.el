;;; jotain-ui-modeline.el --- Modeline configuration for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Modeline configuration using doom-modeline for a modern appearance.

;;; Code:

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 25
        doom-modeline-bar-width 4
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-minor-modes nil
        doom-modeline-enable-word-count nil
        doom-modeline-buffer-encoding nil
        doom-modeline-indent-info nil
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 12
        doom-modeline-env-version t
        doom-modeline-project-detection 'auto
        doom-modeline-lsp t
        doom-modeline-modal t
        doom-modeline-modal-icon t))

;; Icons support for doom-modeline
(use-package nerd-icons
  :defer t)

(provide 'jotain-ui-modeline)
;;; jotain-ui-modeline.el ends here
