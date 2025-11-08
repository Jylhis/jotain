;;; jotain-ui-modeline.el --- Modeline configuration for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Modeline configuration using doom-modeline for a modern appearance.

;;; Code:


(add-hook 'after-init-hook #'doom-modeline-mode)

(use-package doom-modeline)

;; Icons support for doom-modeline
(use-package nerd-icons
  :defer t)

(provide 'jotain-ui-modeline)
;;; jotain-ui-modeline.el ends here
