;;; jotain-editor-core.el --- Core editor functionality for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Core editor functionality including useful editing commands,
;; multiple cursors, better undo, etc.

;;; Code:

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(provide 'jotain-editor-core)
;;; jotain-editor-core.el ends here
