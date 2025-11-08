;;; jotain-completion-orderless.el --- Orderless completion for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Orderless completion style - allows matching in any order.
;; Must be loaded before Vertico for optimal integration.

;;; Code:

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless partial-completion flex basic))
  (completion-category-defaults nil)
  ;; Use partial-completion for files to support path component matching
  ;; e.g., /u/s/a matches /usr/share/applications
  (completion-category-overrides '((file (styles partial-completion orderless))
                                   (buffer (styles orderless))
                                   (project-file (styles partial-completion orderless)))))

(provide 'jotain-completion-orderless)
;;; jotain-completion-orderless.el ends here
