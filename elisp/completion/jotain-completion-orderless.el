;;; jotain-completion-orderless.el --- Orderless completion for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Orderless completion style - allows matching in any order.
;; Must be loaded before Vertico for optimal integration.

;;; Code:

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space))

(provide 'jotain-completion-orderless)
;;; jotain-completion-orderless.el ends here
