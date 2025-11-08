;;; jotain-completion-marginalia.el --- Rich annotations for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Marginalia adds rich annotations to minibuffer completions,
;; showing helpful context like file permissions, function signatures, etc.

;;; Code:

(use-package marginalia
  :demand t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :config
  (marginalia-mode))

(provide 'jotain-completion-marginalia)
;;; jotain-completion-marginalia.el ends here
