;;; jotain-completion-vertico.el --- Vertico completion UI for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Vertico provides a performant and minimalist vertical completion UI.

;;; Code:

(use-package vertico
  :demand t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              ("<tab>" . minibuffer-complete)
              ("<escape>" . minibuffer-keyboard-quit))
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 15)
  :config
  (vertico-mode))

;; Vertico extensions
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(provide 'jotain-completion-vertico)
;;; jotain-completion-vertico.el ends here
