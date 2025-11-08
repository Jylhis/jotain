;;; jotain-completion-vertico.el --- Vertico completion UI for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Vertico provides a performant and minimalist vertical completion UI.

;;; Code:

(use-package vertico
  :demand t)

;; Vertico extensions
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; (use-package vertico-multiform
;;   :requires vertico
;;   :ensure nil
;;   :custom
;;   (vertico-multiform-categories
;;    '((file buffer grid)
;;      (imenu (:not indexed mouse))
;;      (symbol (vertico-sort-function . vertico-sort-alpha))))
;;   (vertico-multiform-commands
;;    '((consult-line buffer)
;;      (consult-git-grep buffer)
;;      (consult-ripgrep buffer)
;;      (consult-grep buffer)
;;      (consult-fd grid)
;;      (execute-extended-command 'vertical))
;;    )
;;   :config
;;   (vertico-multiform-mode 1))

(use-package vertico-buffer
  :after vertico
  :ensure nil
  :custom
  (vertico-buffer-hide-prompt nil)
  (vertico-buffer-display-action '(display-buffer-reuse-window)))

(provide 'jotain-completion-vertico)
;;; jotain-completion-vertico.el ends here
