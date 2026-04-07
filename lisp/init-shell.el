;;; init-shell.el --- Eshell, vterm, comint -*- lexical-binding: t; -*-

;;; Commentary:

;; Shells are inherently a separate concern from prog-mode editing — they
;; have their own input handling, history, prompts, and rendering. Group
;; them together so the rules ("how should the prompt look", "how does
;; history search work") can be coordinated.

;;; Code:

(use-package eshell
  :ensure nil
  :commands (eshell)
  :custom
  (eshell-history-size 10000)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-error-if-no-glob t)
  (eshell-destroy-buffer-when-process-dies t))

(use-package vterm
  :commands (vterm vterm-other-window)
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm: %s"))

(use-package comint
  :ensure nil
  :custom
  (comint-prompt-read-only t)
  (comint-input-ignoredups t)
  (comint-scroll-to-bottom-on-input t))

(provide 'init-shell)
;;; init-shell.el ends here
