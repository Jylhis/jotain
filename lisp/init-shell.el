;;; init-shell.el --- Eshell, comint, ielm -*- lexical-binding: t; -*-

;;; Commentary:

;; Lisp-driven shells and REPLs, grouped so prompt and history settings
;; stay consistent.  The ghostel terminal emulator (a real PTY) lives in
;; init-terminal.el.

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

(use-package comint
  :ensure nil
  :custom
  (comint-prompt-read-only t)
  (comint-input-ignoredups t)
  (comint-scroll-to-bottom-on-input t))

(use-package ielm
  :ensure nil
  :commands ielm
  :config
  (setopt ielm-history-file-name (jotain-var-file "ielm-history.eld")))

(provide 'init-shell)
;;; init-shell.el ends here
