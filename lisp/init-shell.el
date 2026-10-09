;;; init-shell.el --- Eshell, comint, ielm -*- lexical-binding: t; -*-

;;; Commentary:

;; Lisp-driven shells and REPLs, grouped so prompt and history settings
;; stay consistent.  The ghostel terminal emulator (a real PTY) lives in
;; init-terminal.el.

;;; Code:

;;; @doc Built-in Lisp-driven shell: the same on every platform, with
;;; Elisp functions as commands and no subprocess for builtins.
(use-package eshell
  :ensure nil
  :commands (eshell)
  :custom
  (eshell-history-size 10000)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-error-if-no-glob t)
  (eshell-destroy-buffer-when-process-dies t))

;;; @doc Built-in REPL substrate (python, ielm, sql, ...). These
;;; settings apply to every comint-derived buffer.
(use-package comint
  :ensure nil
  :custom
  (comint-prompt-read-only t)
  (comint-input-ignoredups t)
  (comint-scroll-to-bottom-on-input t))

;;; @doc Built-in Emacs Lisp REPL. On Emacs 31+ its input history
;;; persists to a file under `var/'.
(use-package ielm
  :ensure nil
  :commands ielm
  :config
  (when (boundp 'ielm-history-file-name)
    (setopt ielm-history-file-name (jotain-var-file "ielm-history.eld"))))

(provide 'init-shell)
;;; init-shell.el ends here
