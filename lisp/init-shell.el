;;; init-shell.el --- Eshell, comint, ielm -*- lexical-binding: t; -*-

;;; Commentary:

;; Shells are inherently a separate concern from prog-mode editing — they
;; have their own input handling, history, prompts, and rendering. Group
;; them together so the rules ("how should the prompt look", "how does
;; history search work") can be coordinated.
;;
;; The ghostel terminal emulator (a true PTY, unlike these Lisp-driven
;; shells and REPLs) lives in init-terminal.el.

;;; Code:

;;; @doc Built-in Lisp-driven shell — works the same on every platform
;;; and is the right tool for Emacs-flavoured pipelines (commands
;;; as Elisp functions, no subprocess for builtins).
(use-package eshell
  :ensure nil
  :commands (eshell)
  :custom
  (eshell-history-size 10000)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-error-if-no-glob t)
  (eshell-destroy-buffer-when-process-dies t))

;;; @doc Built-in REPL substrate (used by python, ielm, sql, etc.).
;;; The settings here apply to every comint-derived buffer.
(use-package comint
  :ensure nil
  :custom
  (comint-prompt-read-only t)
  (comint-input-ignoredups t)
  (comint-scroll-to-bottom-on-input t))

;;; @doc Built-in Emacs Lisp REPL. Emacs 31+ can persist input history
;;; across sessions like comint/shell already do; point it at a file
;;; under `var/'. Guarded so the config loads on Emacs 30.
(use-package ielm
  :ensure nil
  :commands ielm
  :config
  (when (boundp 'ielm-history-file-name)
    (setopt ielm-history-file-name (jotain-var-file "ielm-history.eld"))))

(provide 'init-shell)
;;; init-shell.el ends here
