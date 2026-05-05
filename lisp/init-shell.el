;;; init-shell.el --- Eshell, vterm, comint -*- lexical-binding: t; -*-

;;; Commentary:

;; Shells are inherently a separate concern from prog-mode editing — they
;; have their own input handling, history, prompts, and rendering. Group
;; them together so the rules ("how should the prompt look", "how does
;; history search work") can be coordinated.

;;; Code:

;;; @doc Built-in Lisp-driven shell — works the same on every platform
;;; @doc and is the right tool for Emacs-flavoured pipelines (commands
;;; @doc as Elisp functions, no subprocess for builtins).
(use-package eshell
  :ensure nil
  :commands (eshell)
  :custom
  (eshell-history-size 10000)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-error-if-no-glob t)
  (eshell-destroy-buffer-when-process-dies t))

;;; @doc Real terminal emulator (libvterm). Use this when you need a
;;; @doc tmux session, ncurses programs, or anything that wants a true
;;; @doc PTY — eshell can't do those.
(use-package vterm
  :commands (vterm vterm-other-window)
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm: %s"))

;;; @doc Built-in REPL substrate (used by python, ielm, sql, etc.).
;;; @doc The settings here apply to every comint-derived buffer.
(use-package comint
  :ensure nil
  :custom
  (comint-prompt-read-only t)
  (comint-input-ignoredups t)
  (comint-scroll-to-bottom-on-input t))

(provide 'init-shell)
;;; init-shell.el ends here
