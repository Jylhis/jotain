;;; jotain-programming-elisp.el --- Emacs Lisp programming support for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Configuration for Emacs Lisp development including debugging,
;; linting, and enhanced editing features.

;;; Code:

;; Elisp mode enhancements
(use-package elisp-mode
  :ensure nil
  :hook ((emacs-lisp-mode . eldoc-mode)
         (emacs-lisp-mode . flymake-mode))
  :config
  ;; Better indentation
  (setq lisp-indent-offset 2)
  
  ;; Enable checkdoc for docstring linting
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local fill-column 80)
              (setq-local tab-width 2))))

;; Helpful - better help buffers
(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)
         ("C-c C-d" . helpful-at-point)))

;; Elisp REPL improvements
(use-package ielm
  :ensure nil
  :hook (ielm-mode . eldoc-mode))

;; Package linting
(use-package package-lint
  :defer t
  :commands (package-lint-current-buffer))

;; Macrostep - interactive macro expansion
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
              ("C-c e" . macrostep-expand)))

;; Better evaluation feedback
(use-package eros
  :hook (emacs-lisp-mode . eros-mode))

;; Auto-compile elisp files
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(provide 'jotain-programming-elisp)
;;; jotain-programming-elisp.el ends here
