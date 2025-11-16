;;; jotain-prog.el --- Programming tools and languages -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas
;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; Keywords: convenience
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;; Programming configuration including:
;; - Tree-sitter and syntax highlighting
;; - LSP support via Eglot
;; - Flymake linting
;; - Essential language modes
;; - Debugging tools

;;; Code:

;;; Basic Programming Setup

(use-package prog-mode
  :hook (prog-mode . display-line-numbers-mode))

(use-package treesit
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install nil)
  :init
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; Linting and Checking

(use-package flymake
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)))

(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :bind (:map flyspell-mode-map
              ("C-M-i" . nil)))

;;; LSP Support

(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode)
                          (eglot-ensure))))
         (markdown-mode . eglot-ensure))
  :custom
  (eglot-send-changes-idle-time 0.5)
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-report-progress nil)
  (eglot-extend-to-xref t)
  :bind (:map eglot-mode-map
              ("C-c r r" . eglot-rename)
              ("C-c r f" . eglot-format)
              ("C-c r a" . eglot-code-actions)
              ("C-h ." . eldoc-doc-buffer))
  :config
  (fset #'jsonrpc--log-event #'ignore))

(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

(use-package xref
  :init
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep)))

;;; Development Tools

(use-package dtrt-indent
  :ensure t
  :diminish
  :hook (prog-mode . dtrt-indent-mode))

(use-package editorconfig
  :ensure t
  :diminish
  :hook (prog-mode . editorconfig-mode))

(use-package direnv
  :if (executable-find "direnv")
  :ensure t
  :init
  (direnv-mode)
  :config
  (add-to-list 'warning-suppress-types '(direnv)))

(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;;; Debugging

(use-package gdb-mi
  :custom
  (gdb-many-windows t)
  (gdb-show-main t))

(use-package dape
  :ensure t
  :defer t
  :custom
  (dape-buffer-window-arrangement 'gud)
  (dape-inlay-hints t))

;;; Major Modes - Essential Languages

;; System languages
(use-package cc-mode
  :custom
  (c-default-style '((c-mode . "stroustrup")
                     (c++-mode . "stroustrup")))
  :mode (("\\.h\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)))

(use-package go-mode :ensure t)
(use-package haskell-mode :ensure t)

;; Scripting languages
(use-package sh-mode
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)))

;; Nix
(use-package nix-mode :ensure t)
(use-package nix-ts-mode :ensure t :mode "\\.nix\\'")

;; Markdown and documentation
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown")
  :hook (markdown-mode . visual-line-mode))

;; Configuration formats
(use-package yaml-mode :ensure t)
(use-package conf-mode
  :mode (("/.dockerignore\\'" . conf-unix-mode)
         ("/.gitignore\\'" . conf-unix-mode)))

;; Build tools
(use-package cmake-mode
  :ensure t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package just-mode :ensure t)

;; Infrastructure as code
(use-package dockerfile-mode :ensure t)
(use-package terraform-mode :ensure t)

;; Web development
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

;;; Terminal

(use-package vterm
  :ensure t
  :defer t
  :custom
  (vterm-always-compile-module t))

(use-package xt-mouse
  :custom
  (xterm-mouse-mode 1))

(provide 'jotain-prog)
;;; jotain-prog.el ends here
