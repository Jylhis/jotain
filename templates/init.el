;;; init.el --- Jotain Emacs initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Minimal init.el that sets up the load path and loads the Jotain
;; distribution.  The real configuration is in elisp/jotain.el.

;;; Code:

;; Add Jotain elisp directory to load path
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/ui" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/completion" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/editor" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/programming" user-emacs-directory))

;; Load Jotain
(require 'jotain)

(provide 'init)
;;; init.el ends here
