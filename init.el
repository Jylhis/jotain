;;; init.el --- Jotain Emacs initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Minimal init.el that sets up the load path and loads the Jotain
;; distribution.  The real configuration is in elisp/jotain.el.

;;; Code:
(message "Start init")

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(require 'jotain)

(provide 'init)
;;; init.el ends here
