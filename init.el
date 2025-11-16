;;; init.el --- Jotain Emacs initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas
;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; URL: https://github.com/Jylhis/jotain
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;; Minimal init.el that sets up the load path and loads the Jotain
;; distribution.  The real configuration is in elisp/jotain.el.

;;; Code:

;; Add elisp directory to load path
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Load Jotain distribution
(require 'jotain)

(provide 'init)
;;; init.el ends here
