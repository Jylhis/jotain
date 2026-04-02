;;; early-init.el --- Early Emacs initialization -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>

;;; Commentary:
;; Early initialization file for Emacs 30+.
;; This file is loaded before package initialization and GUI setup.

;;; Code:

(message "Early start of Jotain")
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setopt load-prefer-newer noninteractive)

(setopt use-package-enable-imenu-support t)

(tool-bar-mode -1)

(setopt inhibit-startup-screen t)

(provide 'early-init)
;;; early-init.el ends here
