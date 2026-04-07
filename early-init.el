;;; early-init.el --- Early Emacs initialization -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>

;;; Commentary:
;; Early initialization file for Emacs 30+.
;; This file is loaded before package initialization and GUI setup.

;;; Code:

(message "Early start of Jotain")

;; Defer GC during startup, then restore a sane threshold.  Shaves real
;; time off init by avoiding GC runs while the bulk of bytecode is loaded.
(setq gc-cons-threshold most-positive-fixnum)

(defun jotain--restore-gc-threshold ()
  "Restore `gc-cons-threshold' to a normal value after startup."
  (setq gc-cons-threshold (* 16 1024 1024)))

(add-hook 'emacs-startup-hook #'jotain--restore-gc-threshold)

;; `init.el' calls `package-initialize' explicitly, so suppress the
;; automatic init that would otherwise run between early-init and init —
;; without this, package.el initializes twice.
(setq package-enable-at-startup nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setopt load-prefer-newer noninteractive)

(setopt use-package-enable-imenu-support t)

(tool-bar-mode -1)

(setopt inhibit-startup-screen t)

(provide 'early-init)
;;; early-init.el ends here
