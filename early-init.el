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

;; Ghostty advertises TERM=xterm-ghostty but Emacs doesn't ship a matching
;; term/xterm-ghostty.el, so it falls back to a minimal terminal setup and
;; misses modifyOtherKeys, bracketed paste, focus events, and 24-bit color.
;; Aliasing to xterm-256color makes term/xterm.el load instead.
;; Set here (not in init.el) because `tty-run-terminal-initialization' runs
;; before init.el is loaded for `emacs -nw'.
(add-to-list 'term-file-aliases '("xterm-ghostty" . "xterm-256color"))

(provide 'early-init)
;;; early-init.el ends here
