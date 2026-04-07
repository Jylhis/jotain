;;; jotain-performance.el --- Performance tweaks and ergonomic hacks -*- lexical-binding:t; -*-

;; Author: Markus Jylhänkangas
;; URL: https://github.com/jylhis/jotain

;;; Commentary:

;; A grab-bag of small performance and ergonomic adjustments cribbed
;; from popular Emacs configurations (Doom, Purcell, Prot, Centaur).
;; Each setting is documented with its origin and rationale.
;;
;; Source:
;; https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/

;;; Code:

;;;; Performance

;; Disable bidirectional text scanning.  Doom Emacs ships this
;; unconditionally — Emacs runs the bidirectional parenthesis algorithm
;; on every redisplay otherwise, which adds noticeable cost in large
;; files (multi-thousand-line JSON, log files).  Safe assuming the user
;; does not edit right-to-left scripts.
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setopt bidi-inhibit-bpa t)

;; Defer fontification while the user is actively typing.  Removes
;; micro-stutters in tree-sitter modes and large buffers; the highlight
;; catches up the moment input pauses.
(setopt redisplay-skip-fontification-on-input t)

;; Modern LSP servers (rust-analyzer, clangd, …) routinely send
;; multi-megabyte responses.  Bumping the read buffer cuts the number
;; of read calls dramatically.  4 MB is what Doom, Purcell, and Centaur
;; all ship.
(setopt read-process-output-max (* 4 1024 1024))

;; Don't draw cursors or highlight selections in non-focused windows.
;; Saves a small amount of rendering work and removes a visual
;; distraction in multi-window layouts.
(setq-default cursor-in-non-selected-windows nil)
(setopt highlight-nonselected-windows nil)

(provide 'jotain-performance)
;;; jotain-performance.el ends here
