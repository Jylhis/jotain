;;; init-casual.el --- Transient menus for built-in tools (casual) -*- lexical-binding: t; -*-

;;; Commentary:

;; casual is one package covering several built-ins, so it gets one file
;; rather than a block in each built-in's module.
;;
;; Each menu is bound in its own mode map (nothing global) through a
;; separate `with-eval-after-load': a single `:after (dired calc ...)'
;; would gate the common menus on rarely loaded calc.

;;; Code:

;;; @doc Discoverable Transient menus over built-in tools: dired, calc,
;;; isearch, ibuffer, and Info.  Each menu is bound to `C-o' in its own
;;; mode map (`<f2>' in isearch, whose `C-o' is already taken).
(use-package casual
  :commands (casual-dired-tmenu
             casual-calc-tmenu
             casual-info-tmenu
             casual-ibuffer-tmenu
             casual-isearch-tmenu)
  :init
  (with-eval-after-load 'dired
    (keymap-set dired-mode-map "C-o" #'casual-dired-tmenu))
  (with-eval-after-load 'calc
    (keymap-set calc-mode-map "C-o" #'casual-calc-tmenu))
  (with-eval-after-load 'info
    (keymap-set Info-mode-map "C-o" #'casual-info-tmenu))
  (with-eval-after-load 'ibuffer
    (keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu))
  (with-eval-after-load 'isearch
    (keymap-set isearch-mode-map "<f2>" #'casual-isearch-tmenu)))

(provide 'init-casual)
;;; init-casual.el ends here
