;;; init-casual.el --- Transient menus for built-in tools (casual) -*- lexical-binding: t; -*-

;;; Commentary:

;; casual layers discoverable Transient menus over built-in Emacs tools
;; (dired, calc, isearch, ibuffer, Info).  The whole suite is one
;; package, so it lives in one file rather than being split across the
;; modules for each built-in it decorates.  transient (via magit) and
;; which-key are already loaded, so casual adds no new core dependency.
;;
;; Menus are opt-in and mode-local: each is bound to a key in its own
;; mode map (nothing global), through `with-eval-after-load' so each
;; binding defers independently of the others — calc, for one, is rarely
;; loaded, and a single `:after (dired calc …)' would gate the common
;; menus on the rare one.

;;; Code:

;; Keymaps referenced inside the `with-eval-after-load' forms below;
;; none of these libraries is loaded in a clean per-file compile
;; session (nix/config-compiled-split.nix), so declare the variables.
;; isearch-mode-map needs no stub — isearch.el is preloaded.
(defvar dired-mode-map)
(defvar calc-mode-map)
(defvar Info-mode-map)
(defvar ibuffer-mode-map)

;;; @doc Discoverable Transient menus over built-in tools: dired, calc,
;;; isearch, ibuffer, and Info.  Each menu is bound to `C-o' in its own
;;; mode map (`<f2>' for isearch, whose `C-o' is already meaningful), so
;;; the menus stay out of the global keymap and act on the current mode.
;;; Builds on the transient library magit already pulls in.
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
