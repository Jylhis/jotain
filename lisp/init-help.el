;;; init-help.el --- Help system upgrades -*- lexical-binding: t; -*-

;;; Commentary:

;; Built-in help tweaks plus `helpful' \u2014 the modern replacement for the
;; default `describe-*' commands. Helpful's buffers show source, callers,
;; usage examples, and active keybindings inline.

;;; Code:

;;; @doc Built-in help window. Auto-focus so you can scroll or dismiss
;;; with q/n/p without reaching for the mouse.
(use-package help
  :ensure nil
  :custom
  (help-window-select t))

;;; @doc Built-in echo-area tooltips on buttons and links when point
;;; lingers — discoverability for the parts of Emacs that aren't
;;; plain text.
(use-package help-at-pt
  :ensure nil
  :custom
  (help-at-pt-display-when-idle t))

;;; @doc Built-in apropos. Bumped to "search everything" so it surfaces
;;; faces, classes, and customs alongside functions and variables.
(use-package apropos
  :ensure nil
  :custom
  (apropos-do-all t))

;;; @doc Replaces the default describe-* commands with richer buffers
;;; that include source, callers, examples, and active keybindings
;;; — the single biggest discoverability upgrade in Emacs.
(use-package helpful
  :diminish
  :bind
  (("C-h f"   . helpful-callable)
   ("C-h v"   . helpful-variable)
   ("C-h k"   . helpful-key)
   ("C-h F"   . helpful-function)
   ("C-h C"   . helpful-command)
   ("C-c C-d" . helpful-at-point)
   ([remap describe-symbol] . helpful-symbol)))

(provide 'init-help)
;;; init-help.el ends here
