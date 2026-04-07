;;; init-help.el --- Help system upgrades -*- lexical-binding: t; -*-

;;; Commentary:

;; Built-in help tweaks plus `helpful' \u2014 the modern replacement for the
;; default `describe-*' commands. Helpful's buffers show source, callers,
;; usage examples, and active keybindings inline.

;;; Code:

(use-package help
  :ensure nil
  :custom
  ;; Auto-focus the help window so you can scroll/dismiss without
  ;; reaching for the mouse.
  (help-window-select t))

(use-package help-at-pt
  :ensure nil
  :custom
  ;; Echo-area tooltips on buttons/links when point lingers.
  (help-at-pt-display-when-idle t))

(use-package apropos
  :ensure nil
  :custom
  ;; Search every symbol type, not just bound functions/variables.
  (apropos-do-all t))

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
