;;; init-help.el --- Help system upgrades -*- lexical-binding: t; -*-

;;; Commentary:

;; Built-in help tweaks plus `helpful', a richer replacement for the
;; default `describe-*' commands.

;;; Code:

(use-package help
  :ensure nil
  :custom
  (help-window-select t)
  (help-window-keep-selected t)
  :config
  (setopt view-lossage-auto-refresh t))

(use-package help-at-pt
  :ensure nil
  :custom
  (help-at-pt-display-when-idle t))

(use-package apropos
  :ensure nil
  ;; Autoloaded on demand; keep it off the startup path.
  :defer t
  :custom
  (apropos-do-all t))

(use-package helpful
  :bind
  (("C-h f"   . helpful-callable)
   ("C-h v"   . helpful-variable)
   ("C-h k"   . helpful-key)
   ("C-h F"   . helpful-function)
   ("C-h C"   . helpful-command)
   ([remap display-local-help] . helpful-at-point)
   ([remap describe-symbol] . helpful-symbol)))

(provide 'init-help)
;;; init-help.el ends here
