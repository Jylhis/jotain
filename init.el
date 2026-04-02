;9u;;; Init.el --- Jotain Emacs Config -*- lexical-binding:t; -*-


;;; Commentary:

;;; Code:

(message "Start of Jotain")

;; Store automatic customization options elsewhere
;; TODO: Where should this go?
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;;(load-theme 'modus-operandi-tritanopia t)
(load-theme 'modus-vivendi-tritanopia t)

(use-package emacs
  :custom
  (fill-column 100)
  (use-short-answers t)
  (use-dialog-boxes nil)
  (delete-by-moving-to-trash t)
  :bind
  (("C-z" . nil)
   ("C-x C-z" . nil)
   ("M-o" . other-window))
  )

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(provide 'init)
;;; init.el ends here
