;;; Init.el --- Jotain Emacs Config -*- lexical-binding:t; -*-


;;; Commentary:

;;; Code:

(message "Start of Jotain")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

(use-package jotain-telemetry
  :custom
  (jotain-telemetry-enabled t)
  :config
  (jotain-telemetry-mode 1))

(provide 'init)
;;; init.el ends here
