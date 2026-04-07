;;; Init.el --- Jotain Emacs Config -*- lexical-binding:t; -*-


;;; Commentary:

;;; Code:

(message "Start of Jotain")

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)


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

(use-package nix-ts-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package markdown-mode
  :ensure t
    :mode ("README\\.md\\'" . gfm-mode)

  )

(use-package jotain-performance)

(use-package jotain-telemetry
  :custom
  (jotain-telemetry-enabled nil))

(use-package magit
  :ensure t
  :custom
    (magit-repository-directories '(("~/Developer" . 1)))
  )

;;;; Activity tracking
;;
;; Editor instrumentation for the local-first self-tracking stack:
;; - wakatime-mode      → heartbeats to a self-hosted Wakapi
;; - activity-watch-mode → heartbeats to ActivityWatch
;; - keyfreq            → command frequency stats
;; - org-clock          → task time tracking with persistence
;; - org-clock-csv      → export org-clock data for analysis

(use-package wakatime-mode
  :ensure t
  :if (executable-find "wakatime-cli")
  :custom
  (wakatime-cli-path (executable-find "wakatime-cli"))
  :config
  (global-wakatime-mode))

(use-package activity-watch-mode
  :ensure t
  :config
  (global-activity-watch-mode))

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package org-clock
  :custom
  (org-clock-persist t)
  (org-clock-idle-time 15)
  (org-clock-into-drawer t)
  :config
  (org-clock-persistence-insinuate))

(use-package org-clock-csv
  :ensure t
  :defer t)

(provide 'init)
;;; init.el ends here
