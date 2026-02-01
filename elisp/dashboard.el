;;; dashboard.el --- Startup dashboard configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Startup screen configuration using Enlight.

;;; Code:

(defun my-dashboard/open-downloads ()
  "Open the user's downloads directory in dired."
  (interactive)
  (let ((downloads-dir
         (if (and (eq system-type 'gnu/linux) (require 'xdg nil t))
             (or (xdg-user-dir "DOWNLOAD") "~/Downloads")
           "~/Downloads")))
    (dired downloads-dir)))

(use-package enlight
  :ensure t
  :custom
  (initial-buffer-choice #'enlight)
  :config
  (setq enlight-content
        (concat
         (propertize "MENU" 'face 'highlight)
         "\n"
         (enlight-menu
          '(("Org Mode"
             ("Org-Agenda (current day)" (org-agenda nil "a") "a"))
            ("Downloads"
             ("Downloads folder" my-dashboard/open-downloads "d"))
            ("Other"
             ("Projects" project-switch-project "p")))))))

(provide 'dashboard)
;;; dashboard.el ends here
