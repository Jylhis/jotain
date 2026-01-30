;;; dashboard.el --- Startup dashboard configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Startup screen configuration using Enlight.

;;; Code:

(use-package enlight
  :ensure t
  :custom
  (initial-buffer-choice #'enlight)
  (enlight-content
   (concat
    (propertize "MENU" 'face 'highlight)
    "\n"
    (enlight-menu
     '(("Org Mode"
        ("Org-Agenda (current day)" (org-agenda nil "a") "a"))
       ("Downloads"
        ("Downloads folder" (dired "~/Downloads") "a"))
       ("Other"
        ("Projects" project-switch-project "p")))))))

(provide 'dashboard)
;;; dashboard.el ends here
