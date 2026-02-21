;;; dashboard.el --- Startup dashboard configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Startup screen configuration using Enlight.

;;; Code:

(use-package enlight
  :ensure t
  :init
  (setopt initial-buffer-choice #'enlight)
  :custom
  (enlight-content
   (concat
    (propertize "MENU" 'face 'highlight)
    "\n"
    (enlight-menu
     '(("Org Mode"
        ("Org-Agenda (current day)" (org-agenda nil "a") "a"))
       ("Files"
        ("Find file" find-file "f")
        ("Recent files" recentf-open-files "r")
        ("Developer projects" (dired "~/Developer") "D"))
       ("Projects"
        ("Switch project" project-switch-project "p")
        ("Magit repositories" magit-list-repositories "m")))))))

(provide 'dashboard)
;;; dashboard.el ends here
