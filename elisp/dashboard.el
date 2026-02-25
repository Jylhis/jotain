;;; dashboard.el --- Startup dashboard configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Startup screen configuration using Enlight.

;;; Code:

(defun jotain-dashboard-initial-buffer ()
  "Return enlight dashboard unless file-visiting buffers already exist.
When Emacs is launched with file arguments, those buffers are
visited before this function runs, so we skip the dashboard."
  (let ((file-buffer (seq-find #'buffer-file-name (buffer-list))))
    (if file-buffer
        file-buffer
      (enlight))))

(use-package enlight
  :ensure t
  :init
  (setopt initial-buffer-choice #'jotain-dashboard-initial-buffer)
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
