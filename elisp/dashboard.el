;;; dashboard.el --- Startup dashboard configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Startup screen configuration using Enlight.

;;; Code:

(defun jotain-dashboard-initial-buffer ()
  "Return enlight dashboard unless file-visiting buffers already exist.
When Emacs is launched with file arguments, those buffers are
visited before this function runs, so we skip the dashboard."
  ;; Optimization: Use `or` with `seq-find` to avoid traversing the buffer list
  ;; twice (previously `seq-some` followed by `seq-find`).
  (or (seq-find (lambda (b)
                  (or (buffer-file-name b)
                      (with-current-buffer b (bound-and-true-p dired-directory))))
                (buffer-list))
      (enlight)))

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
