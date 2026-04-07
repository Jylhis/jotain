;;; init-org.el --- Org mode and friends -*- lexical-binding: t; -*-

;;; Commentary:

;; Org gets its own file because it's an entire writing/agenda/literate-
;; programming environment, not a single feature. Treat the built-in
;; `org' the same as a third-party package — it's big enough to earn it.

;;; Code:

(use-package org
  :ensure nil
  :commands (org-mode org-capture org-agenda)
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :custom
  (org-directory               (expand-file-name "~/Documents"))
  (org-agenda-files            (list org-directory))
  (org-default-notes-file      (expand-file-name "inbox.org" org-directory))
  (org-startup-indented        t)
  (org-startup-folded          'content)
  (org-hide-emphasis-markers   t)
  (org-pretty-entities         t)
  (org-src-fontify-natively    t)
  (org-src-tab-acts-natively   t)
  (org-confirm-babel-evaluate  nil)
  (org-log-done                'time)
  (org-return-follows-link     t)
  (org-fold-catch-invisible-edits 'show-and-error)
  :config
  ;; Capture templates: keep the list short and obvious. Add more here
  ;; rather than scattering them across other modules.
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %U\n  %a")
          ("n" "Note" entry
           (file+headline org-default-notes-file "Notes")
           "* %?\n  %U"))))

;; Time tracking with persistence across restarts.
(use-package org-clock
  :ensure nil
  :after org
  :custom
  (org-clock-persist t)
  (org-clock-idle-time 15)
  (org-clock-into-drawer t)
  :config
  (org-clock-persistence-insinuate))

;; Export org-clock data to CSV for analysis (Pandas, spreadsheets…).
(use-package org-clock-csv
  :after org
  :defer t)

;; Auto-show emphasis markers (* _ / ~) when point is on them, hide
;; otherwise — best of both visual worlds.
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(use-package org-modern
  :hook ((org-mode            . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(use-package org-roam
  :commands (org-roam-node-find org-roam-capture)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture))
  :custom
  (org-roam-directory (expand-file-name "org-roam/" org-directory))
  :config
  (org-roam-db-autosync-mode 1))

(provide 'init-org)
;;; init-org.el ends here
