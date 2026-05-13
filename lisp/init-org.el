;;; init-org.el --- Org mode and friends -*- lexical-binding: t; -*-

;;; Commentary:

;; Org gets its own file because it's an entire writing/agenda/literate-
;; programming environment, not a single feature. Treat the built-in
;; `org' the same as a third-party package — it's big enough to earn it.

;;; Code:

;;; @doc Built-in Org — outline, agenda, capture, literate-programming.
;;; Treated as a "third-party" package despite being built-in
;;; because it's the size and surface area of one. Capture templates
;;; and a small bind table live below.
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
  (org-confirm-babel-evaluate  t)
  (org-log-done                'time)
  (org-return-follows-link     t)
  (org-fold-catch-invisible-edits 'show-and-error)
  :config
  ;; Capture templates: keep the list short and obvious. Add more here
  ;; rather than scattering them across other modules.
  (setopt org-capture-templates
        '(("t" "Todo" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %U\n  %a")
          ("n" "Note" entry
           (file+headline org-default-notes-file "Notes")
           "* %?\n  %U"))))

;;; @doc Built-in time tracking with persistence across restarts. Lets
;;; you resume an interrupted clock without losing the entry.
(use-package org-clock
  :ensure nil
  :after org
  :custom
  (org-clock-persist t)
  (org-clock-idle-time 15)
  (org-clock-into-drawer t)
  :config
  (org-clock-persistence-insinuate))

;;; @doc Reveal Org emphasis markers (`*` `_` `/` `~`) only when point is
;;; on them — best of both visual worlds: clean reading, easy
;;; editing.
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

;;; @doc Modern visual styling for Org buffers and agenda — typographic
;;; bullets, faux-rendered blocks, agenda decorations.
(use-package org-modern
  :hook ((org-mode            . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

;;; @doc Zettelkasten-style note-linking on top of Org. SQLite database
;;; auto-syncs in the background; C-c n f / i / c are the three
;;; bindings you actually use day-to-day.
(use-package org-roam
  :commands (org-roam-node-find org-roam-capture)
  :functions (org-roam-db-autosync-mode)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture))
  :custom
  (org-roam-directory (expand-file-name "org-roam/" org-directory))
  :config
  (make-directory org-roam-directory t)
  (org-roam-db-autosync-mode 1))

(provide 'init-org)
;;; init-org.el ends here
