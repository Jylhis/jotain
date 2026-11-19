;;; init-review.el --- Whole-repository review queue -*- lexical-binding: t; -*-

;;; Commentary:

;; Binds the self-contained lisp/repo-review.el library into the configuration,
;; with org-remark for marginal notes that never touch the sources.

;;; Code:

;;; @doc Whole-repository review queue (the in-repo `lisp/repo-review.el'
;;; library). `C-x p R' opens the current project's review queue: an Org
;;; file with one TODO headline per project file, listed by
;;; `project-files' so .gitignore applies. RET visits a file read-only
;;; in `view-mode', `e' opens it writable, `N' jumps to the next
;;; unreviewed file, and `g' appends files added since the queue was
;;; built. A prefix argument rebuilds the queue after a confirmation.
;;; `C-x p V' (`repo-review-pullreq') does the same for the Forge pull
;;; request or merge request at point: the queue lists the changed
;;; files and RET shows each as a read-only diff. `c' in a diff saves a
;;; draft comment on the line at point into the queue; `P' in the queue
;;; lists the drafts and posts them through Ghub after you confirm. `v'
;;; marks the file at point as viewed (an Org tag, so it persists with
;;; the review); it also works in a diff or a visited file. With gptel,
;;; `a' in a diff drafts a comment for the line at point and `A' on a
;;; draft shows advice on its wording; both send text to the gptel model
;;; and save or post nothing without you.
;;; Queues live under var/repo-review/.
(use-package repo-review
  :ensure nil
  :bind (:map project-prefix-map
              ("R" . repo-review-start)
              ("V" . repo-review-pullreq))
  :custom
  (repo-review-queue-dir (jotain-var-file "repo-review/")))

;;; @doc Marginal notes and highlights for files visited from the review
;;; queue. `repo-review' enables `org-remark-mode' in each visited file
;;; and points the notes at a per-project file beside the review queue,
;;; so the source tree stays untouched. Loads with the default pen set
;;; (mark, red/yellow/blue highlights).
(use-package org-remark
  :defer t)

(provide 'init-review)
;;; init-review.el ends here
