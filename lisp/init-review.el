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
;;; Queues live under var/repo-review/.
(use-package repo-review
  :ensure nil
  :bind (:map project-prefix-map ("R" . repo-review-start))
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
