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
  (repo-review-queue-dir (my-var-file "repo-review/")))

;;; @doc Landing summary for a task branch (the in-repo
;;; `lisp/magit-land.el' library). `C-x p L' lists the commits of the
;;; current branch that are not on origin/main yet, with the check
;;; kinds from refs/notes/checks and the review note from
;;; refs/notes/review under each. `L' runs git-land in a compilation
;;; buffer (`C-u L' adds --yes), `S' shows the check results of the
;;; commit at point with git-results, `D' removes the worktree with
;;; `wt remove' after a prompt, and `g' refreshes.
(use-package magit-land
  :ensure nil
  :bind (:map project-prefix-map ("L" . magit-land)))

;;; @doc Marginal notes and highlights for files visited from the review
;;; queue. `repo-review' enables `org-remark-mode' in each visited file
;;; and points the notes at a per-project file beside the review queue,
;;; so the source tree stays untouched. Loads with the default pen set
;;; (mark, red/yellow/blue highlights).
(use-package org-remark
  :defer t)

(provide 'init-review)
;;; init-review.el ends here
