;;; init-review.el --- Whole-repository review queue -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
;;; lists the drafts and posts them through Ghub after you confirm.
;;; Queues live under var/repo-review/.
(use-package repo-review
  :ensure nil
  :bind (:map project-prefix-map
              ("R" . repo-review-start)
              ("V" . repo-review-pullreq))
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
