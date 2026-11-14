;;; repo-review.el --- Whole-repo code review queue -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))

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

;; Review every file in a project without modifying it:
;;
;;   * `repo-review-start' builds a per-project Org queue with one
;;     TODO headline per project file (via `project-files', so
;;     .gitignore is respected).
;;   * RET opens the file in `view-mode'; `e' opens it writable.
;;   * If `org-remark' is installed, it is enabled in visited files.
;;     Notes pair with the queue: they live in a notes file beside the
;;     queue Org file, never in the source tree.
;;   * `g' appends files added since the queue was built.
;;   * `repo-review-pullreq' reviews a Forge pull request or merge
;;     request (GitHub or GitLab) the same way: the queue lists the
;;     files the request changes and RET shows each one as a read-only
;;     diff against the merge base.  Nothing is fetched or posted
;;     except by the commands described in the manual.

;;; Code:

(require 'org)
(require 'project)
(require 'eieio)

(declare-function org-remark-mode "ext:org-remark")
(declare-function forge-current-topic "ext:forge-topic")
(declare-function forge-pullreq-p "ext:forge-pullreq")
(declare-function forge-get-pullreq "ext:forge-pullreq")
(declare-function forge-get-repository "ext:forge-repo")
(defvar org-remark-notes-file-name)   ; ext:org-remark-global-tracking

(defgroup repo-review nil
  "Whole-repository code review queue."
  :group 'tools)

(defcustom repo-review-queue-dir
  (locate-user-emacs-file "repo-review/")
  "Directory where per-project review queue Org files live."
  :type 'directory)

(defcustom repo-review-source-extensions nil
  "Lowercase file extensions (no dot) to include in the queue.
nil means every file `project-files' returns."
  :type '(repeat string))

(defcustom repo-review-exclude-regexp nil
  "Regexp matched against project-relative paths to leave out, or nil."
  :type '(choice (const nil) regexp))

(defcustom repo-review-use-org-remark t
  "Non-nil means enable `org-remark-mode' in files opened from the queue."
  :type 'boolean)

;;; Queue mode

(defvar-keymap repo-review-queue-mode-map
  "RET" #'repo-review-visit
  "e"   #'repo-review-visit-writable
  "n"   #'org-next-visible-heading
  "p"   #'org-previous-visible-heading
  "N"   #'repo-review-next-todo
  "t"   #'org-todo
  "g"   #'repo-review-refresh
  "/"   #'org-sparse-tree
  "q"   #'quit-window)

(define-derived-mode repo-review-queue-mode org-mode "RepoReview"
  "Major mode for the per-project code review queue.

\\{repo-review-queue-mode-map}")

;;; Files

(defun repo-review--wanted-p (rel)
  "Return non-nil if project-relative path REL belongs in the queue."
  (let ((ext (file-name-extension rel)))
    (and (or (null repo-review-source-extensions)
             (and ext (member (downcase ext) repo-review-source-extensions)))
         (not (and repo-review-exclude-regexp
                   (string-match-p repo-review-exclude-regexp rel))))))

(defun repo-review--files (root)
  "Return sorted project-relative paths of files to review under ROOT."
  (let ((proj (project-current nil root)))
    (unless proj
      (user-error "No project at %s" root))
    (sort (seq-filter #'repo-review--wanted-p
                      (mapcar (lambda (f) (file-relative-name f root))
                              (seq-filter #'file-exists-p (project-files proj))))
          #'string<)))

(defun repo-review--queue-file (root &optional pr)
  "Return the queue Org file for the project rooted at ROOT.
PR, a pull request number, selects the queue of that request."
  (let ((true (file-truename root)))
    (expand-file-name
     (format "%s-%s%s.org"
             (file-name-nondirectory (directory-file-name true))
             (substring (md5 true) 0 8)
             (if pr (format "-pr%d" pr) ""))
     repo-review-queue-dir)))

(defun repo-review--insert-entry (rel)
  "Insert a TODO headline for project-relative path REL."
  (insert (format "** TODO %s\n:PROPERTIES:\n:REVIEW_PATH: %s\n:END:\n" rel rel)))

(defun repo-review--write-queue (file root &optional files header)
  "Write a fresh queue FILE for the project at ROOT; return the file count.
FILES, when non-nil, is the list `repo-review--files' already returned.
HEADER is a string of extra keyword lines for the queue header."
  (let ((files (or files (repo-review--files root))))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert "# -*- mode: repo-review-queue -*-\n"
              (format "#+title: Code review: %s\n" (abbreviate-file-name root))
              (format "#+review_root: %s\n" (expand-file-name root))
              (or header "")
              "#+todo: TODO REVIEWING | DONE SKIP\n\n"
              "* Files\n")
      (mapc #'repo-review--insert-entry files))
    (length files)))

(defun repo-review--keyword (name)
  "Return the value of the queue header keyword NAME, or nil."
  (cadr (assoc name (org-collect-keywords (list name)))))

(defun repo-review--root ()
  "Return the project root recorded in the current queue buffer."
  (or (repo-review--keyword "REVIEW_ROOT")
      (user-error "Not in a code review queue buffer")))

(defun repo-review--pr-queue-p ()
  "Return non-nil when the current queue buffer reviews a pull request."
  (and (repo-review--keyword "REVIEW_HEAD") t))

(defun repo-review--queue-files ()
  "Return the project-relative paths the current queue buffer should list."
  (if (repo-review--pr-queue-p)
      (repo-review--pr-files (repo-review--root)
                             (repo-review--keyword "REVIEW_BASE")
                             (repo-review--keyword "REVIEW_HEAD"))
    (repo-review--files (repo-review--root))))

(defun repo-review--entry-file ()
  "Return the absolute path of the file under the headline at point.
Reject entries with absolute paths or paths escaping REVIEW_ROOT."
  (let ((rel (org-entry-get nil "REVIEW_PATH"))
        (root (repo-review--root)))
    (unless rel
      (user-error "No :REVIEW_PATH: property at point"))
    (when (file-name-absolute-p rel)
      (user-error "REVIEW_PATH must be relative: %s" rel))
    (let ((abs (expand-file-name rel root)))
      (unless (file-in-directory-p abs root)
        (user-error "REVIEW_PATH is outside the review root or a symlink that leads out of it: %s"
                    rel))
      abs)))

;;; Commands

;;;###autoload
(defun repo-review-start (&optional rebuild)
  "Open the code review queue for the current project, building it if needed.
With prefix argument REBUILD, discard review state and rebuild the queue."
  (interactive "P")
  (let* ((proj (or (project-current) (user-error "Not in a project")))
         (root (expand-file-name (project-root proj)))
         (file (repo-review--queue-file root))
         (buf (find-buffer-visiting file)))
    (when (and rebuild (file-exists-p file)
               (not (yes-or-no-p "Discard all review state and rebuild? ")))
      (setq rebuild nil))
    (when (or rebuild (not (file-exists-p file)))
      (let ((files (repo-review--files root)))
        (when (and buf (not (kill-buffer buf)))
          (user-error "Queue buffer was not killed; rebuild cancelled"))
        (message "Queued %d files" (repo-review--write-queue file root files))))
    (pop-to-buffer (find-file-noselect file))
    (unless (derived-mode-p 'repo-review-queue-mode)
      (repo-review-queue-mode))
    (org-content 2)))

(defun repo-review-refresh ()
  "Append files added to the project since the queue was built."
  (interactive)
  (let ((known (make-hash-table :test #'equal))
        (added 0))
    (org-map-entries
     (lambda () (when-let* ((f (org-entry-get nil "REVIEW_PATH"))) (puthash f t known)))
     nil 'file)
    (save-excursion
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (dolist (rel (repo-review--queue-files))
        (unless (gethash rel known)
          (repo-review--insert-entry rel)
          (setq added (1+ added)))))
    (save-buffer)
    (message "Added %d new file(s)" added)))

(defun repo-review--notes-file ()
  "Return the marginal-notes file that pairs with the current queue."
  (unless buffer-file-name
    (user-error "Not in a code review queue buffer"))
  (concat (file-name-sans-extension buffer-file-name) "-notes.org"))

(defun repo-review--open (file read-only)
  "Visit FILE in another window, in `view-mode' when READ-ONLY.
Enables `org-remark-mode' when `repo-review-use-org-remark' and the
package are available, with the notes file beside the queue."
  (let ((notes (and repo-review-use-org-remark (repo-review--notes-file))))
    (find-file-other-window file)
    (view-mode (if read-only 1 -1))
    (when (and notes (require 'org-remark nil t))
      ;; `org-remark-notes-get-file-name' returns this absolute value
      ;; as-is, so highlights never write into the source tree.
      (setq-local org-remark-notes-file-name notes)
      (org-remark-mode 1))))

(defun repo-review-visit ()
  "Open the file under the headline at point read-only.
In a pull request queue, show the diff of the file instead."
  (interactive)
  (let ((file (repo-review--entry-file)))
    (if (repo-review--pr-queue-p)
        (repo-review--show-diff file)
      (repo-review--open file t))))

(defun repo-review-visit-writable ()
  "Open the file under the headline at point for editing."
  (interactive)
  (repo-review--open (repo-review--entry-file) nil))

(defun repo-review-next-todo ()
  "Move to the next headline in TODO state."
  (interactive)
  (let ((pos (save-excursion
               (end-of-line)
               (re-search-forward "^\\*+ TODO " nil t))))
    (if pos
        (progn (goto-char pos) (org-back-to-heading) (org-fold-show-context))
      (message "No TODO files left"))))

;;; Pull requests

(defun repo-review--git (root &rest args)
  "Run git with ARGS in ROOT and return the output lines; signal on failure."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory root)))
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (user-error "Git failed (%s): %s" (string-join args " ")
                    (string-trim (buffer-string)))))
    (split-string (buffer-string) "\n" t)))

(defun repo-review--pr-files (root base head)
  "Return the paths a pull request changes between BASE and HEAD in ROOT.
The comparison uses the merge base, like the forge does."
  (sort (repo-review--git root "diff" "--name-only" "--no-renames"
                          (format "%s...%s" base head))
        #'string<))

(defun repo-review--pr-at-point ()
  "Return the Forge pull request at point, or prompt for a number."
  (unless (require 'forge nil t)
    (user-error "Forge is not installed"))
  (let ((topic (forge-current-topic)))
    (if (and topic (forge-pullreq-p topic))
        topic
      (or (forge-get-pullreq (read-number "Pull request number: "))
          (user-error "Pull request not found in the forge database")))))

(defun repo-review--slot (object slot)
  "Return SLOT of the Forge OBJECT."
  (eieio-oref object slot))

(defun repo-review--pr-info (pr)
  "Return a plist describing the Forge pull request PR.
The keys are :number, :title, :base, :head (revisions), :forge (a
symbol such as `github' or `gitlab') and :repo (\"owner/name\")."
  (let ((repo (forge-get-repository pr)))
    (list :number (repo-review--slot pr 'number)
          :title (repo-review--slot pr 'title)
          :base (repo-review--slot pr 'base-rev)
          :head (repo-review--slot pr 'head-rev)
          :forge (repo-review--slot repo 'forge)
          :repo (format "%s/%s" (repo-review--slot repo 'owner)
                               (repo-review--slot repo 'name)))))

(defun repo-review--pr-header (info)
  "Return the queue header keyword lines for the pull request INFO."
  (format "#+review_pr: %d\n#+review_base: %s\n#+review_head: %s\n#+review_forge: %s\n#+review_repo: %s\n"
          (plist-get info :number) (plist-get info :base) (plist-get info :head)
          (plist-get info :forge) (plist-get info :repo)))

(defvar-local repo-review--diff-queue nil
  "Queue file of the pull request whose diff the buffer shows.")
(defvar-local repo-review--diff-path nil
  "Project-relative path whose diff the buffer shows.")

(defun repo-review--show-diff (rel)
  "Show the diff of project-relative path REL for the current pull request queue."
  (let* ((root (repo-review--root))
         (base (repo-review--keyword "REVIEW_BASE"))
         (head (repo-review--keyword "REVIEW_HEAD"))
         (queue buffer-file-name)
         (buf (get-buffer-create (format "*repo-review: %s*" rel)))
         (lines (repo-review--git root "diff" (format "%s...%s" base head) "--" rel)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-join lines "\n") "\n"))
      (diff-mode)
      (setq-local default-directory (file-name-as-directory root))
      (setq-local repo-review--diff-queue queue)
      (setq-local repo-review--diff-path rel)
      (goto-char (point-min))
      (view-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun repo-review-pullreq (&optional pr)
  "Open the review queue of a Forge pull request or merge request.
PR defaults to the request at point, or one chosen by number.
The queue lists the files the request changes; RET shows each as a
read-only diff.  The request revisions must exist locally: run
`forge-pull' or fetch the request first."
  (interactive)
  (let* ((proj (or (project-current) (user-error "Not in a project")))
         (root (expand-file-name (project-root proj)))
         (info (repo-review--pr-info (or pr (repo-review--pr-at-point))))
         (file (repo-review--queue-file root (plist-get info :number))))
    (unless (file-exists-p file)
      (let ((files (repo-review--pr-files root (plist-get info :base)
                                          (plist-get info :head))))
        (message "Queued %d files"
                 (repo-review--write-queue file root files
                                           (repo-review--pr-header info)))))
    (pop-to-buffer (find-file-noselect file))
    (unless (derived-mode-p 'repo-review-queue-mode)
      (repo-review-queue-mode))
    (org-content 2)))

(provide 'repo-review)
;;; repo-review.el ends here
