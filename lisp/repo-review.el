;;; repo-review.el --- Whole-repo code review queue -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.6"))

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

;;; Code:

(require 'org)
(require 'project)

(declare-function org-remark-mode "ext:org-remark")
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
                              (project-files proj)))
          #'string<)))

(defun repo-review--queue-file (root)
  "Return the queue Org file for the project rooted at ROOT."
  (let ((true (file-truename root)))
    (expand-file-name
     (format "%s-%s.org"
             (file-name-nondirectory (directory-file-name true))
             (substring (md5 true) 0 8))
     repo-review-queue-dir)))

(defun repo-review--insert-entry (rel)
  "Insert a TODO headline for project-relative path REL."
  (insert (format "** TODO %s\n:PROPERTIES:\n:REVIEW_PATH: %s\n:END:\n" rel rel)))

(defun repo-review--write-queue (file root)
  "Write a fresh queue FILE for the project at ROOT; return the file count."
  (let ((files (repo-review--files root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (insert "# -*- mode: repo-review-queue -*-\n"
              (format "#+title: Code review: %s\n" (abbreviate-file-name root))
              (format "#+review_root: %s\n" (expand-file-name root))
              "#+todo: TODO REVIEWING | DONE SKIP\n\n"
              "* Files\n")
      (mapc #'repo-review--insert-entry files))
    (length files)))

(defun repo-review--root ()
  "Return the project root recorded in the current queue buffer."
  (or (cadr (assoc "REVIEW_ROOT" (org-collect-keywords '("REVIEW_ROOT"))))
      (user-error "Not in a code review queue buffer")))

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
        (user-error "REVIEW_PATH escapes the review root: %s" rel))
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
      (when buf (kill-buffer buf))
      (message "Queued %d files" (repo-review--write-queue file root)))
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
      (dolist (rel (repo-review--files (repo-review--root)))
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
  "Open the file under the headline at point read-only."
  (interactive)
  (repo-review--open (repo-review--entry-file) t))

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

(provide 'repo-review)
;;; repo-review.el ends here
