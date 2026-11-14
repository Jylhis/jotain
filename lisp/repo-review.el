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
(require 'cl-lib)
(require 'url-util)

(declare-function org-remark-mode "ext:org-remark")
(declare-function forge-current-topic "ext:forge-topic")
(declare-function forge-pullreq-p "ext:forge-pullreq")
(declare-function forge-get-pullreq "ext:forge-pullreq")
(declare-function forge-get-repository "ext:forge-repo")
(defvar org-remark-notes-file-name)   ; ext:org-remark-global-tracking

;; Optional: only called after the forge stack is loaded.
(declare-function ghub-request "ext:ghub")
(declare-function gptel-request "ext:gptel-request")

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

(defcustom repo-review-post-function #'repo-review--ghub-post
  "Function that sends one review comment request to the forge.
It receives a plist with the keys :forge, :host, :method, :resource and
:payload, and runs only after the user confirmed the post."
  :type 'function)

(defcustom repo-review-mark-viewed-on-visit nil
  "Non-nil means opening a file from the queue marks it as viewed."
  :type 'boolean)

(defcustom repo-review-ai-function #'repo-review--gptel-request
  "Function that asks a language model for text.
It receives the prompt string and a callback, and calls the callback
with the answer string.  The default uses gptel when it is installed."
  :type 'function)

(defcustom repo-review-ai-system-prompt
  "You are a careful code reviewer.  Answer with the review text only, \
in plain sentences, without markdown headings."
  "System prompt for the AI commands of `repo-review'."
  :type 'string)

(defcustom repo-review-ai-max-chars 20000
  "Longest diff text, in characters, that the AI commands send."
  :type 'natnum)

(defcustom repo-review-use-org-remark t
  "Non-nil means enable `org-remark-mode' in files opened from the queue."
  :type 'boolean)

;;; Queue mode

(defvar-local repo-review--diff-queue nil
  "Queue file of the review that the buffer belongs to.")
(defvar-local repo-review--diff-path nil
  "Project-relative path the buffer shows.")

(defvar-keymap repo-review-queue-mode-map
  "RET" #'repo-review-visit
  "e"   #'repo-review-visit-writable
  "P"   #'repo-review-post-comments
  "v"   #'repo-review-toggle-viewed
  "A"   #'repo-review-ai-check-draft
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
  (let ((notes (and repo-review-use-org-remark (repo-review--notes-file)))
        (queue buffer-file-name)
        (rel (file-relative-name file (repo-review--root))))
    (find-file-other-window file)
    (setq-local repo-review--diff-queue queue)
    (setq-local repo-review--diff-path rel)
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
    (when repo-review-mark-viewed-on-visit
      (repo-review--set-viewed t))
    (if (repo-review--pr-queue-p)
        (repo-review--show-diff (org-entry-get nil "REVIEW_PATH"))
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
symbol such as `github' or `gitlab'), :host (the API host) and
:repo (\"owner/name\")."
  (let ((repo (forge-get-repository pr)))
    (list :number (repo-review--slot pr 'number)
          :title (repo-review--slot pr 'title)
          :base (repo-review--slot pr 'base-rev)
          :head (repo-review--slot pr 'head-rev)
          :forge (repo-review--slot repo 'forge)
          :host (repo-review--slot repo 'apihost)
          :repo (format "%s/%s" (repo-review--slot repo 'owner)
                               (repo-review--slot repo 'name)))))

(defun repo-review--pr-header (info)
  "Return the queue header keyword lines for the pull request INFO."
  (format "#+review_pr: %d\n#+review_base: %s\n#+review_head: %s\n#+review_forge: %s\n#+review_repo: %s\n#+review_host: %s\n"
          (plist-get info :number) (plist-get info :base) (plist-get info :head)
          (plist-get info :forge) (plist-get info :repo)
          (or (plist-get info :host) "")))

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
      (repo-review-diff-mode 1)
      (setq-local default-directory (file-name-as-directory root))
      (setq-local repo-review--diff-queue queue)
      (setq-local repo-review--diff-path rel)
      (goto-char (point-min))
      (view-mode 1))
    (pop-to-buffer buf)))

;;; Viewed state

(defconst repo-review--viewed-tag "viewed"
  "Org tag that marks a queue entry as viewed.")

(defun repo-review--set-viewed (state)
  "Set the viewed tag of the file entry at point to STATE, or toggle on `toggle'."
  (let* ((tags (org-get-tags nil t))
         (was (member repo-review--viewed-tag tags))
         (now (if (eq state 'toggle) (not was) state)))
    (unless (eq (and was t) (and now t))
      (org-set-tags (if now
                        (cons repo-review--viewed-tag tags)
                      (delete repo-review--viewed-tag tags))))
    now))

(defun repo-review--viewed-summary ()
  "Return \"viewed/total\" for the file entries of the current queue buffer."
  (let ((total 0) (viewed 0))
    (org-map-entries
     (lambda ()
       (setq total (1+ total))
       (when (member repo-review--viewed-tag (org-get-tags nil t))
         (setq viewed (1+ viewed))))
     "REVIEW_PATH<>\"\"" 'file)
    (format "%d/%d" viewed total)))

(defun repo-review--goto-file-entry ()
  "Move to the file entry at point, from a draft or from a diff buffer."
  (if repo-review--diff-queue
      (goto-char (or (org-find-property "REVIEW_PATH" repo-review--diff-path)
                     (user-error "%s is not in the queue" repo-review--diff-path)))
    (let ((rel (org-entry-get nil "REVIEW_PATH" t)))
      (unless rel
        (user-error "No :REVIEW_PATH: property at point"))
      (goto-char (org-find-property "REVIEW_PATH" rel)))))

(defun repo-review-toggle-viewed ()
  "Toggle the viewed state of the file at point or of the visited file.
The state is an Org tag in the queue, so it persists with the review.
Works in the queue, in a diff and in a file opened from the queue."
  (interactive)
  (let ((buf (if repo-review--diff-queue
                 (find-file-noselect repo-review--diff-queue)
               (current-buffer))))
    (with-current-buffer buf
      (org-with-wide-buffer
       (let ((now (progn (repo-review--goto-file-entry)
                         (repo-review--set-viewed 'toggle))))
         (save-buffer)
         (message "%s: %s (viewed %s)" (org-entry-get nil "REVIEW_PATH")
                  (if now "viewed" "not viewed") (repo-review--viewed-summary)))))))

;;; Comments

(defvar-keymap repo-review-diff-mode-map
  "c" #'repo-review-comment
  "a" #'repo-review-ai-draft-comment
  "v" #'repo-review-toggle-viewed)

(define-minor-mode repo-review-diff-mode
  "Minor mode of a pull request diff buffer opened from a review queue.

\\{repo-review-diff-mode-map}"
  :lighter " RRdiff")

(defun repo-review--diff-new-line ()
  "Return the new-file line number of the diff line at point.
Signal a `user-error' on a removed line or outside a hunk."
  (let ((target (line-beginning-position)))
    (save-excursion
      (end-of-line)
      (unless (re-search-backward
               "^@@ -[0-9,]+ \\+\\([0-9]+\\)\\(?:,[0-9]+\\)? @@" nil t)
        (user-error "Point is not inside a diff hunk"))
      (when (= (point) target)
        (user-error "Point is on a hunk header"))
      (let ((n (string-to-number (match-string 1))))
        (forward-line 1)
        (while (< (point) target)
          (unless (memq (char-after) '(?- ?\\))
            (setq n (1+ n)))
          (forward-line 1))
        (when (memq (char-after target) '(?- ?\\))
          (user-error "Comments go on added or unchanged lines"))
        n))))

(defun repo-review--escape-body (text)
  "Return TEXT with lines that start with a star escaped for Org."
  (replace-regexp-in-string "^\\(\\*\\|,\\*\\)" ",\\1" text))

(defun repo-review--unescape-body (text)
  "Return TEXT with the Org escape of `repo-review--escape-body' removed."
  (replace-regexp-in-string "^,\\(,*\\*\\)" "\\1" text))

(defun repo-review--add-draft (queue rel line text)
  "Add a draft comment TEXT on LINE of REL to the QUEUE file's entry."
  (with-current-buffer (find-file-noselect queue)
    (org-with-wide-buffer
     (let ((pos (or (org-find-property "REVIEW_PATH" rel)
                    (user-error "%s is not in the queue" rel))))
       (goto-char pos)
       (org-end-of-subtree t t)
       (unless (bolp) (insert "\n"))
       (insert (format "*** Draft %s:%d\n:PROPERTIES:\n:DRAFT_LINE: %d\n:END:\n%s\n"
                       rel line line (repo-review--escape-body text)))))
    (save-buffer)))

(defun repo-review-comment (&optional initial)
  "Save a review comment on the diff line at point as a draft in the queue.
INITIAL is the starting text.  Nothing is sent to the forge; see
`repo-review-post-comments'."
  (interactive)
  (unless repo-review--diff-queue
    (user-error "Not in a pull request diff buffer"))
  (let* ((line (repo-review--diff-new-line))
         (text (string-trim (read-string (format "Comment on line %d: " line)
                                         initial))))
    (when (string-empty-p text)
      (user-error "Empty comment, not saved"))
    (repo-review--add-draft repo-review--diff-queue repo-review--diff-path
                            line text)
    (message "Draft saved on %s:%d" repo-review--diff-path line)))

(defun repo-review--drafts ()
  "Return the unposted drafts of the current queue buffer.
Each draft is a plist with :marker, :path, :line and :body."
  (let (drafts)
    (org-with-wide-buffer
     (org-map-entries
      (lambda ()
        (when (and (org-entry-get nil "DRAFT_LINE")
                   (not (org-entry-get nil "POSTED")))
          (push (list :marker (point-marker)
                      :path (org-entry-get nil "REVIEW_PATH" t)
                      :line (string-to-number (org-entry-get nil "DRAFT_LINE"))
                      :body (repo-review--draft-body))
                drafts)))
      nil 'file))
    (nreverse drafts)))

(defun repo-review--draft-body ()
  "Return the comment text of the draft at point."
  (save-excursion
    (org-back-to-heading t)
    (org-end-of-meta-data t)
    (string-trim
     (repo-review--unescape-body
      (buffer-substring-no-properties (point) (org-end-of-subtree t t))))))

(defun repo-review--pr-info-from-queue ()
  "Return the pull request plist recorded in the current queue buffer."
  (list :number (string-to-number (repo-review--keyword "REVIEW_PR"))
        :base (repo-review--keyword "REVIEW_BASE")
        :head (repo-review--keyword "REVIEW_HEAD")
        :forge (intern (repo-review--keyword "REVIEW_FORGE"))
        :repo (repo-review--keyword "REVIEW_REPO")
        :host (let ((h (repo-review--keyword "REVIEW_HOST")))
                (and h (not (string-empty-p h)) h))))

(defun repo-review--comment-request (info draft root)
  "Return the forge request for DRAFT on the pull request INFO in ROOT."
  (let* ((n (plist-get info :number))
         (repo (plist-get info :repo))
         (path (plist-get draft :path))
         (line (plist-get draft :line))
         (head (plist-get info :head))
         (common (list :forge (plist-get info :forge)
                       :host (plist-get info :host) :method "POST")))
    (pcase (plist-get info :forge)
      ('github
       (append common
               (list :resource (format "/repos/%s/pulls/%d/comments" repo n)
                     :payload `((body . ,(plist-get draft :body))
                                (commit_id . ,head) (path . ,path)
                                (line . ,line) (side . "RIGHT")))))
      ('gitlab
       (let ((mb (car (repo-review--git root "merge-base"
                                        (plist-get info :base) head))))
         (append common
                 (list :resource
                       (format "/projects/%s/merge_requests/%d/discussions"
                               (url-hexify-string repo) n)
                       :payload
                       `((body . ,(plist-get draft :body))
                         (position . ((position_type . "text")
                                      (base_sha . ,mb) (start_sha . ,mb)
                                      (head_sha . ,head)
                                      (old_path . ,path) (new_path . ,path)
                                      (new_line . ,line))))))))
      (f (user-error "Posting is not supported for forge %s" f)))))

(defun repo-review--ghub-post (request)
  "Send REQUEST, a plist from `repo-review--comment-request', through Ghub."
  (when noninteractive
    (error "Refusing to post a review comment in a non-interactive session"))
  (unless (require 'ghub nil t)
    (user-error "Ghub is not installed"))
  (ghub-request (plist-get request :method) (plist-get request :resource) nil
                :payload (plist-get request :payload)
                :forge (plist-get request :forge)
                :host (plist-get request :host)
                :auth 'forge))

(defun repo-review--confirm-post (info drafts)
  "Ask whether to post DRAFTS to the pull request INFO; return non-nil if yes."
  (with-current-buffer (get-buffer-create "*repo-review: comments to post*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (d drafts)
        (insert (format "%s:%d\n%s\n\n" (plist-get d :path) (plist-get d :line)
                        (plist-get d :body))))
      (special-mode)
      (goto-char (point-min)))
    (display-buffer (current-buffer)))
  (yes-or-no-p (format "Post %d comment(s) to %s #%d? " (length drafts)
                       (plist-get info :repo) (plist-get info :number))))

(defun repo-review-post-comments ()
  "Post the unposted drafts of this pull request queue as review comments.
List the drafts and ask for confirmation first.  A posted draft gets a
POSTED property and is not sent again."
  (interactive)
  (unless (repo-review--pr-queue-p)
    (user-error "Not in a pull request review queue"))
  (let ((info (repo-review--pr-info-from-queue))
        (root (repo-review--root))
        (drafts (repo-review--drafts)))
    (unless drafts
      (user-error "No unposted draft comments"))
    (unless (repo-review--confirm-post info drafts)
      (user-error "Nothing posted"))
    (let ((requests (mapcar (lambda (d) (repo-review--comment-request info d root))
                            drafts))
          (done 0))
      (unwind-protect
          (cl-mapc
           (lambda (d req)
             (funcall repo-review-post-function req)
             (org-with-wide-buffer
              (goto-char (plist-get d :marker))
              (org-entry-put nil "POSTED" (format-time-string "%FT%T%z")))
             (setq done (1+ done)))
           drafts requests)
        (save-buffer))
      (message "Posted %d comment(s)" done))))

;;; AI assistance

(defun repo-review--gptel-request (prompt callback)
  "Send PROMPT through gptel and pass the answer string to CALLBACK."
  (unless (require 'gptel nil t)
    (user-error "The AI commands need gptel, which is not installed"))
  (gptel-request prompt
    :system repo-review-ai-system-prompt
    :callback (lambda (response info)
                (if (stringp response)
                    (funcall callback response)
                  (message "repo-review: no AI answer (%s)"
                           (plist-get info :status))))))

(defun repo-review--hunk-text ()
  "Return the diff hunk at point, cut to `repo-review-ai-max-chars'."
  (save-excursion
    (end-of-line)
    (unless (re-search-backward "^@@ " nil t)
      (user-error "Point is not inside a diff hunk"))
    (let ((start (point)))
      (forward-line 1)
      (let ((end (if (re-search-forward "^@@ " nil t)
                     (match-beginning 0)
                   (point-max))))
        (buffer-substring-no-properties
         start (min end (+ start repo-review-ai-max-chars)))))))

(defun repo-review-ai-draft-comment ()
  "Ask the AI for a review comment on the hunk at point, then edit and save it.
The hunk is sent to the model that `repo-review-ai-function' uses.
The answer opens in the minibuffer; nothing is saved until you accept,
and nothing is posted to the forge."
  (interactive)
  (unless repo-review--diff-queue
    (user-error "Not in a pull request diff buffer"))
  (let* ((buf (current-buffer))
         (marker (point-marker))
         (line (repo-review--diff-new-line))
         (prompt (format "Write one short review comment for line %d of %s, \
as shown in this diff hunk.\n\n%s"
                         line repo-review--diff-path (repo-review--hunk-text))))
    (funcall repo-review-ai-function prompt
             (lambda (answer)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (goto-char marker)
                   (repo-review-comment (string-trim answer))))))))

(defun repo-review-ai-check-draft ()
  "Ask the AI to check the draft comment at point and show its advice.
The draft stays unchanged."
  (interactive)
  (unless (org-entry-get nil "DRAFT_LINE")
    (user-error "Point is not on a draft comment"))
  (let ((body (repo-review--draft-body))
        (path (org-entry-get nil "REVIEW_PATH" t)))
    (funcall repo-review-ai-function
             (format "Check this code review comment on %s for clarity, tone and \
whether the author can act on it.  Reply with short advice and, if needed, a better \
wording.\n\n%s" path body)
             (lambda (answer)
               (with-current-buffer (get-buffer-create "*repo-review: AI check*")
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (insert answer "\n"))
                 (special-mode)
                 (display-buffer (current-buffer)))))))

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
