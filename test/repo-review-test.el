;;; repo-review-test.el --- ERT tests for repo-review -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;;; Code:

(require 'ert)
(require 'repo-review)

(defmacro repo-review-test--with-project (&rest body)
  "Run BODY in a fresh git project with a private queue directory.
Binds `root' to the project directory."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "repo-review-" t)))
          (repo-review-queue-dir (expand-file-name "queues/" root))
          (repo-review-use-org-remark nil)
          (default-directory root))
     (unwind-protect
         (progn
           (make-directory "src")
           (dolist (f '("src/a.py" "b.nix" "c.log"))
             (write-region "x\n" nil f))
           (write-region "*.log\nqueues/\n" nil ".gitignore")
           (call-process "git" nil nil nil "init" "-q")
           (call-process "git" nil nil nil "add" "-A")
           ,@body)
       (dolist (buf (buffer-list))
         (when (string-prefix-p root (or (buffer-file-name buf) ""))
           (with-current-buffer buf (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (delete-directory root t))))

(defun repo-review-test--paths ()
  "Return the REVIEW_PATH of every entry in the current queue buffer."
  (org-map-entries (lambda () (org-entry-get nil "REVIEW_PATH"))
                   "REVIEW_PATH<>\"\"" 'file))

(ert-deftest repo-review-start-builds-queue ()
  (repo-review-test--with-project
    (repo-review-start)
    (should (derived-mode-p 'repo-review-queue-mode))
    (should (equal (repo-review-test--paths) '(".gitignore" "b.nix" "src/a.py")))))

(ert-deftest repo-review-entry-file-resolves-in-project ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (should (equal (repo-review--entry-file)
                   (expand-file-name ".gitignore" root)))))

(ert-deftest repo-review-extension-filter ()
  (repo-review-test--with-project
    (let ((repo-review-source-extensions '("nix")))
      (repo-review-start)
      (should (equal (repo-review-test--paths) '("b.nix"))))))

(ert-deftest repo-review-refresh-appends-once ()
  (repo-review-test--with-project
    (repo-review-start)
    (write-region "x\n" nil (expand-file-name "src/new.go" root))
    (repo-review-refresh)
    (repo-review-refresh)
    (should (equal (repo-review-test--paths)
                   '(".gitignore" "b.nix" "src/a.py" "src/new.go")))))

(ert-deftest repo-review-reopen-restores-mode ()
  (repo-review-test--with-project
    (repo-review-start)
    (let ((file buffer-file-name))
      (kill-buffer)
      (with-current-buffer (find-file-noselect file)
        (should (derived-mode-p 'repo-review-queue-mode))))))

(ert-deftest repo-review-next-todo-skips-done ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (org-todo "DONE")
    (repo-review-next-todo)
    (should (equal (org-entry-get nil "REVIEW_PATH") "b.nix"))))

(ert-deftest repo-review-visit-is-read-only ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (repo-review-visit)
    (should view-mode)
    (should buffer-read-only)))

(ert-deftest repo-review-rejects-absolute-paths ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (org-entry-put nil "REVIEW_PATH" "/etc/passwd")
    (should-error (repo-review--entry-file) :type 'user-error)))

(ert-deftest repo-review-rejects-parent-directory-escape ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (org-entry-put nil "REVIEW_PATH" "../../etc/passwd")
    (should-error (repo-review--entry-file) :type 'user-error)))

(ert-deftest repo-review-rejects-symlink-leaving-root-and-says-why ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (let ((outside (make-temp-file "repo-review-outside-")))
      (unwind-protect
          (progn
            (make-symbolic-link outside (expand-file-name "link" root))
            (org-entry-put nil "REVIEW_PATH" "link")
            (let ((err (should-error (repo-review--entry-file) :type 'user-error)))
              (should (string-match-p "symlink" (cadr err)))))
        (delete-file outside)))))

(ert-deftest repo-review-exclude-regexp-option ()
  (repo-review-test--with-project
    (let ((repo-review-exclude-regexp "\\.nix\\'"))
      (repo-review-start)
      (should (equal (repo-review-test--paths) '(".gitignore" "src/a.py"))))))

(ert-deftest repo-review-rejects-absolute-path-inside-root ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (org-entry-put nil "REVIEW_PATH" (expand-file-name "b.nix" root))
    (should-error (repo-review--entry-file) :type 'user-error)))

(ert-deftest repo-review-missing-path-is-user-error ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (should-error (repo-review--entry-file) :type 'user-error)))

(ert-deftest repo-review-rebuild-with-confirmation ()
  (repo-review-test--with-project
    (repo-review-start)
    (write-region "extra\n" nil (expand-file-name "extra.go" root))
    (let ((confirmed nil))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_prompt) (setq confirmed t) t)))
        (repo-review-start t)
        (should confirmed)))
    (should (equal (repo-review-test--paths)
                   '(".gitignore" "b.nix" "extra.go" "src/a.py")))))

(ert-deftest repo-review-rebuild-cancel-with-confirmation ()
  (repo-review-test--with-project
    (repo-review-start)
    (let ((original (repo-review-test--paths)))
      (write-region "extra\n" nil (expand-file-name "extra.go" root))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (_prompt) nil)))
        (repo-review-start t))
      (should (equal (repo-review-test--paths) original)))))

(ert-deftest repo-review-queue-skips-deleted-tracked-files ()
  (repo-review-test--with-project
    (delete-file (expand-file-name "b.nix" root))
    (repo-review-start)
    (should (equal (repo-review-test--paths) '(".gitignore" "src/a.py")))))

(ert-deftest repo-review-rebuild-keeps-buffer-when-kill-refused ()
  (repo-review-test--with-project
    (repo-review-start)
    (let ((queue (current-buffer)))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                ((symbol-function 'kill-buffer) (lambda (&rest _) nil)))
        (should-error (repo-review-start t) :type 'user-error))
      (should (buffer-live-p queue)))))

(ert-deftest repo-review-start-not-in-project ()
  (let ((default-directory "/tmp"))
    (should-error (repo-review-start) :type 'user-error)))

(ert-deftest repo-review-entry-file-not-in-queue-buffer ()
  (with-temp-buffer
    (should-error (repo-review--entry-file) :type 'user-error)))

;;; Pull requests

(defun repo-review-test--git (&rest args)
  "Run git with ARGS in `default-directory' and return trimmed stdout."
  (with-temp-buffer
    (apply #'process-file "git" nil t nil "-c" "user.name=t" "-c" "user.email=t@t" args)
    (string-trim (buffer-string))))

(defmacro repo-review-test--with-pr (&rest body)
  "Run BODY in a project with a feature branch; bind `info' to a PR plist."
  (declare (indent 0) (debug t))
  `(repo-review-test--with-project
     (repo-review-test--git "commit" "-q" "-m" "base")
     (let ((base (repo-review-test--git "rev-parse" "HEAD")))
       (repo-review-test--git "checkout" "-q" "-b" "feat")
       (write-region "y\n" nil "src/a.py")
       (write-region "n\n" nil "new.txt")
       (repo-review-test--git "add" "-A")
       (repo-review-test--git "commit" "-q" "-m" "change")
       (let ((info (list :number 7 :title "T" :base base
                         :head (repo-review-test--git "rev-parse" "HEAD")
                         :forge 'github :repo "o/r")))
         (cl-letf (((symbol-function 'repo-review--pr-info) (lambda (_pr) info))
                   ((symbol-function 'repo-review--pr-at-point) (lambda () 'pr)))
           ,@body)))))

(ert-deftest repo-review-pr-queue-lists-changed-files ()
  (repo-review-test--with-pr
    (repo-review-pullreq)
    (should (derived-mode-p 'repo-review-queue-mode))
    (should (repo-review--pr-queue-p))
    (should (string-match-p "-pr7\\.org\\'" buffer-file-name))
    (should (equal (repo-review-test--paths) '("new.txt" "src/a.py")))))

(ert-deftest repo-review-pr-visit-shows-read-only-diff ()
  (repo-review-test--with-pr
    (repo-review-pullreq)
    (goto-char (point-min))
    (repo-review-next-todo)
    (repo-review-visit)
    (should (derived-mode-p 'diff-mode))
    (should buffer-read-only)
    (should (string-match-p "^\\+n$" (buffer-string)))
    (should (equal (file-name-nondirectory
                    (directory-file-name default-directory))
                   (file-name-nondirectory (directory-file-name root))))))

(ert-deftest repo-review-pr-refresh-keeps-entries ()
  (repo-review-test--with-pr
    (repo-review-pullreq)
    (repo-review-refresh)
    (should (equal (repo-review-test--paths) '("new.txt" "src/a.py")))))

(ert-deftest repo-review-pr-reopen-reuses-queue ()
  (repo-review-test--with-pr
    (repo-review-pullreq)
    (goto-char (point-min))
    (repo-review-next-todo)
    (org-todo "DONE")
    (save-buffer)
    (repo-review-pullreq)
    (goto-char (point-min))
    (repo-review-next-todo)
    (should (equal (org-entry-get nil "REVIEW_PATH") "src/a.py"))))

(ert-deftest repo-review-pr-bad-revision-is-user-error ()
  (repo-review-test--with-project
    (should-error (repo-review--pr-files root "nope" "nada") :type 'user-error)))

;;; Comments

(defun repo-review-test--open-diff (path)
  "Open the diff of PATH from the pull request queue in the current buffer."
  (goto-char (point-min))
  (goto-char (org-find-property "REVIEW_PATH" path))
  (repo-review-visit))

(defmacro repo-review-test--with-posts (var &rest body)
  "Run BODY with the post function recording requests in VAR; confirm yes."
  (declare (indent 1) (debug t))
  `(let* ((,var nil)
          (repo-review-post-function (lambda (r) (push r ,var))))
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
       ,@body)))

(ert-deftest repo-review-diff-new-line-counts-added-and-context ()
  (with-temp-buffer
    (insert "diff --git a/f b/f\n--- a/f\n+++ b/f\n@@ -1,3 +5,3 @@\n ctx\n-old\n+new\n tail\n")
    (goto-char (point-min))
    (forward-line 4)
    (should (= (repo-review--diff-new-line) 5))
    (forward-line 2)
    (should (= (repo-review--diff-new-line) 6))
    (forward-line 1)
    (should (= (repo-review--diff-new-line) 7))
    (forward-line -2)
    (should-error (repo-review--diff-new-line) :type 'user-error)
    (goto-char (point-min))
    (should-error (repo-review--diff-new-line) :type 'user-error)))

(ert-deftest repo-review-escape-body-roundtrip ()
  (let ((text "* star\n,* comma\nplain"))
    (should (equal (repo-review--unescape-body (repo-review--escape-body text))
                   text))))

(ert-deftest repo-review-comment-saves-draft-without-posting ()
  (repo-review-test--with-pr
    (repo-review-pullreq)
    (let ((queue (current-buffer)))
      (repo-review-test--with-posts sent
        (repo-review-test--open-diff "new.txt")
        (goto-char (point-min))
        (re-search-forward "^\\+n")
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "* fix this")))
          (repo-review-comment))
        (should-not sent)
        (with-current-buffer queue
          (let ((d (car (repo-review--drafts))))
            (should (equal (plist-get d :path) "new.txt"))
            (should (= (plist-get d :line) 1))
            (should (equal (plist-get d :body) "* fix this"))))))))

(ert-deftest repo-review-comment-outside-diff-buffer ()
  (with-temp-buffer
    (should-error (repo-review-comment) :type 'user-error)))

(ert-deftest repo-review-post-github-request-and-mark-posted ()
  (repo-review-test--with-pr
    (repo-review-pullreq)
    (let ((queue (current-buffer)))
      (repo-review--add-draft buffer-file-name "src/a.py" 1 "looks off")
      (repo-review-test--with-posts sent
        (repo-review-post-comments)
        (should (= (length sent) 1))
        (let ((r (car sent)))
          (should (equal (plist-get r :resource) "/repos/o/r/pulls/7/comments"))
          (should (equal (alist-get 'path (plist-get r :payload)) "src/a.py"))
          (should (equal (alist-get 'commit_id (plist-get r :payload))
                         (plist-get info :head)))
          (should (equal (alist-get 'body (plist-get r :payload)) "looks off")))
        (should-not (repo-review--drafts))
        (should-error (repo-review-post-comments) :type 'user-error)
        (should (= (length sent) 1))
        (should (buffer-live-p queue))))))

(ert-deftest repo-review-post-gitlab-request ()
  (repo-review-test--with-pr
    (setq info (plist-put info :forge 'gitlab))
    (setq info (plist-put info :repo "grp/proj"))
    (repo-review-pullreq)
    (repo-review--add-draft buffer-file-name "new.txt" 1 "hm")
    (repo-review-test--with-posts sent
      (repo-review-post-comments)
      (let* ((r (car sent)) (pos (alist-get 'position (plist-get r :payload))))
        (should (equal (plist-get r :resource)
                       "/projects/grp%2Fproj/merge_requests/7/discussions"))
        (should (equal (alist-get 'new_line pos) 1))
        (should (equal (alist-get 'head_sha pos) (plist-get info :head)))
        (should (equal (alist-get 'base_sha pos) (plist-get info :base)))))))

(ert-deftest repo-review-post-needs-confirmation ()
  (repo-review-test--with-pr
    (repo-review-pullreq)
    (repo-review--add-draft buffer-file-name "src/a.py" 1 "x")
    (let* ((sent nil)
           (repo-review-post-function (lambda (r) (push r sent))))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
        (should-error (repo-review-post-comments) :type 'user-error))
      (should-not sent)
      (should (repo-review--drafts)))))

(ert-deftest repo-review-post-failure-keeps-later-drafts ()
  (repo-review-test--with-pr
    (repo-review-pullreq)
    (repo-review--add-draft buffer-file-name "src/a.py" 1 "one")
    (repo-review--add-draft buffer-file-name "src/a.py" 2 "two")
    (let* ((calls 0)
           (repo-review-post-function
            (lambda (_r) (when (= (cl-incf calls) 2) (error "Network down")))))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (should-error (repo-review-post-comments)))
      (should (equal (mapcar (lambda (d) (plist-get d :body)) (repo-review--drafts))
                     '("two"))))))

(ert-deftest repo-review-default-post-refuses-in-batch ()
  (should noninteractive)
  (should-error (repo-review--ghub-post '(:method "POST"))))

(ert-deftest repo-review-post-outside-pr-queue ()
  (repo-review-test--with-project
    (repo-review-start)
    (should-error (repo-review-post-comments) :type 'user-error)))

;;; Viewed state

(defun repo-review-test--viewed ()
  "Return the paths of the entries tagged viewed in the current queue."
  (delq nil (org-map-entries (lambda () (org-entry-get nil "REVIEW_PATH"))
                             "viewed" 'file)))

(ert-deftest repo-review-toggle-viewed-in-queue ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (repo-review-toggle-viewed)
    (should (equal (repo-review-test--viewed) '(".gitignore")))
    (repo-review-toggle-viewed)
    (should-not (repo-review-test--viewed))))

(ert-deftest repo-review-viewed-persists-across-reopen ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (repo-review-toggle-viewed)
    (let ((file buffer-file-name))
      (kill-buffer)
      (with-current-buffer (find-file-noselect file)
        (should (equal (repo-review-test--viewed) '(".gitignore")))))))

(ert-deftest repo-review-viewed-survives-refresh-and-todo-change ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (repo-review-toggle-viewed)
    (org-todo "DONE")
    (repo-review-refresh)
    (should (equal (repo-review-test--viewed) '(".gitignore")))))

(ert-deftest repo-review-viewed-from-visited-file ()
  (repo-review-test--with-project
    (repo-review-start)
    (let ((queue (current-buffer)))
      (goto-char (point-min))
      (repo-review-next-todo)
      (repo-review-visit)
      (repo-review-toggle-viewed)
      (with-current-buffer queue
        (should (equal (repo-review-test--viewed) '(".gitignore")))))))

(ert-deftest repo-review-viewed-from-pr-diff ()
  (repo-review-test--with-pr
    (repo-review-pullreq)
    (let ((queue (current-buffer)))
      (repo-review-test--open-diff "src/a.py")
      (repo-review-toggle-viewed)
      (with-current-buffer queue
        (should (equal (repo-review-test--viewed) '("src/a.py")))))))

(ert-deftest repo-review-mark-viewed-on-visit-option ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (let ((queue (current-buffer)))
      (repo-review-visit)
      (with-current-buffer queue
        (should-not (repo-review-test--viewed)))
      (let ((repo-review-mark-viewed-on-visit t))
        (with-current-buffer queue (repo-review-visit)))
      (with-current-buffer queue
        (should (equal (repo-review-test--viewed) '(".gitignore")))))))

(ert-deftest repo-review-viewed-on-draft-heading-targets-file ()
  (repo-review-test--with-pr
    (repo-review-pullreq)
    (repo-review--add-draft buffer-file-name "src/a.py" 1 "x")
    (goto-char (point-min))
    (re-search-forward "^\\*\\*\\* Draft")
    (repo-review-toggle-viewed)
    (should (equal (repo-review-test--viewed) '("src/a.py")))
    (should (equal (repo-review--viewed-summary) "1/2"))))

(ert-deftest repo-review-toggle-viewed-outside-queue ()
  (with-temp-buffer
    (should-error (repo-review-toggle-viewed))))

(ert-deftest repo-review-mark-and-unmark-viewed ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (repo-review-mark-viewed)
    (repo-review-mark-viewed)
    (should (equal (repo-review-test--viewed) '(".gitignore")))
    (should (org-entry-get nil "VIEWED_REV"))
    (repo-review-unmark-viewed)
    (repo-review-unmark-viewed)
    (should-not (repo-review-test--viewed))
    (should-not (org-entry-get nil "VIEWED_REV"))))

(ert-deftest repo-review-viewed-ends-when-file-changes ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (repo-review-mark-viewed)
    (repo-review-next-todo)
    (repo-review-mark-viewed)
    (should (equal (repo-review-test--viewed) '(".gitignore" "b.nix")))
    (write-region "changed\n" nil (expand-file-name "b.nix" root))
    (repo-review-start)
    (should (equal (repo-review-test--viewed) '(".gitignore")))
    (should (equal (repo-review--viewed-summary) "1/3"))))

(ert-deftest repo-review-viewed-kept-when-file-is-unchanged ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (repo-review-mark-viewed)
    (should (= 0 (repo-review--expire-viewed)))
    (repo-review-start)
    (should (equal (repo-review-test--viewed) '(".gitignore")))))

(ert-deftest repo-review-viewed-refresh-expires-changed-file ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (repo-review-mark-viewed)
    (write-region "changed\n" nil (expand-file-name ".gitignore" root))
    (repo-review-refresh)
    (should-not (repo-review-test--viewed))))

(ert-deftest repo-review-next-todo-skips-viewed ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (repo-review-mark-viewed)
    (goto-char (point-min))
    (repo-review-next-todo)
    (should (equal (org-entry-get nil "REVIEW_PATH") "b.nix"))
    (repo-review-mark-viewed)
    (repo-review-next-todo)
    (should (equal (org-entry-get nil "REVIEW_PATH") "src/a.py"))
    (repo-review-mark-viewed)
    (goto-char (point-min))
    (let ((pos (point)))
      (repo-review-next-todo)
      (should (= pos (point))))))

(ert-deftest repo-review-queue-header-shows-viewed-count ()
  (repo-review-test--with-project
    (repo-review-start)
    (goto-char (point-min))
    (repo-review-next-todo)
    (should (equal (repo-review--header-summary) "0/3"))
    (repo-review-mark-viewed)
    (should (equal (repo-review--header-summary) "1/3"))))

(ert-deftest repo-review-viewed-pr-queue-records-blob ()
  (repo-review-test--with-pr
    (repo-review-pullreq)
    (let ((queue (current-buffer)))
      (repo-review-test--open-diff "src/a.py")
      (repo-review-mark-viewed)
      (with-current-buffer queue
        (should (= 0 (repo-review--expire-viewed)))
        (should (equal (repo-review-test--viewed) '("src/a.py")))
        (should (= 40 (length (org-entry-get (org-find-property "REVIEW_PATH" "src/a.py")
                                             "VIEWED_REV"))))))))

;;; AI assistance

(ert-deftest repo-review-ai-draft-saves-edited-answer ()
  (repo-review-test--with-pr
    (repo-review-pullreq)
    (let ((queue (current-buffer)) prompts)
      (repo-review-test--open-diff "new.txt")
      (goto-char (point-min))
      (re-search-forward "^\\+n")
      (let ((repo-review-ai-function
             (lambda (prompt cb) (push prompt prompts) (funcall cb "  Rename n.  "))))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (_p &optional initial &rest _) initial)))
          (repo-review-ai-draft-comment)))
      (should (string-match-p "line 1 of new.txt" (car prompts)))
      (should (string-match-p "^\\+n" (car prompts)))
      (with-current-buffer queue
        (should (equal (plist-get (car (repo-review--drafts)) :body) "Rename n."))))))

(ert-deftest repo-review-ai-draft-does-not-post ()
  (repo-review-test--with-pr
    (repo-review-pullreq)
    (repo-review-test--open-diff "new.txt")
    (goto-char (point-min))
    (re-search-forward "^\\+n")
    (let* ((sent nil)
           (repo-review-post-function (lambda (r) (push r sent)))
           (repo-review-ai-function (lambda (_p cb) (funcall cb "x"))))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_p &optional initial &rest _) initial)))
        (repo-review-ai-draft-comment))
      (should-not sent))))

(ert-deftest repo-review-ai-draft-outside-diff ()
  (with-temp-buffer
    (should-error (repo-review-ai-draft-comment) :type 'user-error)))

(ert-deftest repo-review-ai-check-shows-advice-and-keeps-draft ()
  (repo-review-test--with-pr
    (repo-review-pullreq)
    (repo-review--add-draft buffer-file-name "src/a.py" 1 "bad wording")
    (goto-char (point-min))
    (re-search-forward "^\\*\\*\\* Draft")
    (let ((repo-review-ai-function
           (lambda (prompt cb)
             (should (string-match-p "bad wording" prompt))
             (funcall cb "Say it kindly."))))
      (cl-letf (((symbol-function 'display-buffer) #'ignore))
        (repo-review-ai-check-draft)))
    (with-current-buffer "*repo-review: AI check*"
      (should (string-match-p "kindly" (buffer-string))))
    (should (equal (plist-get (car (repo-review--drafts)) :body) "bad wording"))))

(ert-deftest repo-review-ai-check-needs-a-draft ()
  (repo-review-test--with-project
    (repo-review-start)
    (should-error (repo-review-ai-check-draft) :type 'user-error)))

(ert-deftest repo-review-ai-default-without-gptel-is-user-error ()
  (skip-unless (not (locate-library "gptel")))
  (should-error (repo-review--gptel-request "p" #'ignore) :type 'user-error))

(ert-deftest repo-review-hunk-text-is-truncated ()
  (with-temp-buffer
    (insert "@@ -1 +1 @@\n" (make-string 100 ?x) "\n")
    (goto-char (point-min))
    (let ((repo-review-ai-max-chars 20))
      (should (<= (length (repo-review--hunk-text)) 20)))))

(provide 'repo-review-test)
;;; repo-review-test.el ends here
