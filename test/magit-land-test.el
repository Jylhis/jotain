;;; magit-land-test.el --- ERT tests for magit-land -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;;; Code:

(require 'ert)
(require 'magit-land)

(defmacro magit-land-test--with-repo (&rest body)
  "Run BODY in a repository with two commits above origin/main.
Binds `root'.  The first task commit carries a junit result and a
review note."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "magit-land-" t)))
          (default-directory root)
          (process-environment
           (append '("GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t"
                     "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@t"
                     "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_SYSTEM=/dev/null")
                   process-environment)))
     (unwind-protect
         (progn
           (dolist (args '(("init" "-q" "-b" "main")
                           ("commit" "-q" "--allow-empty" "-m" "base")
                           ("update-ref" "refs/remotes/origin/main" "HEAD")
                           ("commit" "-q" "--allow-empty" "-m" "first task")
                           ("notes" "--ref" "checks" "add" "-m" "junit: eA==")
                           ("notes" "--ref" "review" "add" "-m" "check the retry path")
                           ("commit" "-q" "--allow-empty" "-m" "second task")))
             (apply #'call-process "git" nil nil nil args))
           ,@body)
       (when (get-buffer (format "*magit-land: %s*"
                                 (file-name-nondirectory
                                  (directory-file-name root))))
         (kill-buffer (format "*magit-land: %s*"
                              (file-name-nondirectory
                               (directory-file-name root)))))
       (delete-directory root t))))

(ert-deftest magit-land-renders-commits-and-notes ()
  (magit-land-test--with-repo
    (magit-land)
    (let ((text (buffer-string)))
      (should (string-match-p "against origin/main" text))
      (should (string-match-p "2 commits to land" text))
      (should (string-match-p "second task\n    checks: none" text))
      (should (string-match-p "first task\n    checks: junit\n    review: check the retry path"
                              text))
      (should-not (string-match-p "\\bbase\\b" text)))))

(ert-deftest magit-land-commit-at-point ()
  (magit-land-test--with-repo
    (magit-land)
    (goto-char (point-min))
    (re-search-forward "first task")
    (should (equal (magit-land--commit-at-point)
                   (string-trim (shell-command-to-string
                                 "git rev-parse --short HEAD~1"))))
    (forward-line 2)
    (should (equal (magit-land--commit-at-point)
                   (string-trim (shell-command-to-string
                                 "git rev-parse --short HEAD~1"))))))

(ert-deftest magit-land-keys ()
  (should (eq (lookup-key magit-land-mode-map "L") 'magit-land-land))
  (should (eq (lookup-key magit-land-mode-map "S") 'magit-land-show-results))
  (should (eq (lookup-key magit-land-mode-map "D") 'magit-land-remove-worktree)))

(ert-deftest magit-land-reports-a-missing-base ()
  (magit-land-test--with-repo
    (let ((magit-land-base "origin/missing"))
      (magit-land)
      (should (string-match-p "No ref origin/missing" (buffer-string))))))

(provide 'magit-land-test)
;;; magit-land-test.el ends here
