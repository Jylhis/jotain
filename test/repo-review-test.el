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

(ert-deftest repo-review-start-not-in-project ()
  (let ((default-directory "/tmp"))
    (should-error (repo-review-start) :type 'user-error)))

(ert-deftest repo-review-entry-file-not-in-queue-buffer ()
  (with-temp-buffer
    (should-error (repo-review--entry-file) :type 'user-error)))

(provide 'repo-review-test)
;;; repo-review-test.el ends here
