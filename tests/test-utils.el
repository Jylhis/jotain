;;; test-utils.el --- Tests for utils.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for utility functions in utils.el using ERT.
;; Tests are tagged for performance: fast vs filesystem operations.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'utils)

;; Mock org variables for testing (avoid loading org-mode)
(defvar org-directory)
(defvar org-agenda-files)
(setq org-directory (expand-file-name "~/Documents"))
(setq org-agenda-files nil)

;;; Fast Tests (no I/O)

(ert-deftest test-utils/find-org-files-nonexistent-directory ()
  "Test that my/find-org-files-recursively returns nil for nonexistent directory."
  :tags '(unit utils fast)
  (let ((result (my/find-org-files-recursively "/nonexistent/directory/that/does/not/exist")))
    (should (null result))))

(ert-deftest test-utils/find-org-files-nil-input ()
  "Test that my/find-org-files-recursively handles nil input gracefully."
  :tags '(unit utils fast)
  (let ((result (my/find-org-files-recursively nil)))
    (should (null result))))

(ert-deftest test-utils/update-org-agenda-files-nonexistent ()
  "Test that my/update-org-agenda-files preserves agenda files when directories don't exist."
  :tags '(unit utils fast)
  (let ((original-org-agenda-files org-agenda-files))
    (unwind-protect
        (progn
          (setq org-agenda-files '("dummy-file.org"))
          (my/update-org-agenda-files '("/nonexistent/directory"))
          (should (equal org-agenda-files '("dummy-file.org"))))
      (setq org-agenda-files original-org-agenda-files))))

;;; Filesystem Tests (minimal I/O)

(ert-deftest test-utils/find-org-files-empty-directory ()
  "Test that my/find-org-files-recursively returns nil for empty directory."
  :tags '(unit utils filesystem)
  (let ((temp-dir (make-temp-file "emacs-test-empty-" t)))
    (unwind-protect
        (let ((result (my/find-org-files-recursively temp-dir)))
          (should (null result)))
      (delete-directory temp-dir t))))

(ert-deftest test-utils/find-org-files-file-input ()
  "Test that my/find-org-files-recursively returns nil when given a file instead of directory."
  :tags '(unit utils filesystem)
  (let ((temp-file (make-temp-file "emacs-test-file-" nil ".org")))
    (unwind-protect
        (let ((result (my/find-org-files-recursively temp-file)))
          (should (null result)))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Filesystem Tests (full directory structure - SLOW)

(defun test-utils--create-simple-structure ()
  "Create minimal test structure (faster than full structure)."
  (let* ((temp-dir (make-temp-file "emacs-test-" t))
         (subdir (expand-file-name "subdir" temp-dir)))
    (make-directory subdir t)
    (with-temp-file (expand-file-name "test1.org" temp-dir)
      (insert "* Test 1\n"))
    (with-temp-file (expand-file-name "test2.org" subdir)
      (insert "* Test 2\n"))
    temp-dir))

(ert-deftest test-utils/find-org-files-basic ()
  "Test that my/find-org-files-recursively finds org files."
  :tags '(unit utils filesystem slow)
  (let ((temp-dir (test-utils--create-simple-structure)))
    (unwind-protect
        (let ((org-files (my/find-org-files-recursively temp-dir)))
          (should (= (length org-files) 2))
          (should (cl-some (lambda (f) (string-match-p "test1\\.org$" f)) org-files))
          (should (cl-some (lambda (f) (string-match-p "test2\\.org$" f)) org-files))
          (should (cl-every #'file-exists-p org-files)))
      (delete-directory temp-dir t))))

(ert-deftest test-utils/find-org-files-ignores-hidden ()
  "Test that my/find-org-files-recursively ignores hidden folders."
  :tags '(unit utils filesystem slow)
  (let* ((temp-dir (make-temp-file "emacs-test-" t))
         (hidden-dir (expand-file-name ".hidden" temp-dir)))
    (unwind-protect
        (progn
          (make-directory hidden-dir t)
          (with-temp-file (expand-file-name "visible.org" temp-dir)
            (insert "* Visible\n"))
          (with-temp-file (expand-file-name "hidden.org" hidden-dir)
            (insert "* Hidden\n"))
          (let ((org-files (my/find-org-files-recursively temp-dir)))
            (should (= (length org-files) 1))
            (should-not (cl-some (lambda (f) (string-match-p "hidden\\.org$" f)) org-files))))
      (delete-directory temp-dir t))))

(ert-deftest test-utils/update-org-agenda-files-integration ()
  "Test that my/update-org-agenda-files correctly updates agenda files."
  :tags '(integration utils filesystem slow)
  (let ((temp-dir (test-utils--create-simple-structure))
        (original-org-agenda-files org-agenda-files))
    (unwind-protect
        (progn
          (setq org-agenda-files nil)
          (my/update-org-agenda-files (list temp-dir))
          (should (= (length org-agenda-files) 2))
          (should (cl-every #'file-exists-p org-agenda-files)))
      (setq org-agenda-files original-org-agenda-files)
      (delete-directory temp-dir t))))

;; Symlink test - platform specific and slow
(ert-deftest test-utils/find-org-files-symlink-handling ()
  "Test that my/find-org-files-recursively handles symbolic links correctly."
  :tags '(unit utils filesystem slow platform-specific)
  (skip-unless (eq system-type 'gnu/linux))
  (let ((temp-dir (make-temp-file "emacs-test-symlink-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "real.org" temp-dir)
            (insert "* Real org file\n"))
          (let ((link-path (expand-file-name "link.org" temp-dir)))
            (make-symbolic-link (expand-file-name "real.org" temp-dir) link-path)
            (let ((result (my/find-org-files-recursively temp-dir)))
              (should (>= (length result) 1))
              (should (cl-some (lambda (file) (string-match-p "real\\.org$" file)) result)))))
      (delete-directory temp-dir t))))

(provide 'test-utils)
;;; test-utils.el ends here
