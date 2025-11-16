;;; test-jotain.el --- Essential smoke tests for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas
;; Author: Markus Jylhänkangas <markus@jylhis.com>

;;; Commentary:
;; Essential smoke tests that validate Jotain loads correctly.
;; These tests should complete in under 1 second.

;;; Code:

(require 'ert)

;;; Environment

(ert-deftest test-jotain/emacs-version ()
  "Test Emacs version meets minimum requirement (30.1+)."
  :tags '(smoke critical)
  (should (version<= "30.1" emacs-version)))

(ert-deftest test-jotain/lexical-binding ()
  "Test lexical binding is enabled."
  :tags '(smoke critical)
  (should lexical-binding))

;;; Structure

(ert-deftest test-jotain/elisp-directory-exists ()
  "Test that elisp/ directory exists."
  :tags '(smoke critical)
  (let ((elisp-dir (expand-file-name "elisp" user-emacs-directory)))
    (should (file-directory-p elisp-dir))))

(ert-deftest test-jotain/init-exists ()
  "Test that init.el exists."
  :tags '(smoke critical)
  (let ((init-file (expand-file-name "init.el" user-emacs-directory)))
    (should (file-exists-p init-file))))

(ert-deftest test-jotain/early-init-exists ()
  "Test that early-init.el exists."
  :tags '(smoke critical)
  (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
    (should (file-exists-p early-init-file))))

;;; Module Loading

(ert-deftest test-jotain/main-module-loads ()
  "Test that jotain.el loads without errors."
  :tags '(smoke critical)
  (should (require 'jotain nil t)))

(ert-deftest test-jotain/core-module-loads ()
  "Test that jotain-core.el loads."
  :tags '(smoke)
  (should (require 'jotain-core nil t)))

(ert-deftest test-jotain/editor-module-loads ()
  "Test that jotain-editor.el loads."
  :tags '(smoke)
  (should (require 'jotain-editor nil t)))

(ert-deftest test-jotain/prog-module-loads ()
  "Test that jotain-prog.el loads."
  :tags '(smoke)
  (should (require 'jotain-prog nil t)))

(ert-deftest test-jotain/tools-module-loads ()
  "Test that jotain-tools.el loads."
  :tags '(smoke)
  (should (require 'jotain-tools nil t)))

;;; Basic Functionality

(ert-deftest test-jotain/buffer-operations ()
  "Test basic buffer operations work."
  :tags '(smoke)
  (with-temp-buffer
    (should (bufferp (current-buffer)))
    (insert "test")
    (should (= (point-max) 5))))

(ert-deftest test-jotain/feature-provided ()
  "Test that jotain feature is provided."
  :tags '(smoke)
  (should (featurep 'jotain)))

(provide 'test-jotain)
;;; test-jotain.el ends here
