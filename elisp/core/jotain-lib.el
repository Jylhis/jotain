;;; jotain-lib.el --- Utility functions for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Common utility functions used throughout Jotain.

;;; Code:

(defun jotain-lib-version<= (min-version)
  "Return non-nil if Emacs version is at least MIN-VERSION.
MIN-VERSION should be a string like \"30.1\"."
  (version<= min-version emacs-version))

(defun jotain-lib-feature-available-p (feature)
  "Return non-nil if FEATURE is available in this Emacs build."
  (featurep feature))

(defun jotain-lib-ensure-directory (dir)
  "Ensure directory DIR exists, creating it if necessary."
  (unless (file-directory-p dir)
    (make-directory dir t)))

(defun jotain-lib-file-string (file)
  "Read and return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun jotain-lib-safe-require (feature)
  "Try to require FEATURE, returning nil on failure without error."
  (condition-case nil
      (require feature)
    (error nil)))

(defun jotain-lib-add-hook-once (hook function)
  "Add FUNCTION to HOOK, but only run it once.
Removes itself from the hook after first execution."
  (let ((wrapper (lambda (&rest args)
                   (apply function args)
                   (remove-hook hook wrapper))))
    (add-hook hook wrapper)))

(defmacro jotain-lib-after (feature &rest body)
  "Execute BODY after FEATURE is loaded.
If FEATURE is already loaded, execute BODY immediately."
  (declare (indent 1))
  `(if (featurep ,feature)
       (progn ,@body)
     (with-eval-after-load ,feature
       ,@body)))

(defun jotain-lib-system-type-p (type)
  "Return non-nil if running on TYPE system.
TYPE can be 'linux, 'darwin, 'windows, etc."
  (eq system-type type))

(defun jotain-lib-linux-p ()
  "Return non-nil if running on Linux."
  (jotain-lib-system-type-p 'gnu/linux))

(defun jotain-lib-macos-p ()
  "Return non-nil if running on macOS."
  (jotain-lib-system-type-p 'darwin))

(defun jotain-lib-windows-p ()
  "Return non-nil if running on Windows."
  (memq system-type '(ms-dos windows-nt cygwin)))

(provide 'jotain-lib)
;;; jotain-lib.el ends here
