;;; jotain-utils.el --- Utility functions for Jotain -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; URL: https://github.com/jylhis/jotain

;;; Commentary:

;; Small helpers that don't justify their own module.
;;
;; Currently:
;;
;; - `jotain-utils-display-ansi-colors' — interactively render ANSI
;;   escape sequences in the current buffer (useful for log files).
;; - `jotain-utils-toggle-window-split' — swap horizontal/vertical
;;   split when the frame has exactly two windows.
;; - `jotain-utils-auto-create-missing-dirs' — intended for
;;   `find-file-not-found-functions'; silently creates the parent
;;   directory of a file being visited for the first time.
;; - An org-agenda directory scanner cluster
;;   (`jotain-utils-update-org-agenda-files' and friends) for
;;   projects that keep notes split across several directory trees.
;;   These are dormant until something calls
;;   `jotain-utils-setup-org-agenda-files'.

;;; Code:

(require 'ansi-color)

;; Forward declaration so the byte-compiler doesn't warn about
;; assigning to a free variable when org-agenda hasn't loaded yet.
(defvar org-agenda-files)

(defgroup jotain-utils nil
  "Miscellaneous utilities for Jotain."
  :group 'jotain
  :prefix "jotain-utils-")

;;;; ANSI colors

;;;###autoload
(defun jotain-utils-display-ansi-colors ()
  "Render ANSI escape sequences in the current buffer.
Useful for viewing log files produced by tools that emit color
codes unconditionally."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;;;; Org agenda directory scanner
;;
;; Dormant until `jotain-utils-setup-org-agenda-files' is called
;; (typically from org / writing module setup).

(defcustom jotain-utils-org-agenda-directories
  '("~/Documents/Notes"
    "~/Dropbox/Notes"
    "~/Dropbox/Documents/Notes")
  "Directories scanned recursively for Org agenda files.
Missing directories are skipped silently."
  :type '(repeat directory)
  :group 'jotain-utils)

(defcustom jotain-utils-org-agenda-refresh-interval 300
  "Seconds between idle refreshes of `org-agenda-files'.
Only consulted by `jotain-utils-setup-org-agenda-files'."
  :type 'integer
  :group 'jotain-utils)

(defun jotain-utils-find-org-files-recursively (directory)
  "Return all .org files under DIRECTORY, skipping hidden folders.
Returns nil if DIRECTORY does not exist or is not a directory."
  (when (and directory
             (file-exists-p directory)
             (file-directory-p directory))
    (let (files)
      (dolist (file (directory-files-recursively
                     directory "\\.org\\'" t
                     (lambda (dir)
                       (not (string-match-p
                             "\\(^\\|/\\)\\."
                             (file-name-nondirectory dir))))))
        (when (file-regular-p file)
          (push (file-truename file) files)))
      (nreverse files))))

(defun jotain-utils-update-org-agenda-files (&optional directories)
  "Refresh `org-agenda-files' from DIRECTORIES (or the defcustom).
Sets `org-agenda-files' to the deduplicated list of .org files
found.  Does nothing if no files are found.  Echoes a count."
  (interactive)
  (let* ((dirs (or directories jotain-utils-org-agenda-directories))
         (files nil)
         (valid-dirs 0))
    (dolist (dir dirs)
      (when-let* ((found (jotain-utils-find-org-files-recursively
                          (expand-file-name dir))))
        (setq valid-dirs (1+ valid-dirs)
              files (nconc files found))))
    (setq files (delete-dups files))
    (when files
      (setq org-agenda-files files))
    (message "jotain-utils: %d org files across %d directories"
             (length files) valid-dirs)))

(defvar jotain-utils--org-agenda-refresh-timer nil
  "Idle timer refreshing `org-agenda-files', or nil.")

(defun jotain-utils-setup-org-agenda-files ()
  "Scan now and schedule periodic refresh of `org-agenda-files'.
Cancels any existing refresh timer before rescheduling."
  (jotain-utils-update-org-agenda-files)
  (when (timerp jotain-utils--org-agenda-refresh-timer)
    (cancel-timer jotain-utils--org-agenda-refresh-timer))
  (setq jotain-utils--org-agenda-refresh-timer
        (run-with-idle-timer jotain-utils-org-agenda-refresh-interval t
                             #'jotain-utils-update-org-agenda-files)))

;;;; Window management

;;;###autoload
(defun jotain-utils-toggle-window-split ()
  "Swap horizontal and vertical split orientation.
Requires exactly two windows.  Buffer contents, point positions
and window focus are preserved."
  (interactive)
  (unless (= (count-windows) 2)
    (user-error "Can only toggle split with exactly 2 windows"))
  (let* ((this-buf (window-buffer))
         (next-buf (window-buffer (next-window)))
         (this-edges (window-edges (selected-window)))
         (next-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-edges) (car next-edges))
                                 (<= (cadr this-edges) (cadr next-edges)))))
         (splitter (if (= (car this-edges)
                          (car (window-edges (next-window))))
                       #'split-window-horizontally
                     #'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (when this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-buf)
      (set-window-buffer (next-window) next-buf)
      (select-window first-win)
      (when this-win-2nd (other-window 1)))))

;;;; find-file helpers

;;;###autoload
(defun jotain-utils-auto-create-missing-dirs ()
  "Silently create the parent directory of a file being visited.
Intended for `find-file-not-found-functions'.  Always returns
nil so the rest of the chain still runs and `find-file' continues
to offer the usual \"new file\" buffer."
  (let ((target-dir (and buffer-file-name
                         (file-name-directory buffer-file-name))))
    (when (and target-dir (not (file-exists-p target-dir)))
      (make-directory target-dir t)))
  nil)

(provide 'jotain-utils)
;;; jotain-utils.el ends here
