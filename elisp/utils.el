;;; utils.el --- Utility functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom utility functions and helpers.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'ansi-color)

;;;###autoload
(defun my/display-ansi-colors ()
  "Apply ANSI color codes to the current buffer.
This function processes the entire buffer and interprets any ANSI
escape sequences, rendering the corresponding colors in the buffer.

Useful for viewing logs or other text files that include ANSI
color codes."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; Org-mode utilities
(defcustom my/org-agenda-directories
  '("~/Documents/Notes"
    "~/Dropbox/Notes"
    "~/Dropbox/Documents/Notes")
  "List of directories to search for org agenda files.
Each directory will be searched recursively for .org files."
  :type '(repeat directory)
  :group 'org-agenda)

(defun my/find-org-files-recursively (directory)
  "Find all .org files recursively in DIRECTORY, ignoring hidden folders."
  (when (and directory (file-exists-p directory) (file-directory-p directory))
    (let ((files '()))
      (dolist (file (directory-files-recursively directory "\\.org\\'" t
                                                 (lambda (dir)
                                                   (not (string-match-p "\\(^\\|/\\)\\." (file-name-nondirectory dir))))))
        (when (file-regular-p file)
          (push (file-truename file) files)))
      (nreverse files))))

(defun my/update-org-agenda-files (&optional directories)
  "Update org-agenda-files to include all .org files from multiple directories.
If DIRECTORIES is provided, search those directories.
Otherwise, use `my/org-agenda-directories'."
  (let* ((directories (or directories my/org-agenda-directories))
         (expanded-dirs (mapcar #'expand-file-name directories))
         (valid-dirs (seq-filter #'file-directory-p expanded-dirs))
         (org-files (delete-dups (cl-mapcan #'my/find-org-files-recursively valid-dirs))))
    (when org-files
      (setq org-agenda-files org-files))
    (message "Updated org-agenda-files: %d files found across %d directories"
             (length org-files)
             (length valid-dirs))))

(defun my/setup-org-agenda-files ()
  "Set up dynamic org agenda files updating."
  ;; Initial update
  (my/update-org-agenda-files)

  ;; Update agenda files periodically instead of on every agenda access
  (run-with-idle-timer 300 t #'my/update-org-agenda-files))

;; Window management utilities
;;;###autoload
(defun my/toggle-window-split ()
  "Toggle between horizontal and vertical window split.
When there are exactly 2 windows, switch their layout orientation:
  - Horizontal split (side-by-side) becomes vertical (stacked)
  - Vertical split (stacked) becomes horizontal (side-by-side)

The current buffers, point positions, and window focus are preserved.
If there are not exactly 2 windows, display an error message."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))
    (user-error "Can only toggle split with exactly 2 windows")))

(provide 'utils)
;;; utils.el ends here
