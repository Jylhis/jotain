;;; utils.el --- Utility functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Custom utility functions and helpers.

;;; Code:

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

(defvar org-agenda-files)

(defun my/update-org-agenda-files (&optional directories)
  "Update org-agenda-files to include all .org files from multiple directories.
If DIRECTORIES is provided, search those directories.
Otherwise, use `my/org-agenda-directories'."
  (let* ((directories (or directories my/org-agenda-directories))
         (org-files '())
         (valid-dir-count 0))
    ;; Collect org files from all directories that exist
    (dolist (dir directories)
      (let ((expanded-dir (expand-file-name dir)))
        (when (file-exists-p expanded-dir)
          (setq valid-dir-count (1+ valid-dir-count))
          (setq org-files (nconc org-files
                                 (my/find-org-files-recursively expanded-dir))))))
    ;; Remove duplicates (in case of symlinks or overlapping paths)
    (setq org-files (delete-dups org-files))
    (when org-files
      (setq org-agenda-files org-files))
    (message "Updated org-agenda-files: %d files found across %d directories"
             (length org-files)
             valid-dir-count)))

(defvar org-agenda-files)

(defun my/update-org-agenda-files-async
    (&optional directories)
  "Update org-agenda-files asynchronously using `find' if available.
If DIRECTORIES is provided, search those directories.
Otherwise, use `my/org-agenda-directories'.
Falls back to synchronous `my/update-org-agenda-files' if `find' is unavailable."
  (let* ((directories (or directories my/org-agenda-directories))
         (valid-dirs '()))
    (dolist (dir directories)
      (let ((expanded-dir (expand-file-name dir)))
        (when (file-exists-p expanded-dir)
          (push expanded-dir valid-dirs))))
    (setq valid-dirs (nreverse valid-dirs))
    (if (and valid-dirs (executable-find "find"))
        (let ((output-buffer (generate-new-buffer " *org-agenda-find*"))
              (valid-dir-count (length valid-dirs)))
          (make-process
           :name "org-agenda-find"
           :buffer output-buffer
           :command `("find" "-L" ,@valid-dirs
                      "-type" "d" "-name" ".*" "-prune"
                      "-o" "-type" "f" "-name" "*.org" "-print")
           :sentinel `(lambda (process event)
                        (when (string-match-p "finished" event)
                          (let ((output (with-current-buffer ,output-buffer (buffer-string))))
                            (kill-buffer ,output-buffer)
                            (let* ((files (split-string output "\n" t))
                                   (unique-files (delete-dups (mapcar #'file-truename files))))
                              (when unique-files
                                (setq org-agenda-files unique-files))
                              (message "Updated org-agenda-files async: %d files found across %d directories"
                                       (length unique-files)
                                       ,valid-dir-count)))))))
      (my/update-org-agenda-files directories))))


(defun my/setup-org-agenda-files ()
  "Set up dynamic org agenda files updating."
  ;; Initial update
  (my/update-org-agenda-files)

  ;; Update agenda files periodically instead of on every agenda access
  (run-with-idle-timer 300 t #'my/update-org-agenda-files-async))

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
