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

;; Obsidian utilities
;; (defun my/obsidian-vault-exists-p ()
;;   "Check if the configured Obsidian vault directory exists."
;;   (and (boundp 'obsidian-vault-directory)
;;        obsidian-vault-directory
;;        (file-directory-p obsidian-vault-directory)))

;; (defun my/find-obsidian-files-recursively (directory)
;;   "Find all .md files recursively in Obsidian DIRECTORY."
;;   (when (and directory (file-exists-p directory) (file-directory-p directory))
;;     (let ((files '()))
;;       (dolist (file (directory-files-recursively directory "\\.md\\'" t
;;                                                  (lambda (dir)
;;                                                    (not (string-match-p "\\(^\\|/\\)\\." (file-name-nondirectory dir))))))
;;         (when (file-regular-p file)
;;           (push (file-truename file) files)))
;;       (nreverse files))))

;; (defun my/obsidian-daily-note ()
;;   "Create or open today's daily note in Obsidian vault."
;;   (interactive)
;;   (if (my/obsidian-vault-exists-p)
;;       (let* ((today (format-time-string "%Y-%m-%d"))
;;              (daily-notes-dir (expand-file-name "Daily Notes" obsidian-vault-directory))
;;              (daily-note-file (expand-file-name (concat today ".md") daily-notes-dir)))
;;         (unless (file-directory-p daily-notes-dir)
;;           (make-directory daily-notes-dir t))
;;         (find-file daily-note-file)
;;         (when (zerop (buffer-size))
;;           (insert (format "# %s\n\n" today))
;;           (save-buffer)))
;;     (message "Obsidian vault directory not found or not configured")))

;; Org-mode utilities
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

(defun my/update-org-agenda-files ()
  "Update org-agenda-files to include all .org files under Documents."
  (let ((org-files (my/find-org-files-recursively (expand-file-name org-directory))))
    (when org-files
      (setq org-agenda-files org-files))
    (message "Updated org-agenda-files: %d files found" (length org-files))))

(defun my/setup-org-agenda-files ()
  "Set up dynamic org agenda files updating."
  ;; Initial update
  (my/update-org-agenda-files)
  
  ;; Update agenda files periodically instead of on every agenda access
  (run-with-idle-timer 300 t #'my/update-org-agenda-files))

(provide 'utils)
;;; utils.el ends here
