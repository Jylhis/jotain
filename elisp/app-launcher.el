;;; app-launcher.el --- Launch applications from Emacs -*- lexical-binding: t -*-

;; Author: Sebastien Waegeneire
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/sebastienwae/app-launcher

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; app-launcher define the `app-launcher-run-app' command which uses
;; Emacs standard completion feature to select an application installed
;; on your machine and launch it.

;;; Acknowledgements:

;; This package uses code from the Counsel package by Oleh Krehel.
;; https://github.com/abo-abo/swiper

(require 'xdg)
(require 'cl-seq)

(defcustom app-launcher-apps-directories
  (mapcar (lambda (dir) (expand-file-name "applications" dir))
	  (cons (xdg-data-home)
		(xdg-data-dirs)))
  "Directories in which to search for applications (.desktop files)."
  :type '(repeat directory))

(defcustom app-launcher--annotation-function #'app-launcher--annotation-function-default
  "Define the function that genereate the annotation for each completion choices."
  :type 'function)

(defcustom app-launcher--action-function #'app-launcher--action-function-default
  "Define the function that is used to run the selected application."
  :type 'function)

(defconst app-launcher--awk-script
  "BEGIN { FS=\"=\" }
FNR==1 {
    if (NR > 1) {
        if (is_app && name != \"\" && exec_cmd != \"\") {
             printf \"%s\\0%s\\0%s\\0%s\\0%s\\0%s\\0\", last_file, name, exec_cmd, comment, visible, try_exec
        } else if (last_file != \"\") {
             if (has_entry_group == 0) {
                 printf \"%s\\0ERROR\\0no [Desktop Entry] group\\0\\0\\0\\0\", last_file
             } else if (name == \"\") {
                 printf \"%s\\0ERROR\\0no Name\\0\\0\\0\\0\", last_file
             }
        }
    }
    last_file = FILENAME
    in_entry = 0
    is_app = 0
    has_entry_group = 0
    name = \"\"
    exec_cmd = \"\"
    comment = \"\"
    visible = \"t\"
    try_exec = \"\"
}

/^ *\\[Desktop Entry\\] *$/ { in_entry = 1; has_entry_group = 1; next }
/^ *\\[/ { in_entry = 0 }

in_entry {
    if (/^ *Type *= *Application *$/) { is_app = 1 }
    else if (/^ *Name *=/) { sub(/^ *Name *= */, \"\"); name = $0 }
    else if (/^ *Exec *=/) { sub(/^ *Exec *= */, \"\"); exec_cmd = $0 }
    else if (/^ *Comment *=/) { sub(/^ *Comment *= */, \"\"); comment = $0 }
    else if (/^ *(Hidden|NoDisplay) *= *(true|1) *$/) { visible = \"nil\" }
    else if (/^ *TryExec *=/) { sub(/^ *TryExec *= */, \"\"); try_exec = $0 }
}

END {
    if (is_app && name != \"\" && exec_cmd != \"\") {
         printf \"%s\\0%s\\0%s\\0%s\\0%s\\0%s\\0\", last_file, name, exec_cmd, comment, visible, try_exec
    } else if (last_file != \"\") {
         if (has_entry_group == 0) {
             printf \"%s\\0ERROR\\0no [Desktop Entry] group\\0\\0\\0\\0\", last_file
         } else if (name == \"\") {
             printf \"%s\\0ERROR\\0no Name\\0\\0\\0\\0\", last_file
         }
    }
}"
  "Awk script to parse .desktop files.")

(defvar app-launcher--cache nil
  "Cache of desktop files data.")

(defvar app-launcher--cache-timestamp nil
  "Time when we last updated the cached application list.")

(defvar app-launcher--cached-files nil
  "List of cached desktop files.")

(defun app-launcher-list-desktop-files ()
  "Return an alist of all Linux applications.
Each list entry is a pair of (desktop-name . desktop-file).
This function always returns its elements in a stable order."
  (let ((hash (make-hash-table :test #'equal))
	result)
    (dolist (dir app-launcher-apps-directories)
      (when (file-exists-p dir)
	(let ((dir (file-name-as-directory dir)))
	  (dolist (file (directory-files-recursively dir ".*\\.desktop$"))
	    (let ((id (subst-char-in-string ?/ ?- (file-relative-name file dir))))
	      (when (and (not (gethash id hash)) (file-readable-p file))
		(push (cons id file) result)
		(puthash id file hash)))))))
    result))

(defun app-launcher-parse-files (files)
  "Parse the .desktop files to return usable informations."
  (let ((hash (make-hash-table :test #'equal))
        (file-list (mapcar #'cdr files)))
    (when file-list
      (with-temp-buffer
        (mapc (lambda (f) (insert (expand-file-name f) "\0")) file-list)
        (call-process-region (point-min) (point-max) "xargs" t t nil "-0" "awk" app-launcher--awk-script)
        (goto-char (point-min))
        (let (file name exec comment visible try-exec)
          (while (not (eobp))
            (setq file (buffer-substring-no-properties (point) (1- (search-forward "\0")))
                  name (buffer-substring-no-properties (point) (1- (search-forward "\0")))
                  exec (buffer-substring-no-properties (point) (1- (search-forward "\0")))
                  comment (buffer-substring-no-properties (point) (1- (search-forward "\0")))
                  visible (buffer-substring-no-properties (point) (1- (search-forward "\0")))
                  try-exec (buffer-substring-no-properties (point) (1- (search-forward "\0"))))
            (if (string= name "ERROR")
                (progn
                  (message "Warning: File %s has %s" file exec)
                  (when (boundp 'counsel-linux-apps-faulty)
                    (push file counsel-linux-apps-faulty)))
              (when (or (string= try-exec "")
                        (locate-file try-exec exec-path nil #'file-executable-p))
                (puthash name
                         (list (cons 'file file)
                               (cons 'exec exec)
                               (cons 'comment comment)
                               (cons 'visible (string= visible "t")))
                         hash)))))))
    hash))

(defun app-launcher-list-apps ()
  "Return list of all Linux .desktop applications."
  (let* ((new-desktop-alist (app-launcher-list-desktop-files))
	 (new-files (mapcar 'cdr new-desktop-alist)))
    (unless (and (equal new-files app-launcher--cached-files)
		 (null (cl-find-if
			(lambda (file)
			  (time-less-p
			   app-launcher--cache-timestamp
			   (nth 5 (file-attributes file))))
			new-files)))
      (setq app-launcher--cache (app-launcher-parse-files new-desktop-alist))
      (setq app-launcher--cache-timestamp (current-time))
      (setq app-launcher--cached-files new-files)))
  app-launcher--cache)

(defun app-launcher--annotation-function-default (choice)
  "Default function to annotate the completion choices."
  (let ((str (cdr (assq 'comment (gethash choice app-launcher--cache)))))
    (when str (concat " - " (propertize str 'face 'completions-annotations)))))

(defun app-launcher--action-function-default (selected)
  "Default function used to run the selected application."
  (let* ((exec (cdr (assq 'exec (gethash selected app-launcher--cache))))
	 (command (let (result)
		    (dolist (chunk (split-string exec " ") result)
		      (unless (or (equal chunk "%U")
				  (equal chunk "%F")
				  (equal chunk "%u")
				  (equal chunk "%f"))
			(setq result (concat result chunk " ")))))))
    (call-process-shell-command command nil 0 nil)))

;;;###autoload
(defun app-launcher-run-app (&optional arg)
  "Launch an application installed on your machine.
When ARG is non-nil, ignore NoDisplay property in *.desktop files."
  (interactive)
  (let* ((candidates (app-launcher-list-apps))
	 (result (completing-read
		  "Run app: "
		  (lambda (str pred flag)
		    (if (eq flag 'metadata)
			'(metadata
			  (annotation-function . (lambda (choice)
						   (funcall
						    app-launcher--annotation-function
						    choice))))
		      (complete-with-action flag candidates str pred)))
		  (lambda (x y)
		    (if arg
			t
		      (cdr (assq 'visible y))))
		  t nil 'app-launcher nil nil)))
    (funcall app-launcher--action-function result)))

;; Provide the app-launcher feature
(provide 'app-launcher)
