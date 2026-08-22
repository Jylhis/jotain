;;; elisp-doc-index.el --- Function documentation batch generation -*- lexical-binding: t -*-

;; Copyright (C) 2026 gudzpoz

;; Author: gudzpoz
;; Version: 0.1

;;; Commentary:
;;

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;

;;; Code:

(require 'elisp-doc-extract)

(defvar elisp-doc--debug-quick nil)

(defvar elisp-doc--cache-shard nil)
(defvar elisp-doc--cache-shards nil)

(defvar elisp-doc--source-files (make-hash-table :test 'equal))
(defvar elisp-doc--export-current nil)
(defvar elisp-doc--export-errors nil)
(defvar elisp-doc--export-remaining nil)
(defun elisp-doc--export-retry ()
  (dolist (err elisp-doc--export-errors) (push (car err) elisp-doc--export-remaining))
  (setq elisp-doc--export-errors nil))
(defun elisp-doc--export-reset ()
  (message "current: %S\nremaining: %S\nerrors: %S"
           elisp-doc--export-current elisp-doc--export-remaining elisp-doc--export-errors)
  (setq elisp-doc--export-current nil
        elisp-doc--export-remaining nil
        elisp-doc--export-errors nil))

(defun elisp-doc--portable-hash (symbol)
  (let ((s (symbol-name symbol))
        (i 0) (hash 0))
    (while (< i (length s))
      (setq hash (+ (* 31 hash) (aref s i))
            i (1+ i)))
    hash))

(defvar elisp-doc--exporting-buffer nil
  "Tracking the current buffer so that we can kill them even on failures.")
(defun elisp-doc--export-or-cache-one (helpful-func symbol)
  (let* ((entry (make-elisp-doc-entry
                 :symbol symbol :type helpful-func :buffer nil :sections nil))
         (dest (elisp-doc-entry-output-path entry))
         (inhibit-message t))
    (unless (file-exists-p dest)
      (setq entry (elisp-doc-extract-helpful helpful-func symbol))
      (setq elisp-doc--exporting-buffer (elisp-doc-entry-buffer entry))
      (elisp-doc-export entry))
    entry))

(defun elisp-doc--export-all (pred helpful-func type)
  (if (or elisp-doc--export-remaining elisp-doc--export-current)
      (progn
        (when elisp-doc--export-current
          (push elisp-doc--export-current elisp-doc--export-remaining)
          (setq elisp-doc--export-current nil))
        (message "continue: %d left" (length elisp-doc--export-remaining)))
    (mapatoms (lambda (s)
                (when (and (funcall pred s)
                           (not (string-prefix-p "elisp-doc-" (symbol-name s)))
                           (or (not elisp-doc--cache-shards)
                               (equal elisp-doc--cache-shard
                                      (% (elisp-doc--portable-hash s)
                                         elisp-doc--cache-shards))))
                  (push s elisp-doc--export-remaining)))))
  (when (and (numberp elisp-doc--cache-shard)
             (= elisp-doc--cache-shard elisp-doc--cache-shards))
    (elisp-doc--export-reset))
  (let ((elisp-doc--count 0) (total (length elisp-doc--export-remaining)) (i 0) symbol)
    (while elisp-doc--export-remaining
      (setq symbol (pop elisp-doc--export-remaining)
            elisp-doc--export-current symbol)
      (setq i (1+ i))
      (when (or (not elisp-doc--debug-quick)
                (< (setq elisp-doc--count (1+ elisp-doc--count))
                   elisp-doc--debug-quick))
        (elisp-doc--log "%2s(%6d/%6d) %S" (or elisp-doc--cache-shard "") i total symbol)
        (when-let* ((entry (condition-case err
                               (elisp-doc--export-or-cache-one helpful-func symbol)
                             (error
                              (push (cons symbol err) elisp-doc--export-errors)
                              (message "%s" (error-message-string err))
                              nil)))
                    (symbol (elisp-doc-entry-symbol entry)))
          (push (cons helpful-func symbol)
                (gethash (or (find-lisp-object-file-name symbol type)
                             (if (eq type 'defun) (help-C-file-name symbol 'subr)))
                         elisp-doc--source-files))))
      (setq elisp-doc--export-current nil)
      (when elisp-doc--exporting-buffer
        (kill-buffer elisp-doc--exporting-buffer)
        (setq elisp-doc--exporting-buffer nil)))
    (when elisp-doc--cache-shard
      (with-temp-file (file-name-concat elisp-doc-cache-dir
                                        (format "%d.listing" elisp-doc--cache-shard))
        (prin1 elisp-doc--source-files (current-buffer))))))

(defun elisp-doc--load-cache ()
  (cl-assert (zerop (hash-table-count elisp-doc--source-files)))
  (dolist (listing (file-expand-wildcards (file-name-concat elisp-doc-cache-dir "*.listing")))
    (let ((cached (with-temp-buffer
                    (insert-file-contents listing)
                    (goto-char (point-min))
                    (read (current-buffer)))))
      (maphash (lambda (k v)
                 (puthash k (nconc v (gethash k elisp-doc--source-files))
                          elisp-doc--source-files))
               cached))))

(defun elisp-doc--please-noninteractive (&rest _)
  "An advice to abort when interactivity is wanted when `noninteractive'."
  (when noninteractive
    (error "Please don't.")))
(advice-add #'y-or-n-p :before #'elisp-doc--please-noninteractive)
(advice-add #'yes-or-no-p :before #'elisp-doc--please-noninteractive)

(defun elisp-doc--functionp (symbol)
  (when (or (special-form-p symbol) (functionp symbol) (macrop symbol))
    (if (not (autoloadp (symbol-function symbol)))
        (documentation symbol t)
      (ignore-error error
        (autoload-do-load (symbol-function symbol) symbol))
      (if (not (autoloadp (symbol-function symbol)))
          (documentation symbol t)
        (message "autoload failed: %S" symbol)
        (push (list symbol 'autoload-do-load) elisp-doc--export-errors)))))

(defun elisp-doc--variablep (symbol)
  (and (boundp symbol)
       (not (keywordp symbol))
       (documentation-property symbol 'variable-documentation t)))

(defsubst elisp-doc--stylized-name (symbol type)
  (if (eq type #'describe-face)
      (with-temp-buffer
        (insert (symbol-name symbol))
        (put-text-property (point-min) (point-max) 'face symbol)
        (let* ((inhibit-message t)
               (text (or (ignore-error error
                           (htmlize-region-for-paste (point-min) (point-max)))
                         (buffer-string))))
          (substring text
                     (if (string-prefix-p "<pre>" text) (length "<pre>") 0)
                     (if (string-suffix-p "</pre>" text) (- (length "</pre>"))))))
    (symbol-name symbol)))

(defun elisp-doc--create-json-index ()
  (let (fun var face)
    (maphash (lambda (_file items)
               (dolist (pair items)
                 (let ((ty (car pair))
                       (sym (symbol-name (cdr pair))))
                   (cond
                    ((eq ty #'helpful-function)
                     (push sym fun))
                    ((eq ty #'helpful-variable)
                     (push sym var))
                    ((eq ty #'describe-face)
                     (push sym face))))))
             elisp-doc--source-files)
    (with-temp-file (file-name-concat elisp-doc-output-dir "symbols.json")
      (json-insert `(fun ,(vconcat fun) var ,(vconcat var) face ,(vconcat face))))))

(defun elisp-doc--create-type-index (type)
  (let ((title (elisp-doc-entry-type-desc type))
        (name (elisp-doc-entry-type-short type))
        entries)
    (maphash (lambda (file items)
               (dolist (pair items)
                 (let ((ty (car pair)))
                   (when (eq ty type)
                     (push (cons file (cdr pair)) entries)))))
             elisp-doc--source-files)
    (setq entries (sort entries (lambda (a b) (string-lessp (cdr a) (cdr b)))))
    (with-temp-file (file-name-concat elisp-doc-output-dir (concat name ".html"))
      (insert "<div class=\"table\"><table id=\"index\"><tbody>")
      (dolist (entry entries)
        (let ((file (car entry))
              (sym (cdr entry)))
          (insert "<tr><td><a href=\"./" name
                  "/" (url-hexify-string (elisp-doc--encode-url-part (symbol-name sym) t))
                  ".html\"><code>" (elisp-doc--stylized-name sym type)
                  "</code></a></td><td><a href=\""
                  (let ((url (htmlize-attr-escape (string-trim-left
                                                   (or (elisp-doc--get-source-file file) "#")
                                                   "\\.+"))))
                    (if (string-prefix-p "/" url) (concat "." url) url))
                  "\"><code>" (if file (htmlize-protect-string (file-name-nondirectory file)) "/")
                  "</code></a></td></tr>")))
      (insert "</tbody></table></div>")
      (elisp-doc-wrap-page-body
       (concat (capitalize title) " Index")
       (format "%s Index (%d)" (capitalize title) (length entries))
       (format "Index page: all %d %s symbols from Emacs, documentation links, and source file locations."
               (length entries) title)))))

(defun elisp-doc--write-file-section (rel items type)
  (when (length> items 0)
    (let ((title (elisp-doc-entry-type-desc type))
          (name (elisp-doc-entry-type-short type))
          m (count 0))
      (insert "<h2>Defined " title "s</h2>\n")
      (setq m (elisp-doc--make-marker (- (point) (length "</h2>\n"))))
      (insert "<div class=\"table\"><table><tbody>")
      (dolist (pair items)
        (let ((ty (car pair))
              (sym (cdr pair)))
          (when (eq ty type)
            (setq count (1+ count))
            (insert "<tr><td><a href=\"" rel name
                    "/" (url-hexify-string (elisp-doc--encode-url-part (symbol-name sym) t))
                    ".html\"><code>" (elisp-doc--stylized-name sym type)
                    "</code></a></td><td>"
                    (or (ignore-error error
                          (htmlize-protect-string
                           (cond
                            ((eq type 'helpful-variable) (elisp-get-var-docstring sym))
                            ((eq type 'helpful-function) (elisp-get-fnsym-args-string sym))
                            ((eq type 'describe-face) (face-documentation sym)))))
                        "")
                    "</td></tr>"))))
      (insert "</tbody></table></div>")
      (save-excursion
        (goto-char m)
        (insert (format " (%d)" count))))))

(defun elisp-doc--format-commentary (string)
  (with-temp-buffer
    (insert (helpful--format-docstring string))
    (let ((elisp-doc--convert-steps
           (remq 'elisp-doc--prune-headings elisp-doc--convert-steps))
          (entry (make-elisp-doc-entry
                  :symbol nil :type nil
                  :buffer (current-buffer)
                  :sections (list (make-elisp-doc-entry-section
                                   :heading "Documentation"
                                   :heading-marker (point-min-marker)
                                   :start (point-min-marker)
                                   :end (point-max-marker))))))
      (elisp-doc--run-steps entry))
    (buffer-string)))

(defun elisp-doc--create-file-index (file items)
  (with-temp-buffer
    (setq items (sort items (lambda (a b) (string-lessp (cdr a) (cdr b)))))
    (let ((rel (elisp-doc--get-source-file file)) (level 0) path name)
      (when (string-prefix-p "." rel)
        (setq rel (string-trim-left rel "\\.+")
              path (file-name-concat elisp-doc-output-dir rel)
              name (file-name-nondirectory rel))
        (insert (elisp-doc--format-commentary
                 (save-window-excursion
                   (save-excursion
                     (let ((inhibit-message t))
                       (condition-case err
                           (finder-commentary file)
                         (error (error-message-string err))))
                     (buffer-string)))))
        (mapc (lambda (c) (if (= c ?/) (setq level (1+ level)))) rel)
        (let ((rel (apply #'concat (make-list (1- level) "../"))))
          (elisp-doc--write-file-section rel items #'helpful-variable)
          (elisp-doc--write-file-section rel items #'helpful-function)
          (elisp-doc--write-file-section rel items #'describe-face))
        (elisp-doc-wrap-page-body (concat "File: " name)
                                  (concat "File: <code>" (htmlize-protect-string name) "</code>")
                                  (format
                                   "Variable definitions, function definitions and face definitions in Emacs source file `%s'"
                                   (string-trim-left rel "/+")))
        (make-directory (file-name-directory path) t)
        (write-region (point-min) (point-max) path)))))

(provide 'elisp-doc-index)
