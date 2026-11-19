;;; site-export.el --- Export the manual's chapters to HTML fragments  -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded after org-export.el by nix/site.nix, with docs/ as the current
;; directory.  Writes one body-only HTML fragment per page into OUT and a
;; manifest.tsv (slug, title, description) in the order of the #+INCLUDE
;; lines of jotain.org, which is also the order of the manual.

;;; Code:

(require 'subr-x)

(defun jotain-site--keyword (file key)
  "Return the value of #+KEY in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (if (re-search-forward (format "^#\\+%s:[ \t]*\\(.*\\)$" key) nil t)
        (match-string 1)
      (error "No #+%s in %s" key file))))

(defun jotain-site--chapters ()
  "Return the base names of the chapters included by jotain.org."
  (with-temp-buffer
    (insert-file-contents "jotain.org")
    (let (files)
      (while (re-search-forward "^#\\+INCLUDE: \"\\([^\".]+\\)\\.org::" nil t)
        (push (match-string 1) files))
      (nreverse files))))

(defun jotain-site--export (file out)
  "Export FILE as a body-only HTML fragment to OUT."
  (with-current-buffer (find-file-noselect file)
    (org-export-to-file 'html out nil nil nil t)))

(defun jotain-site-export (out)
  "Write the fragments and manifest.tsv to the directory OUT."
  (make-directory out t)
  (with-temp-file "module-options.org"
    (insert "#+TITLE: Module Options\n"
            "#+DESCRIPTION: Every option of the Home Manager and NixOS modules.\n\n"
            "* Module Options\n:PROPERTIES:\n:CUSTOM_ID: module-options\n:END:\n\n"
            "#+INCLUDE: \"generated/options.org\" :minlevel 2\n"))
  (let ((rows nil))
    (dolist (slug (append (jotain-site--chapters) '("module-options")))
      (let ((file (concat slug ".org")))
        (jotain-site--export file (expand-file-name (concat slug ".html") out))
        (push (format "%s\t%s\t%s" slug
                      (jotain-site--keyword file "TITLE")
                      (jotain-site--keyword file "DESCRIPTION"))
              rows)))
    (let ((org-html-toplevel-hlevel 2))
      (jotain-site--export "README.org" (expand-file-name "readme.html" out)))
    (with-temp-file (expand-file-name "manifest.tsv" out)
      (insert (string-join (nreverse rows) "\n") "\n"))))

(provide 'site-export)
;;; site-export.el ends here
