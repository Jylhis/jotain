;;; org-export.el --- Org export setup shared by the Info manual and the site  -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded by nix/info-manual.nix and nix/site.nix with `emacs -Q --batch'.
;; Cross-chapter links such as [[file:keybindings.org::#id][text]] are
;; rewritten before parsing: to internal links in the Info manual (where all
;; chapters are included into docs/jotain.org), to links between the site's
;; pages in HTML.  JOTAIN_BASE is the site's base path.

;;; Code:

(require 'org)
(require 'ox)
(require 'ox-texinfo)
(require 'ox-html)
(require 'ol-info)

(defconst jotain-export-docs-dir
  (file-name-as-directory (or (getenv "JOTAIN_DOCS") default-directory)))

(defconst jotain-export-base (or (getenv "JOTAIN_BASE") ""))

(setq org-export-use-babel nil
      org-export-with-toc nil
      org-export-with-section-numbers nil
      org-export-with-broken-links nil
      org-export-with-sub-superscripts nil
      org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-htmlize-output-type nil
      org-html-validation-link nil
      org-html-toplevel-hlevel 1
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-link-org-files-as-html nil
      org-html-prefer-user-labels t
      org-html-table-default-attributes nil
      org-info-other-documents
      (append '(("embark" . "https://github.com/oantolin/embark"))
              org-info-other-documents))

(defun jotain-export-page-slug (file)
  "Return the site page slug of docs FILE (a base name without extension)."
  (if (equal file "jotain") "module-options" file))

(defun jotain-export--top-id (file)
  "Return the CUSTOM_ID of the first heading of docs/FILE.org."
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat file ".org")
                                            jotain-export-docs-dir))
    (if (re-search-forward "^:CUSTOM_ID:[ \t]+\\(.+\\)$" nil t)
        (match-string 1)
      (error "No CUSTOM_ID in %s.org" file))))

(defconst jotain-export--file-link-re
  "\\[\\[file:\\([^]:]+\\)\\.org\\(?:::#\\([^]]+\\)\\)?\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]")

(defun jotain-export-rewrite-links (backend)
  "Rewrite links to other chapters for BACKEND in the current buffer."
  (let ((info (org-export-derived-backend-p backend 'texinfo)))
    (goto-char (point-min))
    (while (re-search-forward jotain-export--file-link-re nil t)
      (let* ((file (match-string 1))
             (desc (match-string 3))
             (id (or (match-string 2)
                     (save-match-data (jotain-export--top-id file)))))
        (replace-match
         (cond
          (info (format "[[#%s]%s]" id (if desc (format "[%s]" desc) "")))
          (t (format "@@html:<a href=\"%s/docs/%s/#%s\">@@%s@@html:</a>@@"
                     jotain-export-base (jotain-export-page-slug file) id
                     (or desc file))))
         t t)))))

(add-hook 'org-export-before-parsing-functions #'jotain-export-rewrite-links)

(provide 'org-export)
;;; org-export.el ends here
