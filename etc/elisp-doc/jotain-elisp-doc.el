;;; jotain-elisp-doc.el --- Batch API-reference generator for jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;;; Commentary:
;;
;; jotain's replacement for elisp-doc's `elisp-doc-main.el'.  Where the
;; upstream driver documents *all* of Emacs across versions (with tmux /
;; socket sharding), this one documents only the packages this config
;; bundles: it requires the config's package features, then exports only
;; the symbols those packages define, keyed by package.
;;
;; It reuses the vendored elisp-doc engine (`elisp-doc-extract',
;; `elisp-doc-index', `elisp-doc-shortdoc') without modifying it, and
;; drives it from `load-history' provenance so the corpus stays scoped.
;;
;; Invocation (see nix/emacs-api-doc.nix):
;;
;;   emacs --batch -L etc/elisp-doc -l jotain-elisp-doc
;;
;; Environment:
;;   ELISP_DOC_OUTPUT_DIR   destination directory (required)
;;   JOTAIN_PKG_FEATURES    newline-separated feature names to document
;;   JOTAIN_DOC_QUICK       if set, only export this many symbols per kind
;;                          (fast smoke test)

;;; Code:

(require 'cl-lib)
(require 'package)
(require 'subr-x)
;; `elisp-get-fnsym-args-string' / `elisp-get-var-docstring' (used for the
;; per-package markdown and the index summary column) live here.
(require 'elisp-mode)
(require 'find-func)

;; The vendored elisp-doc engine.  These `require's pull in helpful,
;; htmlize, elisp-demos, shortdoc and friends, which nix/emacs-api-doc.nix
;; adds to the doc Emacs' load-path.
(require 'elisp-doc-extract)
(require 'elisp-doc-index)
(require 'elisp-doc-shortdoc)

;; Optional highlighters, exactly as elisp-doc-main.el registers them, so
;; source snippets in docstrings pick up the same span classes.
(dolist (feature '(highlight-numbers highlight-quoted rainbow-delimiters))
  (ignore-errors (require feature nil t)))
(when (featurep 'highlight-numbers)
  (add-to-list 'helpful--highlighting-funcs #'highlight-numbers-mode))
(when (featurep 'highlight-quoted)
  (add-to-list 'helpful--highlighting-funcs #'highlight-quoted-mode))
(when (featurep 'rainbow-delimiters)
  (add-to-list 'helpful--highlighting-funcs #'rainbow-delimiters-mode))

(setq noninteractive t
      make-backup-files nil
      ;; Keep helpful cheap in batch: no cross-reference scans, no key
      ;; lookups.  elisp-doc-main.el binds these dynamically; we set them
      ;; globally since this process only ever generates docs.
      inhibit-message nil)

;;;; Configuration from the environment

(defun jotain-doc--env (name)
  "Return non-empty environment variable NAME, or nil."
  (let ((v (getenv name)))
    (and v (not (string-empty-p v)) v)))

(let ((out (jotain-doc--env "ELISP_DOC_OUTPUT_DIR")))
  (unless out
    (error "jotain-elisp-doc: ELISP_DOC_OUTPUT_DIR is required"))
  (setq elisp-doc-output-dir (expand-file-name out)
        elisp-doc-cache-dir (expand-file-name "cache" out)))
(make-directory elisp-doc-output-dir t)

(defvar jotain-doc--quick
  (when-let* ((q (jotain-doc--env "JOTAIN_DOC_QUICK")))
    (string-to-number q))
  "When set, limit each export pass to this many symbols (smoke test).")
(when jotain-doc--quick
  (setq elisp-doc--debug-quick jotain-doc--quick))

(defun jotain-doc--features ()
  "Return the list of feature symbols to document."
  (if-let* ((raw (jotain-doc--env "JOTAIN_PKG_FEATURES")))
      (mapcar #'intern
              (seq-remove #'string-empty-p
                          (split-string raw "[\n\r]+" t "[ \t]+")))
    (error "jotain-elisp-doc: JOTAIN_PKG_FEATURES is required")))

;;;; Load the packages and build package-provenance tables

(defmacro jotain-doc--ignoring (&rest body)
  "Run BODY, logging and swallowing any error."
  `(condition-case err (progn ,@body)
     (error (message "jotain-elisp-doc: %s" (error-message-string err)) nil)))

(defvar jotain-doc--pkg-dirs (make-hash-table :test 'equal)
  "Map of package directory (a store path) -> package name string.")
(defvar jotain-doc--skipped nil
  "List of (FEATURE . REASON) that failed to load.")

(defun jotain-doc--register-feature (feature)
  "`require' FEATURE and record its install directory for scoping."
  (let ((inhibit-message t))
    (if (jotain-doc--ignoring (require feature) t)
        (when-let* ((lib (locate-library (symbol-name feature)))
                    (dir (file-name-directory (file-truename lib))))
          (puthash (directory-file-name dir) (symbol-name feature)
                   jotain-doc--pkg-dirs))
      (push (cons feature "require failed") jotain-doc--skipped))))

(defun jotain-doc--file-pkg (file)
  "Return the package name owning FILE, or nil if out of scope."
  (when (stringp file)
    (let ((true (file-truename file)) found)
      (maphash (lambda (dir name)
                 (when (and (not found)
                            (string-prefix-p (file-name-as-directory dir) true))
                   (setq found name)))
               jotain-doc--pkg-dirs)
      found)))

;; Provenance tables built from `load-history': the exact symbols each
;; in-scope file defined.  This is more reliable than per-symbol
;; `symbol-file' and captures "everything the package defines".
(defvar jotain-doc--fun-syms (make-hash-table :test 'eq))
(defvar jotain-doc--var-syms (make-hash-table :test 'eq))
(defvar jotain-doc--face-syms (make-hash-table :test 'eq))
(defvar jotain-doc--sym-pkg (make-hash-table :test 'eq)
  "Map of in-scope symbol -> owning package name string.")

(defun jotain-doc--classify (sym pkg)
  "Record SYM under PKG in the appropriate provenance table."
  (when (symbolp sym)
    (puthash sym pkg jotain-doc--sym-pkg)
    (cond
     ((facep sym) (puthash sym t jotain-doc--face-syms))
     ((fboundp sym) (puthash sym t jotain-doc--fun-syms))
     ((boundp sym) (puthash sym t jotain-doc--var-syms)))
    ;; A symbol can be both a function and a variable.
    (when (and (fboundp sym) (boundp sym) (not (keywordp sym)))
      (puthash sym t jotain-doc--var-syms))))

(defun jotain-doc--scan-load-history ()
  "Populate the provenance tables from `load-history'."
  (dolist (entry load-history)
    (when-let* ((file (car-safe entry))
                (pkg (jotain-doc--file-pkg file)))
      (dolist (form (cdr entry))
        (cond
         ;; Bare symbol: a defvar/defconst.
         ((symbolp form) (jotain-doc--classify form pkg))
         ;; (TYPE . SYM): defun, macro, t (autoload), defface, define-type…
         ((and (consp form) (symbolp (cdr form)))
          (jotain-doc--classify (cdr form) pkg))
         ;; (cl-defmethod SYM …) and similar: SYM is the cadr.
         ((and (consp form) (consp (cdr form)) (symbolp (cadr form)))
          (jotain-doc--classify (cadr form) pkg)))))))

;;;; Scoped predicates

(defun jotain-doc--pred (base table)
  "Return a predicate: in TABLE and satisfying BASE."
  (lambda (sym)
    (and (gethash sym table)
         (funcall base sym))))

;;;; Manual (plaintext markdown) output, one file per package

(defun jotain-doc--symbol-md (sym kind)
  "Return a markdown block documenting SYM as KIND, or nil if undocumented."
  (let* ((doc (pcase kind
                ('fun (ignore-errors (documentation sym t)))
                ('var (ignore-errors
                        (documentation-property sym 'variable-documentation t)))
                ('face (ignore-errors (face-documentation sym)))))
         (sig (pcase kind
                ('fun (ignore-errors (elisp-get-fnsym-args-string sym)))
                (_ nil))))
    (when (and doc (not (string-empty-p (string-trim doc))))
      (concat "### `" (symbol-name sym) "`  _("
              (symbol-name kind) ")_\n\n"
              (when (and sig (not (string-empty-p sig)))
                (concat "`" (string-trim sig) "`\n\n"))
              (string-trim doc) "\n"))))

(defun jotain-doc--write-package-md (pkg syms)
  "Write md/PKG.md documenting SYMS (an alist of (SYM . KIND))."
  (let ((blocks (delq nil
                      (mapcar (lambda (cell)
                                (jotain-doc--symbol-md (car cell) (cdr cell)))
                              (sort (copy-sequence syms)
                                    (lambda (a b)
                                      (string-lessp (symbol-name (car a))
                                                    (symbol-name (car b)))))))))
    (when blocks
      (let ((dir (file-name-concat elisp-doc-output-dir "md")))
        (make-directory dir t)
        (with-temp-file (file-name-concat dir (concat pkg ".md"))
          (insert "## `" pkg "`\n\n")
          (insert (string-join blocks "\n"))
          (insert "\n"))))))

(defun jotain-doc--collect-package-symbols ()
  "Return a map of package name -> alist of (SYM . KIND)."
  (let ((by-pkg (make-hash-table :test 'equal)))
    (cl-flet ((add (sym kind)
                (when-let* ((pkg (gethash sym jotain-doc--sym-pkg)))
                  (push (cons sym kind) (gethash pkg by-pkg)))))
      (maphash (lambda (s _) (add s 'fun)) jotain-doc--fun-syms)
      (maphash (lambda (s _) (add s 'var)) jotain-doc--var-syms)
      (maphash (lambda (s _) (add s 'face)) jotain-doc--face-syms))
    by-pkg))

;;;; Per-package HTML index pages and the top index

(defun jotain-doc--pkg-items (pkg)
  "Return elisp-doc source-file items for PKG: list of (HELPFUL-FN . SYM)."
  (let (items)
    (maphash (lambda (file file-items)
               (when (equal pkg (jotain-doc--file-pkg file))
                 (setq items (nconc (copy-sequence file-items) items))))
             elisp-doc--source-files)
    items))

(defun jotain-doc--create-package-index (pkg)
  "Write pkg/PKG.html listing every documented symbol of PKG."
  (when-let* ((items (jotain-doc--pkg-items pkg)))
    (let ((dir (file-name-concat elisp-doc-output-dir "pkg")))
      (make-directory dir t)
      (with-temp-buffer
        ;; Links from pkg/PKG.html up to ../fun|var|face/<sym>.html.
        (elisp-doc--write-file-section "../" items #'helpful-variable)
        (elisp-doc--write-file-section "../" items #'helpful-function)
        (elisp-doc--write-file-section "../" items #'describe-face)
        (elisp-doc-wrap-page-body
         (concat "Package: " pkg)
         (concat "Package: <code>" (htmlize-protect-string pkg) "</code>")
         (format "Every documented symbol defined by the `%s' package." pkg))
        (write-region (point-min) (point-max)
                      (file-name-concat dir (concat pkg ".html")))))))

(defun jotain-doc--documented-packages ()
  "Return the sorted list of package names that produced any output."
  (let (names)
    (maphash (lambda (file _items)
               (when-let* ((pkg (jotain-doc--file-pkg file)))
                 (cl-pushnew pkg names :test #'equal)))
             elisp-doc--source-files)
    (sort names #'string-lessp)))

(defun jotain-doc--create-top-index (packages)
  "Write index.html: PACKAGES plus links to the global indexes."
  (with-temp-buffer
    (insert "<p>Generated API reference for the Emacs packages this "
            "configuration bundles — every function, command, variable, "
            "user option and face they define, rendered from their "
            "docstrings.</p>\n")
    (insert "<h2>Global indexes</h2>\n<ul>"
            "<li><a href=\"./fun.html\">All functions</a></li>"
            "<li><a href=\"./var.html\">All variables</a></li>"
            "<li><a href=\"./face.html\">All faces</a></li>"
            "<li><a href=\"./shortdoc.html\">Shortdoc cheatsheet</a></li>"
            "</ul>\n")
    (insert (format "<h2>Packages (%d)</h2>\n" (length packages)))
    (insert "<div class=\"table\"><table><tbody>")
    (dolist (pkg packages)
      (insert "<tr><td><a href=\"./pkg/"
              (url-hexify-string (elisp-doc--encode-url-part pkg t))
              ".html\"><code>" (htmlize-protect-string pkg)
              "</code></a></td></tr>"))
    (insert "</tbody></table></div>")
    (when jotain-doc--skipped
      (insert (format "<h2>Not documented (%d)</h2>\n<p>These packages "
                      "failed to load headless and were skipped:</p>\n<p>"
                      (length jotain-doc--skipped)))
      (insert (string-join
               (mapcar (lambda (c) (concat "<code>" (htmlize-protect-string
                                                     (symbol-name (car c)))
                                           "</code>"))
                       jotain-doc--skipped)
               ", "))
      (insert "</p>\n"))
    (elisp-doc-wrap-page-body
     "Jotain — Emacs package API reference"
     "Emacs package API reference"
     "Docstring-level reference for every package the Jotain Emacs configuration bundles.")
    (write-region (point-min) (point-max)
                  (file-name-concat elisp-doc-output-dir "index.html"))))

;;;; Drive everything

(defun jotain-doc--source-file-in-scope (orig path)
  "Advice for `elisp-doc--get-source-file': suppress links to unbuilt
source pages for our packages (we don't render `../src/'), while keeping
upstream behaviour for core Emacs paths."
  (if (jotain-doc--file-pkg path)
      nil
    (funcall orig path)))
(advice-add 'elisp-doc--get-source-file :around
            #'jotain-doc--source-file-in-scope)

;; Keep our manual (info) cross-links pointing at the jotain site layout,
;; which mounts the Emacs and Elisp manuals under /info/.
(defun jotain-doc--info-under-info (url)
  "Advice for `elisp-doc--info-url-for-node': prefix /info to URL."
  (if (string-prefix-p "/" url) (concat "/info" url) url))
(advice-add 'elisp-doc--info-url-for-node :filter-return
            #'jotain-doc--info-under-info)

(defun jotain-doc-generate ()
  "Top-level generation entry point."
  (let ((features (jotain-doc--features)))
    (message "jotain-elisp-doc: loading %d package features" (length features))
    (dolist (feature features)
      (jotain-doc--register-feature feature))
    (message "jotain-elisp-doc: %d package dirs, %d skipped"
             (hash-table-count jotain-doc--pkg-dirs)
             (length jotain-doc--skipped))
    (jotain-doc--scan-load-history)
    (message "jotain-elisp-doc: in scope — %d funcs, %d vars, %d faces"
             (hash-table-count jotain-doc--fun-syms)
             (hash-table-count jotain-doc--var-syms)
             (hash-table-count jotain-doc--face-syms))

    ;; HTML per-symbol export, reusing the vendored engine.
    (elisp-doc--export-reset)
    (elisp-doc--export-all
     (jotain-doc--pred #'elisp-doc--functionp jotain-doc--fun-syms)
     #'helpful-function 'defun)
    (message "jotain-elisp-doc: functions exported")

    (elisp-doc--export-reset)
    (elisp-doc--export-all
     (jotain-doc--pred #'elisp-doc--variablep jotain-doc--var-syms)
     #'helpful-variable 'defvar)
    (message "jotain-elisp-doc: variables exported")

    (elisp-doc--export-reset)
    (elisp-doc--export-all
     (jotain-doc--pred #'facep jotain-doc--face-syms)
     #'describe-face 'defface)
    (message "jotain-elisp-doc: faces exported")
    (elisp-doc--export-reset)

    ;; Global indexes (from the vendored engine).
    (jotain-doc--ignoring (elisp-doc--create-json-index))
    (jotain-doc--ignoring (elisp-doc--create-type-index #'helpful-function))
    (jotain-doc--ignoring (elisp-doc--create-type-index #'helpful-variable))
    (jotain-doc--ignoring (elisp-doc--create-type-index #'describe-face))
    (jotain-doc--ignoring (elisp-doc-export-shortdoc))

    ;; jotain per-package HTML + top index.
    (let ((packages (jotain-doc--documented-packages)))
      (dolist (pkg packages)
        (jotain-doc--ignoring (jotain-doc--create-package-index pkg)))
      (jotain-doc--create-top-index packages))

    ;; Plaintext markdown, per package, for the Info manual.
    (let ((by-pkg (jotain-doc--collect-package-symbols)))
      (maphash (lambda (pkg syms)
                 (jotain-doc--ignoring (jotain-doc--write-package-md pkg syms)))
               by-pkg))
    (message "jotain-elisp-doc: done")))

(jotain-doc-generate)
(kill-emacs 0)

(provide 'jotain-elisp-doc)

;;; jotain-elisp-doc.el ends here
