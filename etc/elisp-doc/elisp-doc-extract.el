;;; elisp-doc-extract.el --- Extract function documentation as text -*- lexical-binding: t -*-

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

;; Core
(require 'help)
(require 'help-fns)
(require 'helpful)
(require 'info)
(require 'info-look)

;; Plugin
(require 'elisp-demos)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
(require 'shortdoc)
(defun elisp-doc-shortdoc-advice-helpful-update ()
  (save-excursion
    (goto-char (point-min))
    (when-let* ((sym (and (symbolp helpful--sym) helpful--sym))
                (doc (shortdoc-function-examples sym))
                (_ (re-search-forward "^\\(References\\|Demos\\)$")))
      (forward-line 0)
      (let ((inhibit-read-only t))
        (insert (helpful--heading "Shortdoc"))
        (dolist (entry doc)
          (let ((kind (car entry))
                (example (cdr entry)))
            (insert (helpful--syntax-highlight ";; ")
                    (helpful--button (symbol-name kind) 'helpful-shortdoc-button
                                     'shortdoc-group kind)
                    "\n"
                    (helpful--syntax-highlight example)
                    "\n")))
        (insert "\n\n")))))
(advice-add 'helpful-update :after #'elisp-doc-shortdoc-advice-helpful-update)

;; Utils
(require 'cl-macs)
(require 'find-func)
(require 'htmlize)
(require 'seq)
(require 'text-property-search)
(require 'url-util)
(require 'whitespace)

;; Polyfill
(eval-and-compile
  (when (not (fboundp 'with-memoization))
    (defmacro with-memoization (place &rest code)
      "Return the value of CODE and stash it in PLACE.
If PLACE's value is non-nil, then don't bother evaluating CODE
and return the value found in PLACE instead."
      (declare (indent 1) (debug (gv-place body)))
      (gv-letplace (getter setter) place
        `(or ,getter
             ,(macroexp-let2 nil val (macroexp-progn code)
                `(progn
                   ,(funcall setter val)
                   ,val)))))))
(when (not (boundp 'lisp-directory))
  (let ((simple-file-name (locate-file "simple" load-path (get-load-suffixes))))
    (setq lisp-directory (file-truename (file-name-directory simple-file-name)))))
(when (not (fboundp 'shortdoc-function-examples))
  (defun shortdoc-function-examples (function)
    "Return all shortdoc examples for FUNCTION.
The result is an alist with items of the form (GROUP . EXAMPLES),
where GROUP is a shortdoc group where FUNCTION appears, and
EXAMPLES is a string with the usage examples of FUNCTION defined
in GROUP.  Return nil if FUNCTION is not a function or if it
doesn't has any shortdoc information."
    (let ((groups (and (symbolp function)
                       (shortdoc-function-groups function)))
          (examples nil))
      (mapc
       (lambda (group)
         (with-temp-buffer
           (shortdoc--display-function (assq function (assq group shortdoc--groups)))
           (goto-char (point-min))
           (forward-line 2)
           (push `(,group . ,(string-trim
                              (buffer-substring-no-properties
                               (point)
                               (point-max))))
                 examples)))
       groups)
      examples)))

(cl-defstruct elisp-doc-entry
  (symbol nil :type symbol)
  (type nil :type symbol)
  (buffer nil :type buffer)
  (sections nil :type list))

(defun elisp-doc-entry-type-desc (entry)
  "Return a human-readable English word for the type of the ENTRY."
  (let ((type (if (symbolp entry) entry (elisp-doc-entry-type entry))))
    (cond
     ((eq type 'describe-face) "face")
     ((eq type 'helpful-variable) "variable")
     ((eq type 'helpful-function) "function")
     (t (let ((type (symbol-name type)))
          (if (string-prefix-p "helpful-" type)
              (substring type (length "helpful-"))
            (substring type (length "describe-"))))))))

(defun elisp-doc-entry-type-short (entry)
  "Return a short word (or abbreviation) suitable for urls for ENTRY."
  (let ((type (elisp-doc-entry-type-desc entry)))
    (cond
     ((or (equal "face" type) (equal "macro" type)) type)
     (t (substring type 0 3)))))

(cl-defstruct elisp-doc-entry-section
  (heading nil :type (or nil string))
  (heading-marker nil :type (or nil marker))
  (start nil :type marker)
  (end nil :type marker))

(defsubst elisp-doc--find-section (entry heading)
  "Find the section with HEADING in ENTRY."
  (let (found)
    (dolist (section (elisp-doc-entry-sections entry) found)
      (if (equal heading (elisp-doc-entry-section-heading section))
          (setq found section)))))



(defvar elisp-doc--verbose t)
(defun elisp-doc--log (msg &rest args)
  "Logs by calling `message' with MSG and ARGS."
  (if elisp-doc--verbose
      (apply #'message msg args)))

(defun elisp-doc--file-parent (file)
  "That is `(file-name-parent-directory)', for Emacs 28."
  (file-name-directory (directory-file-name (expand-file-name file))))

(defvar elisp-doc-cache-dir
  (file-name-concat
   (if load-file-name
       (elisp-doc--file-parent load-file-name)
     default-directory)
   "cache"))

(defun elisp-doc-extract-helpful (helpful-func symbol)
  "Extract documentation sections for SYMBOL by calling HELPFUL-FUNC.

It calls the provided function on the symbol, renames the generated
buffer, make the buffer no longer read-only, and then returns a
`elisp-doc-entry' describing the sections."
  (let* (helpful-buffer
         (helpful-switch-buffer-function
          (lambda (b) (setq helpful-buffer b))))
    (let ((inhibit-message t))
      (funcall helpful-func symbol)
      (with-current-buffer (or helpful-buffer (help-buffer))
        (setq helpful-buffer (current-buffer))
        (set-buffer-file-coding-system 'utf-8-emacs)
        (read-only-mode -1)
        (rename-buffer (generate-new-buffer-name (format " *elisp-doc<%S>*" symbol)))
        (make-elisp-doc-entry
         :symbol symbol
         :type helpful-func
         :buffer (current-buffer)
         :sections (elisp-doc--extract-helpful))))))

(defsubst elisp-doc--make-marker (where)
  "Make a marker pointing to WHERE."
  (let ((m (make-marker))) (set-marker m where)))

(defun elisp-doc--extract-helpful (&optional force)
  "Extract basic sections for current helpful buffer."
  (cl-assert (or force (memq major-mode '(helpful-mode help-mode))))
  (let* (headings
         next
         heading
         heading-marker
         (start (point-min-marker))
         (commit (lambda (heading-marker end)
                   (push (make-elisp-doc-entry-section
                          :heading heading
                          :heading-marker heading-marker
                          :start start
                          :end end)
                         headings))))
    (goto-char (point-min))
    (while (setq next (text-property-search-forward 'face 'helpful-heading t))
      (funcall commit heading-marker
               (setq heading-marker
                     (elisp-doc--make-marker (prop-match-beginning next))))
      (setq start (elisp-doc--make-marker (prop-match-end next))
            heading (string-trim (buffer-substring-no-properties (prop-match-beginning next)
                                                                 (prop-match-end next)))))
    (funcall commit heading-marker (point-max-marker))
    (nreverse headings)))

(defun elisp-doc--clean-up ()
  "Delete all buffers."
  (dolist (buffer (buffer-list))
    (if (string-prefix-p " *elisp-doc" (buffer-name buffer))
        (kill-buffer buffer))))



(defvar elisp-doc-output-dir
  (file-name-concat
   (if load-file-name
       (elisp-doc--file-parent load-file-name)
     default-directory)
   "output"))

(defun elisp-doc--encode-url-part (s &optional keep-most)
  "Make S suitable for urls.

If KEEP-MOST, leave most characters as is. Otherwise, encode them as
what Info mode might do."
  (if keep-most
      (string-replace "/" "_002f" s)
    (mapconcat (lambda (ch)
                 ;; info.el
                 (if (or (< ch 32)      ; ^@^A-^Z^[^\^]^^^-
                         (<= 33 ch 47)  ; !"#$%&'()*+,-./
                         (<= 58 ch 64)  ; :;<=>?@
                         (<= 91 ch 96)  ; [\]_`
                         (<= 123 ch 127)) ; {|}~ DEL
                     (format "_00%x" ch)
                   (char-to-string ch)))
               s "")))

(defun elisp-doc-entry-output-path (entry)
  "Return output path for ENTRY, ensuring writable."
  (let ((type (symbol-name (elisp-doc-entry-type entry)))
        (name (symbol-name (elisp-doc-entry-symbol entry))))
    (cl-assert (or (string-prefix-p "helpful-" type)
                   (string-prefix-p "describe-" type)))
    (setq type (elisp-doc-entry-type-short entry))
    (let ((file (expand-file-name
                 (file-name-concat
                  elisp-doc-output-dir
                  type
                  (concat (elisp-doc--encode-url-part name t) ".html")))))
      (make-directory (elisp-doc--file-parent file) t)
      file)))

(defvar elisp-doc--convert-steps
  '(elisp-doc--filter-sections
    elisp-doc--prune-buttons
    elisp-doc--prune-headings
    elisp-doc--highlight-doc-lisp
    elisp-doc--highlight-args
    elisp-doc--insert-release
    elisp-doc--escape-html
    elisp-doc--prune-whitespace
    elisp-doc--wrap-code
    elisp-doc--face-special
    elisp-doc--link-arg-refs
    elisp-doc--htmlize
    elisp-doc--prune-html
    elisp-doc--collapse-value
    elisp-doc--wrap-inline-code
    ))
(defvar elisp-doc--entry)

(defun elisp-doc--gather-desc (entry)
  "Extract a short description for ENTRY."
  (let (spec desc)
    (goto-char (point-min))
    (forward-paragraph)
    (setq spec (buffer-substring-no-properties (point-min) (point)))
    (when-let* ((doc (elisp-doc--find-section entry "Documentation")))
      (goto-char (elisp-doc-entry-section-start doc))
      (forward-paragraph)
      (setq desc (buffer-substring-no-properties (elisp-doc-entry-section-start doc) (point))))
    (string-trim (concat spec (or desc "")))))

(defun elisp-doc-wrap-page-body (title title-html desc)
  "Wrap the current buffer with HTML heads.

TITLE is used in `title' tags, while TITLE-HTML is for `h1'. DESC is
filled into `meta' description."
  (goto-char (point-min))
  (insert-before-markers
   (format
    "<html lang=\"en-US\"><head><meta charset=\"UTF-8\"><meta content=\"width=device-width\" name=\"viewport\">
<title>%s – Emacs Documentation Online</title>
<meta name=\"description\" content=\"%s\">
<meta name=\"color-scheme\" content=\"light dark\">
<link href=\"/style.css\" rel=\"stylesheet\" type=\"text/css\">
<link href=\"/emacs.css\" rel=\"stylesheet\" type=\"text/css\">
</head><body><article><h1>%s</h1>"
    (htmlize-protect-string title)
    (htmlize-attr-escape desc)
    title-html))
  (goto-char (point-max))
  (insert-before-markers "</article><footer>
Content of the documentation pages is part of <a href=\"https://www.gnu.org/software/emacs/\">GNU Emacs</a> and available under <a href=\"https://www.gnu.org/licenses/gpl-3.0.html\">GPL</a>.
</footer></body></html>"))

(defun elisp-doc--run-steps (entry)
  (dolist (step elisp-doc--convert-steps)
    (funcall step entry)))

(defun elisp-doc-export (entry)
  "Convert the buffer of ENTRY into HTML, and then output."
  (with-current-buffer (elisp-doc-entry-buffer entry)
    (let ((elisp-doc--entry entry)
          (name (symbol-name (elisp-doc-entry-symbol entry)))
          (desc (elisp-doc--gather-desc entry)))
      (elisp-doc--run-steps entry)
      (let ((output (elisp-doc-entry-output-path entry))
            (type (capitalize (elisp-doc-entry-type-desc entry))))
        (elisp-doc-wrap-page-body (concat type ": " name)
                                  (concat type ": <code>" (htmlize-protect-string name) "</code>")
                                  desc)
        (elisp-doc--log "writing output: %s" output)
        (write-region (point-min) (point-max) output))))
  entry)



(defvar elisp-doc--output-sections
  '(nil
    ;; functions
    "Signature" "Documentation" "Key Bindings"
    "Aliases" "Implementations" "Source Code"
    "Demos" "Shortdoc"
    ;; variable
    "Value"))
(defun elisp-doc--filter-sections (entry)
  "Remove sections from ENTRY, keeping only `elisp-doc--output-sections'."
  (setf (elisp-doc-entry-sections entry)
        (seq-filter
         (lambda (section)
           (if (member (elisp-doc-entry-section-heading section)
                       elisp-doc--output-sections)
               t
             (delete-region
              (elisp-doc-entry-section-heading-marker section)
              (elisp-doc-entry-section-end section))
             nil))
         (elisp-doc-entry-sections entry))))



(defun elisp-doc--apply-highlights (section highlighted)
  "Copy argument highlight from HIGHLIGHTED to SECTION."
  (let ((start 0) (end (length highlighted)))
    (while (setq start (text-property-any start end 'face 'help-argument-name
                                          highlighted))
      (let ((span-end (or (text-property-any start end 'face nil
                                             highlighted)
                          end)))
        (put-text-property (+ (elisp-doc-entry-section-start section) start)
                           (+ (elisp-doc-entry-section-start section) span-end)
                           'face 'help-argument-name)
        (setq start span-end)))))
(defun elisp-doc--highlight-args (entry)
  "Highlight matching argument names in sections in ENTRY."
  (when-let* ((sig (elisp-doc--find-section entry "Signature")))
    (let* ((doc (or (elisp-doc--find-section entry "Documentation")))
           (highlights (help-highlight-arguments
                        (buffer-substring-no-properties
                         (elisp-doc-entry-section-start sig)
                         (elisp-doc-entry-section-end sig))
                        (when doc
                          (buffer-substring-no-properties
                           (elisp-doc-entry-section-start doc)
                           (elisp-doc-entry-section-end doc))))))
      (elisp-doc--apply-highlights sig (car highlights))
      (when doc (elisp-doc--apply-highlights doc (cdr highlights))))))

(defvar elisp-doc--code-wrap-regexp "\\(?:\n[ \t]+[^[:space:]].+$\\)+")
(defun elisp-doc--highlight-doc-lisp (entry)
  "Find indented blocks in ENTRY and highlight them as Lisp code."
  (when-let* ((doc (elisp-doc--find-section entry "Documentation")))
    (goto-char (elisp-doc-entry-section-start doc))
    (while (and (not (eobp))
                (re-search-forward elisp-doc--code-wrap-regexp
                                   (elisp-doc-entry-section-end doc) t))
      (when (progn (goto-char (match-beginning 0))
                   (skip-chars-forward "\n \t")
                   (eq ?\( (char-after)))
        (let* ((start (1+ (match-beginning 0)))
               (end (match-end 0))
               (text (helpful--syntax-highlight (buffer-substring-no-properties start end))))
          (goto-char start)
          (delete-region start end)
          (insert text))))))



(defun elisp-doc--insert-release (entry)
  "Insert \"Probaly introduced in ...\" string in ENTRY."
  (when-let* ((doc (elisp-doc--find-section entry "Documentation"))
              (sym (elisp-doc-entry-symbol entry))
              (release (help-fns--first-release sym)))
    (goto-char (1- (elisp-doc-entry-section-end doc)))
    (insert "\nProbably introduced at or before Emacs version " release ".")))



(defconst elisp-doc--html-entity-regexp
  (rx-to-string
   (let ((c 0) chars)
     (while (< c (length htmlize-basic-character-table))
       (when (not (equal (string c) (aref htmlize-basic-character-table c)))
         (push c chars))
       (setq c (1+ c)))
     (append `(or (any (,(1+ #x10FFFF) . ,(max-char)))) chars))))
(defun elisp-doc--escape-html (_entry)
  "Replace special characters with html entities."
  (goto-char (point-min))
  (while (re-search-forward elisp-doc--html-entity-regexp nil t)
    (let* ((start (match-beginning 0))
           (props (text-properties-at start))
           (c (string-to-char (match-string 0)))
           (subst (if (<= c #x10FFFF)
                      (aref htmlize-basic-character-table c)
                    (format "\\x%X" c))))
      (replace-match subst t t)
      (set-text-properties start (+ start (length subst)) props))))



(defvar elisp-doc--wrap-sections
  '("Source Code" "Signature" "Demos" "Shortdoc" "Value"))
(defun elisp-doc--can-safely-wrap-line ()
  ;; Assuming at line start
  (or
   ;; empty line -> safe
   (progn (skip-chars-forward "\0- ") (eolp))
   ;; start with words, and does not contain long spaces
   (and (progn (forward-line 0)
               (let ((c (char-after)))
                 (or (<= ?A c ?Z) (<= ?a c ?z))))
        (not (re-search-forward "[^.]\\(?:  \\|\t\\)" (line-end-position) t)))))
(defun elisp-doc--use-nbsp ()
  "Replace following spaces with \"&nbsp;\"."
  (let ((prev (point)))
    (let ((spaces (skip-chars-forward " ")))
      (delete-region prev (point))
      (dotimes (_ spaces)
        (insert "&nbsp;")))))
(defun elisp-doc--wrap-code (entry)
  "Wrap source sections in ENTRY with `pre' tags."
  ;; entire section as code
  (dolist (section (if (eq t elisp-doc--wrap-sections)
                       (elisp-doc-entry-sections entry)
                     (mapcar (lambda (s) (elisp-doc--find-section entry s))
                             elisp-doc--wrap-sections)))
    (when section
      (goto-char (1- (elisp-doc-entry-section-end section)))
      (insert "</code></pre>")
      (goto-char (elisp-doc-entry-section-start section))
      (insert "<pre><code>")))
  ;; indented block: ensure line breaks
  (when-let* ((doc (elisp-doc--find-section entry "Documentation")))
    (goto-char (elisp-doc-entry-section-start doc))
    (let (prev next (end (elisp-doc-entry-section-end doc)))
      (while (and (not (eobp)) (< (point) end))
        (when (or (setq next (not (elisp-doc--can-safely-wrap-line))) prev)
          (forward-line 0)
          (insert "<br>")
          (elisp-doc--use-nbsp))
        (setq prev next)
        (forward-line)))))

(defun elisp-doc--wrap-inline-code (entry)
  "Wrap with `code' for text with `font-lock-constant-face' in ENTRY."
  (dolist (section (elisp-doc-entry-sections entry))
    (unless (or (eq t elisp-doc--wrap-sections)
                (member (elisp-doc-entry-section-heading section) elisp-doc--wrap-sections))
      (let (prop)
        (goto-char (elisp-doc-entry-section-start section))
        (while (and (setq prop (text-property-search-forward 'face 'font-lock-constant-face t))
                    (<= (prop-match-end prop) (elisp-doc-entry-section-end section)))
          (let ((point (point)))
            (insert "</code>")
            (goto-char (prop-match-beginning prop))
            (insert "<code>")
            (goto-char (+ point (length "<code></code>")))))))))



(defvar elisp-doc--pure-ui-button-categories
  (mapcar (lambda (s) (concat (symbol-name s) "-button"))
          '(helpful-forget-button    ; unbind symbol
            helpful-c-source-directory  ; set source dir
            helpful-disassemble-button  ; ~
            helpful-run-test-button     ; ~
            helpful-edebug-button       ; ~
            helpful-trace-button        ; ~
            ;; helpful-navigate-button ; [[link]]: file
            helpful-buffer-button    ; switch to buffer (TODO: when?)
            helpful-customize-button ; ~
            helpful-associated-buffer-button ; change associated buffer
            helpful-toggle-button            ; ~
            helpful-set-button               ; ~
            helpful-view-literal-button      ; toggle view
            helpful-all-references-button    ; expand
            helpful-callees-button           ; search
            ;; helpful-manual-button ; [[link]]: manual
            ;; helpful-describe-button ; [[link]]: other symbol
            ;; helpful-describe-exactly-button ; [[link]]: other symbol or binding
            ;; helpful-info-button ; [[link]]: info node
            ;; helpful-shortdoc-button ; [[link]]: shortdoc
            ;; helpful-link-button ; [[link]]
            )))
(defsubst elisp-doc--get-prop (prop-match key)
  "Utility function to get KEY prop from first char in PROP-MATCH."
  (get-text-property (prop-match-beginning prop-match) key))
(defvar elisp-doc--C-source-dir nil)
(defun elisp-doc--prune-by-replace (section text replace)
  (goto-char (elisp-doc-entry-section-start section))
  (when (search-forward text (elisp-doc-entry-section-end section) t)
    (let ((from (match-beginning 0))
          (to (match-end 0)))
      (set-text-properties
       from (+ from (length replace))
       (prog1
           (text-properties-at from)
         (delete-region from to)
         (insert replace))))))
(defun elisp-doc--prune-buttons (entry)
  "Remove interative buttons in ENTRY."
  (goto-char (point-min))
  (let (prop)
    (while (setq prop (text-property-search-forward 'button))
      (when (or (member (symbol-name (get-text-property (prop-match-beginning prop) 'category))
                        elisp-doc--pure-ui-button-categories)
                (eq (elisp-doc--get-prop prop 'action) 'elisp-demos-add-demo))
        (delete-region (prop-match-beginning prop) (prop-match-end prop)))))
  ;; prune home dir path
  (when-let* ((dir find-function-C-source-directory)
              (src (elisp-doc--find-section entry "Source Code")))
    (elisp-doc--prune-by-replace
     src (with-memoization elisp-doc--C-source-dir
           (abbreviate-file-name find-function-C-source-directory))
     "/usr/src/emacs/src"))
  ;; prune nix's lengthy lisp load-path
  (when-let* ((src (elisp-doc--find-section entry "Source Code")))
    (elisp-doc--prune-by-replace src lisp-directory "/usr/src/emacs/lisp/")))

(defun elisp-doc--prune-headings (_entry)
  "Remove trailing newline from headings."
  (goto-char (point-min))
  (ignore-error end-of-file
    (read (current-buffer)) ; the symbol
    (put-text-property (point-min) (point) 'face 'font-lock-constant-face))
  (let (prop)
    (while (setq prop (text-property-search-forward 'face 'helpful-heading t))
      (let ((end (prop-match-end prop)))
        (when (= ?\n (char-after (1- end)))
          (set-text-properties (1- end) end nil))))))



(defvar elisp-doc--linkers
  '((helpful-navigate-button . elisp-doc--link-source-file)
    (helpful-manual-button . elisp-doc--link-manual)
    (helpful-describe-button . elisp-doc--link-symbol)
    (helpful-describe-exactly-button . elisp-doc--link-symbol)
    (helpful-info-button . elisp-doc--link-manual)
    (helpful-shortdoc-button . elisp-doc--shortdoc)
    (helpful-link-button . elisp-doc--link-url)
    (help-news . elisp-doc--link-source-file)
    (help-info . elisp-doc--link-manual)
    (help-face . elisp-doc--link-face-symbol)
    (help-symbol . elisp-doc--link-symbol)
    (help-variable . elisp-doc--link-symbol)
    (help-function . elisp-doc--link-symbol)
    (help-face-def . elisp-doc--link-source-file)
    (help-function-def . elisp-doc--link-source-file)
    ))
(defsubst elisp-doc--button-category (cat)
  "Get button names from CAT category name."
  (let ((name (symbol-name cat))) ; not interned
    (cl-assert (string-suffix-p "-button" name))
    (intern (substring name 0 (- (length name) (length "-button"))))))
(defsubst elisp-doc--link (link)
  "Generate an HTML link wrapper from LINK."
  (cons (concat "<a href=\"" (htmlize-attr-escape link) "\">") "</a>"))
(defun elisp-doc--get-source-file (path)
  "Convert PATH to source file links."
  (cond
   ((string-prefix-p lisp-directory path)
    (let ((path (let ((path (substring path (length lisp-directory))))
                  (dolist (suffix '(".gz") path)
                    (if (string-suffix-p suffix path)
                        (setq path (substring path 0 (- (length path) (length suffix)))))))))
      (concat "../src/" path ".html")))
   ((string-prefix-p doc-directory path)
    (let ((path (string-trim-left (substring path (length doc-directory)) "/")))
      (if find-function-C-source-directory
          (concat "../etc/" path ".html")
        (concat "https://github.com/emacs-mirror/emacs/blob/master/etc/" path))))
   ((and find-function-C-source-directory
         (string-prefix-p find-function-C-source-directory path))
    (concat "../src/"
            (string-trim-left (substring path (length find-function-C-source-directory)) "/")
            ".html"))
   ((and (string-prefix-p "src/" path) (string-suffix-p ".c" path)) ; `find-lisp-object-file-name'
    (concat "../" path ".html"))
   ((let (link)
      (dolist (pair package-alist link)
        (when-let* ((dir (package-desc-dir (cadr pair)))
                    (_ (string-prefix-p dir path))
                    (rel (string-trim-left (substring path (length dir)) "/")))
          (setq link (concat "../src/" (symbol-name (car pair)) "/" rel ".html"))))))
   (t nil)))
(defun elisp-doc--link-source-file (prop)
  "Return link wrappers if PROP is of file links."
  (when-let* ((path (or (elisp-doc--get-prop prop 'path)
                        (seq-find #'stringp (elisp-doc--get-prop prop 'help-args))))
              (link (elisp-doc--get-source-file path)))
    (elisp-doc--link link)))
(defvar elisp-doc--manual-cache (make-hash-table :test 'equal))
(defsubst elisp-doc--replace-all (from to)
  (goto-char (point-min))
  (while (search-forward from nil t)
    (replace-match to t t)))
(defsubst elisp-doc--re-replace-all (from to)
  (goto-char (point-min))
  (while (re-search-forward from nil t)
    (replace-match to t t)))
(defun elisp-doc--info-url-for-node (manual node)
  "`Info-url-for-node' but with our documentation instead."
  (unless node
    (string-match "\\`([^()]+)" manual)
    (setq node (substring manual (match-end 0))
          manual (substring manual 1 (1- (match-end 0)))))
  (when (equal node "")
    (setq node "index"))
  (setq node (string-trim node))
  (setq node
        (with-temp-buffer
          (insert (downcase node))
          (elisp-doc--re-replace-all "[^a-z0-9.#]+" "-")
          (elisp-doc--re-replace-all "-+" "-")
          (unless (equal node "Help-]")
            (elisp-doc--re-replace-all "-?\\.-?" "."))
          (buffer-string)))
  (concat "/" manual "/" node ".html"))
(defun elisp-doc--link-manual (prop)
  "Convert manual link PROP to online urls."
  (if-let* ((symbol (elisp-doc--get-prop prop 'symbol))
            (name (symbol-name symbol))
            (node (save-window-excursion
                    (ignore-error error
                      (info-lookup 'symbol name #'emacs-lisp-mode t)
                      (elisp-doc--info-url-for-node
                       (file-name-sans-extension
                        (file-name-nondirectory Info-current-file))
                       Info-current-node)))))
      (elisp-doc--link (concat node "#index-" (elisp-doc--encode-url-part name)))
    (save-window-excursion
      (if-let* ((info-node (or (elisp-doc--get-prop prop 'info-node)
                               (car-safe (elisp-doc--get-prop prop 'help-args)))))
          (with-memoization (gethash info-node elisp-doc--manual-cache)
            (ignore-error error
              (info info-node)
              (elisp-doc--link (elisp-doc--info-url-for-node info-node nil))))
        (let ((start (prop-match-beginning prop))
              (end (prop-match-end prop)))
          (delete-region start end)
          (setf (prop-match-end prop) start)
          '("" . ""))))))
(defun elisp-doc--shortdoc (prop)
  "Hint that PROP matches are `shortdoc' links."
  (when-let* ((shortdoc (elisp-doc--get-prop prop 'shortdoc-group)))
    (elisp-doc--link (concat "../shortdoc.html#" (symbol-name shortdoc)))))
(defun elisp-doc--link-url (prop)
  (when-let* ((url (elisp-doc--get-prop prop 'url)))
    (elisp-doc--link url)))
(defsubst elisp-doc--link-symbol-type (sym type &optional extra)
  "Return relative link to SYM of TYPE, with optional EXTRA text."
  (cl-assert (member type '("var" "fun" "face")))
  (cons (concat "<a " (or extra "")
                "href=\"../" type
                "/" (url-hexify-string (elisp-doc--encode-url-part (symbol-name sym) t)) ".html\">"
                (if extra "" "<code>"))
        (if extra "</a>" "</code></a>")))
(defun elisp-doc--link-face-multiple-inheritance (prop list)
  "Add links pointing to LIST of inherited faces for PROP text."
  (let* ((start (prop-match-beginning prop))
         (end (prop-match-end prop))
         (m (elisp-doc--make-marker end))
         (c1 (char-after start))
         (c2 (char-after (1- end)))
         ranges)
    (cl-assert (and (eq c1 ?\() (eq c2 ?\))))
    (goto-char (1+ start))
    (while list
      (let ((sym (pop list)) read sym-start)
        (skip-syntax-forward "-")
        (setq sym-start (point))
        (setq read (read (current-buffer)))
        (cl-assert (eq read sym))
        (push (cons sym (cons sym-start (point))) ranges)))
    (dolist (range ranges)
      (let* ((sym (car range))
             (left (cadr range))
             (right (cddr range))
             (link (elisp-doc--link-symbol-type sym "face")))
        (goto-char right)
        (insert (cdr link))
        (goto-char left)
        (insert (car link))))
    (goto-char m)
    (setf (prop-match-end prop) m)
    '("" . "")))
(defun elisp-doc--link-symbol (prop)
  "Auto-generate link wrapper for PROP text."
  (let* ((face (elisp-doc--get-prop prop 'face))
         (help-args (elisp-doc--get-prop prop 'help-args))
         (sym (or (elisp-doc--get-prop prop 'symbol) (car-safe help-args)))
         (sym (pcase sym (`',s s) (_ sym)))
         (fboundp (and (symbolp sym) (fboundp sym)))
         (boundp (and (symbolp sym) (boundp sym)))
         (facep (and (symbolp sym) (facep sym))))
    (cond
     ((eq face 'help-key-binding)
      (let ((fun (elisp-doc--link-symbol-type sym "fun")))
        (cons "<kbd>"
              (concat "</kbd> (" (car fun)
                      (htmlize-protect-string (symbol-name sym))
                      (cdr fun) ")"))))
     ((and boundp fboundp)
      (let ((text (buffer-substring (prop-match-beginning prop) (prop-match-end prop)))
            (var (elisp-doc--link-symbol-type sym "var" "class=\"v\" "))
            (fun (elisp-doc--link-symbol-type sym "fun" "class=\"f\" ")))
        (cons (concat (car var) "<code>")
              (concat "</code>(var)" (cdr var) "/"
                      (car fun) "<code>" text "</code>" "(fun)" (cdr fun)))))
     (boundp (elisp-doc--link-symbol-type sym "var"))
     (fboundp (elisp-doc--link-symbol-type sym "fun"))
     (facep (elisp-doc--link-symbol-type sym "face"))
     ((listp sym) (elisp-doc--link-face-multiple-inheritance prop sym)))))
(defun elisp-doc--link-face-symbol (prop)
  "Similar to `elisp-doc--link-symbol' but expecting face PROP only."
  (when-let* ((help-args (elisp-doc--get-prop prop 'help-args))
              (sym (car-safe help-args)))
    (elisp-doc--link-symbol-type sym "face")))
(defvar elisp-doc--face-classes
  '((helpful-heading . "h2")
    (help-argument-name . "arg")
    (help-key-binding . "key")
    (button . "bt")
    (font-lock-variable-name-face . "v")
    (font-lock-negation-char-face . "neg")
    (font-lock-regexp-grouping-construct . "rx-gr")
    (font-lock-regexp-grouping-backslash . "rx-gr-bs")
    (font-lock-warning-face . "w")
    (font-lock-builtin-face . "bi")
    (font-lock-comment-delimiter-face . "cd")
    (font-lock-constant-face . "m")
    (font-lock-string-face . "s")
    (font-lock-doc-face . "doc")
    (font-lock-function-name-face . "f")
    (font-lock-comment-face . "c")
    (font-lock-type-face . "ty")
    (font-lock-keyword-face . "k")
    ;; highlights
    (highlight-numbers-number . "m")
    (highlight-function-calls-face . "bi")
    (highlight-quoted-quote . "s")
    (highlight-quoted-symbol . "m")
    ;; elisp-demos
    (org-block-begin-line . "c")
    (org-block-end-line . "c")
    (org-special-keyword . "c")
    (org-meta-line . "c")
    (org-drawer . "c")
    (org-property-value . "s")
    (org-code . "")
    (org-block . "")))
(defun elisp-doc--general-highlight (prop)
  "Default link wrapper to wrap PROP links in styling spans."
  (let* ((faces (prop-match-value prop)))
    (cond
     ((eq faces 'helpful-heading) '("<h2>" . "</h2>"))
     (t
      (let ((face (mapconcat
                   (lambda (sym)
                     (or (cdr (assq sym elisp-doc--face-classes))
                         (let ((class (cond ((string-prefix-p "org-" (symbol-name sym)) "c")
                                            ((string-match "^rainbow-delimiters-depth-\\([0-9]+\\)-face$"
                                                           (symbol-name sym))
                                             (concat "rd" (match-string 1 (symbol-name sym))))
                                            (t (symbol-name sym)))))
                           (push (cons sym class) elisp-doc--face-classes)
                           class)))
                   (if (listp faces) faces (list faces))
                   " ")))
        (cons (format "<span class=\"%s\">" (htmlize-attr-escape face)) "</span>"))))))
(defun elisp-doc--htmlize (_entry)
  "Convert all styled (with `face') text spans into HTML spans."
  (goto-char (point-min))
  (let (prop)
    (while (setq prop (text-property-search-forward 'face))
      (let ((additions
             (if-let* ((category (elisp-doc--get-prop prop 'category))
                       (linker (assq (elisp-doc--button-category category) elisp-doc--linkers))
                       (link (funcall (cdr linker) prop)))
                 link
               (elisp-doc--general-highlight prop))))
        (goto-char (prop-match-end prop))
        (insert (cdr additions))
        (goto-char (prop-match-beginning prop))
        (insert (car additions))
        (goto-char (+ (prop-match-end prop) (length (car additions)) (length (cdr additions))))))))

(defun elisp-doc--find-text-fragment (text section pred)
  "Produce a tentative text fragment link fragment for TEXT in SECTION.

TODO: The caller may use PRED to check if the fragment if valid."
  (goto-char (elisp-doc-entry-section-start section))
  (catch 'break
    (while (search-forward text (elisp-doc-entry-section-end section) t)
      (when (funcall pred)
        (throw 'break
               (progn
                 (goto-char (match-beginning 0))
                 (backward-word)
                 (htmlize-attr-escape
                  (concat (string-clean-whitespace
                           (buffer-substring-no-properties
                            (point) (match-beginning 0)))
                          "-," text))))))))
(defconst elisp-doc--nth-word
  '("first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth"))
(defun elisp-doc--find-arg-ref (prop i section)
  "Produce a text fragment for I th arg wrapped by PROP in SECTION."
  (let ((case-fold-search nil)
        (arg (buffer-substring-no-properties (prop-match-beginning prop)
                                             (prop-match-end prop))))
    (or (elisp-doc--find-text-fragment
         arg section
         (lambda () (eq 'help-argument-name (get-text-property (1- (point)) 'face))))
        (progn (setq case-fold-search t) nil)
        (elisp-doc--find-text-fragment
         (or (nth (1- i) elisp-doc--nth-word) arg) section (lambda () t)))))
(defun elisp-doc--link-arg-refs (entry)
  "Link from signature args to documentation descriptions in ENTRY."
  (when-let* ((sig (elisp-doc--find-section entry "Signature"))
              (doc (elisp-doc--find-section entry "Documentation")))
    (goto-char (elisp-doc-entry-section-start sig))
    (let (arg (i 0))
      (while (and (setq arg (text-property-search-forward 'face 'help-argument-name t))
                  (<= (point) (elisp-doc-entry-section-end sig)))
        (let ((point (point))
              (head (concat "<a href=\"#:~:text="
                            (elisp-doc--find-arg-ref arg (setq i (1+ i)) doc)
                            "\">"))
              (tail "</a>"))
          (goto-char (prop-match-end arg))
          (insert tail)
          (goto-char (prop-match-beginning arg))
          (insert head)
          (goto-char (+ point (length head) (length tail))))))))

(defun elisp-doc--css (theme)
  "Switch to THEME and export CSS string."
  (load-theme theme t)
  (let* ((printed (make-hash-table :test 'equal))
         (mapping (htmlize-make-face-map
                   (mapcar #'car
                           (seq-filter (lambda (face)
                                         (prog1
                                             (not (gethash (cdr face) printed))
                                           (puthash (cdr face) t printed)))
                                       elisp-doc--face-classes)))))
    (clrhash printed)
    (with-temp-buffer
      (dolist (face elisp-doc--face-classes)
        (when-let* ((symbol (car face))
                    (name (cdr face))
                    (fstruct (gethash symbol mapping))
                    (_ (not (gethash name printed))))
          (puthash name t printed)
          (cond
           ((eq face 'help-key-binding) (insert "kbd, "))
           ((eq face 'helpful-heading) (insert "h2, ")))
          (insert "." name " { ")
          (dolist (spec (htmlize-css-specs fstruct))
            (insert spec " "))
          (insert "}\n")))
      (buffer-string))))

(defun elisp-doc-css (theme-light theme-dark)
  "Produce a CSS for used classes depending on current theme."
  (with-temp-buffer
    (insert (elisp-doc--css theme-light))
    (insert "\n@media (prefers-color-scheme: dark) {\n")
    (insert (elisp-doc--css theme-dark))
    (insert "}\n")
    (buffer-string)))



(defun elisp-doc--prune-whitespace (_entry)
  "Call `whitespace-cleanup'."
  (let ((whitespace-style '(indentation::space
                            space-before-tab::space
                            space-after-tab::space
                            empty trailing)))
    (deactivate-mark t)
    (whitespace-cleanup)))
(defvar elisp-doc--prune-html-pairs
  '(("<p><br>" . "<p>")
    ("<br><p>" . "<p>")
    ("<p><h2>" . "<h2>")
    ("<p><pre>" . "<pre>")
    ("\n\n</code></pre>" . "</code></pre>")
    ("<h2>Demos</h2>
<pre><code>
</code></pre>
" . "")))
(defun elisp-doc--prune-html (entry)
  "Add `p' tags in ENTRY."
  (dolist (section (elisp-doc-entry-sections entry))
    (unless (or (eq t elisp-doc--wrap-sections)
                (member (elisp-doc-entry-section-heading section) elisp-doc--wrap-sections))
      (goto-char (elisp-doc-entry-section-start section))
      (insert "<p>")
      (while (and (search-forward "\n\n" nil t)
                  (<= (point) (elisp-doc-entry-section-end section)))
        (replace-match "\n<p>" t t))))
  (dolist (pair elisp-doc--prune-html-pairs)
    (elisp-doc--replace-all (car pair) (cdr pair))))

(defun elisp-doc--collapse-value (entry)
  "Insert `details' if \"Value\" section gets too large in ENTRY."
  (when-let* ((section (elisp-doc--find-section entry "Value"))
              (start (elisp-doc-entry-section-start section))
              (end (elisp-doc-entry-section-end section)))
    (when (> (- (line-number-at-pos end) (line-number-at-pos start)) 30)
      (goto-char (1- end))
      (insert "\n</details>")
      (goto-char start)
      (insert "<details><summary>Large value</summary>"))))



(defvar elisp-doc-face-sample-text
  "The quick brown fox jumps over the lazy dog.")
(defun elisp-doc--face-special (_entry)
  "Special treatments for `describe-face' outputs.

Helpful does not provide `describe-face' equivalence."
  (goto-char (point-min))
  (when (re-search-forward "\\`Face: .+ (\\(sample\\)) .+$" 512 t)
    (let ((html (let ((style (text-properties-at 0 (match-string 1))))
                  (replace-match elisp-doc-face-sample-text t t)
                  (set-text-properties (point-min) (point) style)
                  (let ((inhibit-message t))
                    (condition-case err
                        (htmlize-region-for-paste (point-min) (point))
                      (error (message "htmlize error: %S" err) nil))))))
      (delete-region (point-min) (point))
      (insert "<h2>Face Sample:</h2><p>"
              (or html "Sample not available")))
    (when (search-forward "Documentation:" nil t)
      (replace-match "<h2>Documentation:</h2>" t t))
    (when (re-search-forward "\n+\\(?:\n.+:.+$\\)\\{10,\\}" nil t)
      (goto-char (match-beginning 0))
      (let ((space-start (point)))
        (skip-chars-forward "\n")
        (delete-region space-start (point)))
      (insert "\n<div class=\"table\"><table><tbody>\n")
      (while (re-search-forward "^.+?\\(:\\).+$" nil t)
        (goto-char (match-end 0))
        (insert "</td></tr>")
        (goto-char (match-beginning 1))
        (delete-char 1)
        (insert "</td><td>")
        (goto-char (match-beginning 0))
        (insert "<tr><td>")
        (goto-char (+ (match-end 0) (length "<tr><td></td><td></td></tr>"))))
      (insert "</tbody></table></div>"))))



;; (pop-to-buffer
;;  (elisp-doc-entry-buffer
;;   (elisp-doc-export (elisp-doc-extract-helpful #'helpful-function 'interactive))))

;; (pop-to-buffer
;;  (elisp-doc-entry-buffer
;;   (elisp-doc-export (elisp-doc-extract-helpful #'helpful-function 'defmacro))))

;; (pop-to-buffer
;;  (elisp-doc-entry-buffer
;;   (elisp-doc-export (elisp-doc-extract-helpful #'helpful-variable 'case-fold-search))))

;; (pop-to-buffer
;;  (elisp-doc-entry-buffer
;;   (elisp-doc-export (elisp-doc-extract-helpful #'describe-face 'woman-italic))))



(provide 'elisp-doc-extract)

;;; elisp-doc-extract.el ends here
