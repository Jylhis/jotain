;;; elisp-doc-shortdoc.el --- Export shortdoc documentation -*- lexical-binding: t -*-

;;; Commentary:
;;

(require 'shortdoc)
(require 'htmlize)
(require 'elisp-doc-extract)

;;; Code:

(defun elisp-doc--export-shortdoc-group (group data)
  "Insert GROUP of shortdoc DATA entries into the current buffer."
  (insert (string-trim (helpful--heading (propertize (concat "Group: " (symbol-name group))
                                                     'symbol group)))
          "\n\n")
  (dolist (entry data)
    (cond
     ((stringp entry)
      (insert (helpful--heading entry) "\n"))
     ((fboundp (car entry))
      (let* ((sym (pop entry))
             (sig (helpful--signature sym))
             (after (string-match-p "[ )]" sig 1))
             (doc (propertize
                   (concat ";; " (car (split-string (documentation sym) "\n")))
                   'face 'font-lock-comment-face))
             (doc (help-highlight-arguments sig doc)))
        (insert "("
                (helpful--button
                 (symbol-name sym) 'helpful-describe-button
                 'symbol sym)
                (substring (car doc) after) "\n")
        (insert  (cdr doc) "\n")
        (shortdoc--display-function (cons sym entry)))
      (insert "\n")))))

(defun elisp-doc--insert-toc ()
  (goto-char (point-min))
  (insert "<ul>")
  (dolist (group (sort (mapcar #'car shortdoc--groups) #'string-lessp))
    (insert "<li><a href=\"#" (htmlize-attr-escape (symbol-name group)) "\">"
            (htmlize-protect-string (symbol-name group)) "</a></li>"))
  (insert "</ul>"))

(defun elisp-doc-export-shortdoc ()
  "Exports shortdoc documents to a single HTML cheatsheet page."
  (when-let* ((buffer (get-buffer "*elisp-doc-shortdoc*")))
    (kill-buffer buffer))
  (with-current-buffer (get-buffer-create "*elisp-doc-shortdoc*")
    (cl-assert (eq (point-max) (point-min)))
    ;; export to emacs faces
    (insert "Emacs shortdoc cheatsheet.\n")
    (let ((groups (sort (copy-sequence shortdoc--groups)
                        (lambda (a b) (string-lessp (car a) (car b))))))
      (dolist (group groups)
        (elisp-doc--export-shortdoc-group (car group) (cdr group))))
    (let (prop)
      (goto-char (point-min))
      (while (setq prop (text-property-search-forward 'face 'shortdoc-section t))
        (delete-region (prop-match-beginning prop) (prop-match-end prop))
        (when (get-text-property (point) 'button)
          (delete-region
           (point)
           (prop-match-end
            (text-property-search-forward 'face 'shortdoc-section t)))))
      (goto-char (point-min))
      (while (setq prop (text-property-search-forward 'shortdoc-example))
        (let* ((start (prop-match-beginning prop))
               (end (prop-match-end prop))
               (code (buffer-substring-no-properties start end)))
          (delete-region start end)
          (insert (helpful--syntax-highlight code)))))

    ;; now, to html
    (let* ((entry (make-elisp-doc-entry
                   :symbol 'shortdoc
                   :type nil
                   :buffer (current-buffer)
                   :sections (cdr (elisp-doc--extract-helpful t))))
           (elisp-doc--convert-steps
            (remq 'elisp-doc--filter-sections elisp-doc--convert-steps))
           (elisp-doc--wrap-sections t))
      (elisp-doc--run-steps entry)

      ;; well, most of the headings should be h3
      (elisp-doc--replace-all "<h2>" "<h3>")
      (elisp-doc--replace-all "</h2>" "</h3>")
      ;; except group headings
      (goto-char (point-min))
      (while (re-search-forward "<h3>\\(Group: [^<]+\\)</h3>" nil t)
        (replace-match (concat "<h2>" (match-string 1) "</h2>") t t))

      ;; fix `elisp-doc--run-steps' relative links
      (goto-char (point-min))
      (while (search-forward " href=\"../" nil t)
        (replace-match " href=\"./" t t))

      ;; add anchors
      (goto-char (point-min))
      (let (prop)
        (while (setq prop (text-property-search-forward 'symbol))
          (goto-char (prop-match-beginning prop))
          (if (/= ?> (char-before))
              (goto-char (prop-match-end prop))
            (let ((sym (prop-match-value prop)))
              (backward-char)
              (insert " id=\"" (htmlize-attr-escape (symbol-name sym)) "\""))
            (forward-line))))

      (elisp-doc--insert-toc)

      (elisp-doc-wrap-page-body
       "Emacs Lisp Shortdoc Cheatsheet"
       "Emacs Lisp Shortdoc Cheatsheet"
       "Emacs Lisp Cheatsheet, expored directly from Emacs shortdoc package with various function usage examples."))

    (write-region (point-min) (point-max)
                  (file-name-concat elisp-doc-output-dir "shortdoc.html"))

    (current-buffer)))

(provide 'elisp-doc-shortdoc)

;;; elisp-doc-shortdoc.el ends here
