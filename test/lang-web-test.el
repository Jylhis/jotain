;;; lang-web-test.el --- Tests for the CSS/SCSS mode routing -*- lexical-binding: t; -*-

;;; Commentary:

;; `init-lang-web.el' and `init-prog.el' are read as data, so no module is
;; loaded.  `.scss' must reach the classic `scss-mode': the CSS grammar does
;; not parse SCSS nesting, variables or mixins.

;;; Code:

(require 'ert)

(defconst lang-web-test--root
  (locate-dominating-file
   (or load-file-name buffer-file-name default-directory) "init.el")
  "Root of the Emacs configuration.")

(defun lang-web-test--forms (file)
  "Return the top-level forms of lisp/FILE."
  (with-temp-buffer
    (insert-file-contents (expand-file-name file (expand-file-name "lisp" lang-web-test--root)))
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (read (current-buffer)) forms))
        (end-of-file nil))
      (nreverse forms))))

(ert-deftest lang-web-test-scss-uses-scss-mode ()
  "The `css-mode' use-package maps `.css' to the ts-mode and `.scss' to `scss-mode'."
  (let* ((form (seq-find (lambda (f) (and (eq (car-safe f) 'use-package)
                                          (eq (nth 1 f) 'css-mode)))
                         (lang-web-test--forms "init-lang-web.el")))
         (modes (cadr (memq :mode form))))
    (should form)
    (should (eq (cdr (assoc "\\.css\\'" modes)) 'css-ts-mode))
    (should (eq (cdr (assoc "\\.scss\\'" modes)) 'scss-mode))))

(ert-deftest lang-web-test-ts-remap-leaves-scss-mode ()
  "The tree-sitter remap table does not remap `scss-mode' to the CSS ts-mode."
  (let* ((form (seq-find (lambda (f) (and (eq (car-safe f) 'defvar)
                                          (eq (nth 1 f) 'my-prog-ts-remaps)))
                         (lang-web-test--forms "init-prog.el")))
         (table (eval (nth 2 form) t))
         (css (assq 'css table)))
    (should css)
    (should (memq 'css-mode css))
    (should-not (memq 'scss-mode css))))

(provide 'lang-web-test)
;;; lang-web-test.el ends here
