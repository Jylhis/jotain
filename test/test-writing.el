;;; test-writing.el --- Prose list-shaking regression tests -*- lexical-binding: t; -*-

;;; Commentary:

;; `init-writing.el' turns on `visual-wrap-prefix-mode' for every
;; `text-mode' buffer.  In Markdown, with a proportional font, that mode
;; re-measures the list marker prefix while font lock reruns and the list
;; shakes.  Markdown must opt out, as Org does.
;;
;; The tests read `init-writing.el' as data instead of loading it, which
;; would pull in jinx, denote, and friends.

;;; Code:

(require 'ert)
(require 'seq)
(require 'visual-wrap)
(require 'test-helper)

(defun test-writing--use-package (name)
  "Return the `use-package' form for NAME in the writing module."
  (test-helper-form "lisp/init-writing.el" 'use-package name))

(defun test-writing--keyword (form keyword)
  "Return the forms that follow KEYWORD in the `use-package' FORM."
  (let ((tail (memq keyword form)))
    (seq-take-while (lambda (x) (not (keywordp x))) (cdr tail))))

(ert-deftest test-writing-markdown-buffer-has-no-visual-wrap-prefix ()
  "Running the markdown opt-out leaves `visual-wrap-prefix-mode' off.
The `text-mode' hook enables it first, so this reproduces the order of
the real mode hooks."
  (let* ((form (test-writing--use-package 'markdown-mode))
         (defun-form (seq-find (lambda (x) (eq (car-safe x) 'defun))
                               (test-writing--keyword form :preface))))
    (should defun-form)
    (eval defun-form t)
    (with-temp-buffer
      (text-mode)
      (visual-wrap-prefix-mode 1)
      (should visual-wrap-prefix-mode)
      (jotain-writing--disable-visual-wrap-prefix)
      (should-not visual-wrap-prefix-mode))))

(ert-deftest test-writing-mixed-pitch-faces-extended-after-load ()
  "The mixed-pitch form adds the Markdown faces in `:config', not `:custom'.
A `:custom' value that reads `mixed-pitch-fixed-pitch-faces' runs before
the deferred package loads and signals `void-variable' in every
`text-mode' hook, which leaves Markdown buffers without font lock."
  (let ((form (test-writing--use-package 'mixed-pitch)))
    (should form)
    (should-not (memq :custom form))
    (should (string-match-p
             "markdown-code-face"
             (prin1-to-string (test-writing--keyword form :config))))))

(provide 'test-writing)
;;; test-writing.el ends here
