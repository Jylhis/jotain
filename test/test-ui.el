;;; test-ui.el --- UI helper tests -*- lexical-binding: t; -*-

;;; Commentary:

;; `init-ui.el' is read as data instead of loaded, which would pull in
;; doom-modeline, auto-dark, and friends.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'test-helper)

(ert-deftest test-ui-modeline-icons-follow-the-frame ()
  "The icon switch follows the frame it is given and skips redundant sets."
  (test-helper-load-defuns "lisp/init-ui.el" 'jotain-ui--apply-modeline-icons)
  (defvar doom-modeline-icon)
  (let* ((sets 0)
         (watcher (lambda (&rest _) (setq sets (1+ sets))))
         (old (default-boundp 'doom-modeline-icon)))
    (set-default 'doom-modeline-icon t)
    (add-variable-watcher 'doom-modeline-icon watcher)
    (unwind-protect
        (cl-letf (((symbol-function 'force-mode-line-update) #'ignore)
                  ((symbol-function 'display-graphic-p) (lambda (&optional _f) nil)))
          (jotain-ui--apply-modeline-icons)
          (should-not doom-modeline-icon)
          (should (= sets 1))
          (jotain-ui--apply-modeline-icons)
          (should (= sets 1))
          (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _f) t)))
            (jotain-ui--apply-modeline-icons (selected-frame))
            (should (eq doom-modeline-icon t))
            (should (= sets 2))))
      (remove-variable-watcher 'doom-modeline-icon watcher)
      (unless old (makunbound 'doom-modeline-icon)))))

(provide 'test-ui)
;;; test-ui.el ends here
