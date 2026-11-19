;;; keys-test.el --- Tests for the window and quit commands in init-keys.el -*- lexical-binding: t; -*-

;;; Commentary:

;; `init-keys.el' is read as data; only the two commands are evaluated.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'test-helper)

(defun keys-test--load ()
  "Evaluate the commands out of the keys module."
  (test-helper-load-defuns "lisp/init-keys.el"
                           'jotain-toggle-window-split
                           'jotain-keyboard-quit-dwim))

(defmacro keys-test--quit-case (depth completions-window &rest body)
  "Run BODY with the minibuffer DEPTH and COMPLETIONS-WINDOW stubbed.
Binds `calls' to the list of quit commands that ran, newest first."
  (declare (indent 2))
  `(let (calls)
     (cl-letf (((symbol-function 'minibuffer-depth) (lambda () ,depth))
               ((symbol-function 'get-buffer-window)
                (lambda (&rest _) ,completions-window))
               ((symbol-function 'abort-recursive-edit)
                (lambda () (push 'abort calls)))
               ((symbol-function 'delete-completion-window)
                (lambda () (push 'close-completions calls)))
               ((symbol-function 'keyboard-quit)
                (lambda () (push 'quit calls))))
       (keys-test--load)
       ,@body)
     (nreverse calls)))

(ert-deftest keys-test-quit-dwim-aborts-open-minibuffer ()
  "An open minibuffer is aborted even with a Completions window visible."
  (should (equal (keys-test--quit-case 1 t (jotain-keyboard-quit-dwim))
                 '(abort))))

(ert-deftest keys-test-quit-dwim-closes-completions-window ()
  "With no minibuffer, a visible Completions window is closed."
  (should (equal (keys-test--quit-case 0 t (jotain-keyboard-quit-dwim))
                 '(close-completions))))

(ert-deftest keys-test-quit-dwim-falls-back-to-keyboard-quit ()
  "With neither, the plain `keyboard-quit' runs."
  (should (equal (keys-test--quit-case 0 nil (jotain-keyboard-quit-dwim))
                 '(quit))))

(defun keys-test--left-edge (window)
  "Return the left edge of WINDOW."
  (car (window-edges window)))

(ert-deftest keys-test-toggle-window-split-flips-layout ()
  "Two side-by-side windows become stacked and back, keeping buffers and focus."
  (keys-test--load)
  (let ((a (generate-new-buffer " *keys-a*"))
        (b (generate-new-buffer " *keys-b*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) a)
          (let ((first (selected-window))
                (second (split-window-right)))
            (set-window-buffer second b)
            (should-not (= (keys-test--left-edge first) (keys-test--left-edge second)))
            (jotain-toggle-window-split)
            (should (= (keys-test--left-edge (selected-window))
                       (keys-test--left-edge (next-window))))
            (should (eq (window-buffer (selected-window)) a))
            (should (eq (window-buffer (next-window)) b))
            (jotain-toggle-window-split)
            (should-not (= (keys-test--left-edge (selected-window))
                           (keys-test--left-edge (next-window))))
            (should (eq (window-buffer (selected-window)) a))
            (should (eq (window-buffer (next-window)) b))))
      (kill-buffer a)
      (kill-buffer b))))

(ert-deftest keys-test-toggle-window-split-needs-two-windows ()
  "Any other window count is a user error."
  (keys-test--load)
  (save-window-excursion
    (delete-other-windows)
    (should-error (jotain-toggle-window-split) :type 'user-error)
    (split-window-right)
    (split-window-below)
    (should-error (jotain-toggle-window-split) :type 'user-error)))

(provide 'keys-test)
;;; keys-test.el ends here
