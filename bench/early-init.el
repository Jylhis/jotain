;;; early-init.el --- Startup benchmark wrapper -*- lexical-binding: t; -*-

;;; Commentary:

;; Wrapper early-init: installs timing advice around `require', `load',
;; and `package-refresh-contents', then loads the real early-init.el from
;; the parent directory.  Run as `emacs --init-directory=<repo>/bench'
;; (`just bench-built').

;;; Code:

(defvar jotain-bench--real-dir
  (file-name-directory
   (directory-file-name (expand-file-name user-emacs-directory)))
  "Real config directory, the parent of this bench directory.")

(defvar jotain-bench--results nil)

(defvar jotain-bench--loads nil
  "Alist of (FILE . SECONDS) for top-level `load' calls.
Loads not driven by a tracked `require': autoloads, `load-theme', the
package-quickstart file.  See Finding 52 in
docs/reviews/2026-07-emacs-nix-deep-review.md.")

(defvar jotain-bench--require-depth 0
  "Dynamic nesting depth of tracked `require'/refresh calls.
Lets `jotain-bench--load-advice' skip loads already attributed to an
enclosing `require'.")

(defun jotain-bench--require-advice (orig-fn feature &rest args)
  (let ((jotain-bench--require-depth (1+ jotain-bench--require-depth)))
    (if (memq feature features)
        (apply orig-fn feature args)
      (let ((start (current-time)))
        (prog1 (apply orig-fn feature args)
          (let ((elapsed (float-time (time-subtract (current-time) start))))
            (when (> elapsed 0.001)
              (push (cons (symbol-name feature) elapsed) jotain-bench--results))))))))

(advice-add 'require :around #'jotain-bench--require-advice)

(defvar jotain-bench--load-exclude '("early-init.el" "init.el")
  "Basenames the `load' advice ignores.
These are the wrapper's own loads of the real early-init.el and init.el,
which span nearly all of startup.")

(defun jotain-bench--load-advice (orig-fn file &rest args)
  "Time a top-level `load' of FILE not nested in a tracked `require'.
The depth guard avoids double-counting `require'd modules; the wrapper's
own delegation loads are excluded by name."
  (if (> jotain-bench--require-depth 0)
      (apply orig-fn file args)
    (let ((start (current-time)))
      (prog1 (apply orig-fn file args)
        (let ((elapsed (float-time (time-subtract (current-time) start)))
              (base (and (stringp file) (file-name-nondirectory file))))
          (when (and (> elapsed 0.001)
                     (not (member base jotain-bench--load-exclude)))
            (push (cons (or base (format "%s" file)) elapsed)
                  jotain-bench--loads)))))))

(advice-add 'load :around #'jotain-bench--load-advice)

(defun jotain-bench--pkg-refresh-advice (orig-fn &rest args)
  ;; Bump the depth so the refresh's internal `load's count toward this
  ;; NETWORK entry, not the autoload bucket.
  (let ((jotain-bench--require-depth (1+ jotain-bench--require-depth))
        (start (current-time)))
    (prog1 (apply orig-fn args)
      (push (cons "NETWORK:package-refresh-contents"
                  (float-time (time-subtract (current-time) start)))
            jotain-bench--results))))

(advice-add 'package-refresh-contents :around #'jotain-bench--pkg-refresh-advice)

(setq user-emacs-directory jotain-bench--real-dir)
(load (expand-file-name "early-init.el" jotain-bench--real-dir) nil t)

(provide 'early-init)
;;; early-init.el ends here
