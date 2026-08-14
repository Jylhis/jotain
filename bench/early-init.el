;;; early-init.el --- Startup benchmark wrapper -*- lexical-binding: t; -*-

;;; Commentary:

;; Wrapper early-init that installs timing advice around `require' and
;; `package-refresh-contents', then delegates to the real early-init.el
;; in the parent directory.  Invoked via:
;;
;;   emacs --init-directory=<repo>/bench

;;; Code:

(defvar jotain-bench--real-dir
  (file-name-directory
   (directory-file-name (expand-file-name user-emacs-directory)))
  "Real config directory — parent of this bench wrapper directory.")

(defvar jotain-bench--results nil)

(defvar jotain-bench--loads nil
  "Alist of (FILE . SECONDS) for top-level `load' calls.
These are loads NOT driven by a tracked `require' — i.e. the
autoload-triggered loads (a mode hook pulling in a file with no
`require', a `load-theme', the package-quickstart file) that a
require-only harness never saw.  Addresses review Finding 52.")

(defvar jotain-bench--require-depth 0
  "Dynamic nesting depth of tracked `require'/refresh calls.
Special (dynamic) so `jotain-bench--load-advice' can tell a `load'
fired from inside a `require' (already attributed to that feature)
from a genuine autoload trampoline load fired at top level.")

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
  "Basenames the `load' advice ignores: this wrapper's own delegation
loads of the real early-init.el / init.el, which span nearly the whole
startup and are not autoloads.")

(defun jotain-bench--load-advice (orig-fn file &rest args)
  "Time top-level `load' of FILE not nested inside a tracked `require'.
Everything reaches Emacs through `load', so the depth guard keeps this
from double-counting the module loads `require' already measures; what
remains are the autoload-driven loads Finding 52 flagged as invisible.
The two wrapper delegation loads are excluded by name."
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
  ;; Bump the depth counter so the refresh's own internal `load' calls
  ;; stay attributed to this NETWORK op instead of leaking into the
  ;; autoload bucket above.
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
