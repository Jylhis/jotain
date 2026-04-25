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

(defun jotain-bench--require-advice (orig-fn feature &rest args)
  (if (memq feature features)
      (apply orig-fn feature args)
    (let ((start (current-time)))
      (prog1 (apply orig-fn feature args)
        (let ((elapsed (float-time (time-subtract (current-time) start))))
          (when (> elapsed 0.001)
            (push (cons (symbol-name feature) elapsed) jotain-bench--results)))))))

(advice-add 'require :around #'jotain-bench--require-advice)

(defun jotain-bench--pkg-refresh-advice (orig-fn &rest args)
  (let ((start (current-time)))
    (prog1 (apply orig-fn args)
      (push (cons "NETWORK:package-refresh-contents"
                  (float-time (time-subtract (current-time) start)))
            jotain-bench--results))))

(advice-add 'package-refresh-contents :around #'jotain-bench--pkg-refresh-advice)

(setq user-emacs-directory jotain-bench--real-dir)
(load (expand-file-name "early-init.el" jotain-bench--real-dir) nil t)

(provide 'early-init)
;;; early-init.el ends here
