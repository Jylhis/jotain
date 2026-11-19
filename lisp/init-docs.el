;;; init-docs.el --- Make the configuration's Info manual discoverable -*- lexical-binding: t; -*-

;;; Commentary:

;; The Nix distribution wraps emacs with INFOPATH pointing at
;; jotain.info. This module is the fallback for a host Emacs without that
;; wrapper: if a built manual is found (JOTAIN_INFO_DIR, result-info/ or
;; result/ in the checkout), add its directory to
;; `Info-additional-directory-list'.

;;; Code:

;; Not `Info-directory-list': `info-initialize' skips building it from
;; INFOPATH once it is non-nil, so seeding it would hide every other
;; manual.  Info appends `Info-additional-directory-list' afterwards.
(defvar Info-additional-directory-list)

(defvar jotain-info--candidate-paths
  (delq nil
        (list
         (getenv "JOTAIN_INFO_DIR")
         (expand-file-name "result-info/share/info" user-emacs-directory)
         (expand-file-name "result/share/info" user-emacs-directory)))
  "Directories to probe for a bundled `jotain.info' file.")

(let ((found
       (seq-find
        (lambda (dir)
          (and (stringp dir)
               (file-exists-p (expand-file-name "jotain.info" dir))))
        jotain-info--candidate-paths)))
  (when found
    (with-eval-after-load 'info
      (add-to-list 'Info-additional-directory-list found)))
  (when (and init-file-debug (not found))
    (message "init-docs: no jotain.info found in %S"
             jotain-info--candidate-paths)))

(provide 'init-docs)
;;; init-docs.el ends here
