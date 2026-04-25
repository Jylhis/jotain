;;; init-docs.el --- Make Jotain's Info manual discoverable -*- lexical-binding: t; -*-

;;; Commentary:

;; The Nix build (`jotainEmacsPackages') pre-wraps emacs with
;; INFOPATH pointing at the bundled jotain.info, so `C-h i d'
;; already lists Jotain under the "Emacs" category for NixOS /
;; nix-darwin / Home Manager users.  This module is the fallback for
;; source-checkout workflows (`just run' against a host emacs that
;; is not the Jotain wrapper): if `just info' has produced
;; `result-info/share/info/jotain.info' in the repo, register its
;; directory in `Info-directory-list' so the manual still opens.

;;; Code:

(require 'info)

(defvar jotain-info--candidate-paths
  (delq nil
        (list
         ;; Explicit override for unusual checkouts.
         (getenv "JOTAIN_INFO_DIR")
         ;; `just info' drops the result symlink here.
         (expand-file-name "result-info/share/info" user-emacs-directory)
         ;; Fallback: `nix build .#info -o result' without a custom name.
         (expand-file-name "result/share/info" user-emacs-directory)))
  "Directories to probe for a bundled `jotain.info' file.")

(let ((found
       (seq-find
        (lambda (dir)
          (and (stringp dir)
               (file-exists-p (expand-file-name "jotain.info" dir))))
        jotain-info--candidate-paths)))
  (when found
    (add-to-list 'Info-directory-list found))
  (when (and init-file-debug (not found))
    (message "init-docs: no jotain.info found in %S"
             jotain-info--candidate-paths)))

(provide 'init-docs)
;;; init-docs.el ends here
