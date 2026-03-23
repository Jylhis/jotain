;;; init.el --- Jotain Emacs configuration -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>

;;; Commentary:
;; Main configuration entry point. Loads modules from lisp/ and modules/.
;;
;; Load order matters. Dependencies:
;;   jotain-defaults — no dependencies (foundational)
;;   jotain-platform — depends on jotain-defaults
;;   (future lisp/ modules may depend on any lisp/ module)
;;   (future modules/ files may depend on lisp/ only)

;;; Code:

;; Add module directories to load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Initialize package.el (archives configured by jotain-platform)
(require 'package)
(setq package-vc-heuristic-alist nil)
(package-initialize)

;;; Built-in modules (lisp/)
(require 'jotain-defaults)
(require 'jotain-platform)

;;; Third-party modules (modules/)
;; None yet — add (require 'module-name) here as modules/ files are created

;;; Private extensions
;; Load private.el after all standard modules, if it exists.
;; Errors are caught and reported without preventing startup.
(add-hook 'after-init-hook
          (lambda ()
            (let ((private-file (expand-file-name "private.el" user-emacs-directory)))
              (when (file-exists-p private-file)
                (condition-case err
                    (load private-file t)
                  (error
                   (display-warning 'jotain
                                    (format "Error loading private.el: %s" (error-message-string err))
                                    :error)))))))

(provide 'init)
;;; init.el ends here
