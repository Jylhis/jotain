;;; init.el --- Jotain Emacs Configuration --- Init -*- lexical-binding: t; -*-
;;
;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; URL: https://github.com/Jylhis/jotain
;; Package-Requires: ((emacs "30.2"))
;; Keywords: config
;; SPDX-License-Identifier: GPL-3.0-or-later
;;

;;; Commentary:
;; Main configuration entry point. Loads modules from lisp/ and modules/.
;;

;;; Code:


;;; Jotain Emacs custom options

;; TODO

;;; General Emacs configs (use-package ...

(use-package emacs
  :ensure nil
  :bind ; NOTE: M-x describe-personal-bindings (for all use-packge binds)
  (("M-o" . other-window)
   ("C-x ;" . comment-line)
   ("C-z" . nil)
   ("C-x C-z" . nil)
   ("C-x C-b" . ibuffer) ; Q: VS ([remap list-buffers] . ibuffer) ?
   )
  :custom
  (recentf-max-saved-items 300) ; default is 20
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
  (recentf-save-file (expand-file-name "cache/recentf" user-emacs-directory))
  (auto-save-default t)
  (enable-recursive-minibuffers t) ; Q: In emacs-solo, this is also defined in use-package minibuffers, why?
  (completion-ignore-case t) ; Q: In emacs-solo, this is also defined in use-package minibuffers, why?
  (delete-by-moving-to-trash t)
  (use-short-answers t)
  (visible-bell nil)
  (use-dialog-box nil)
  (create-lockfiles nil)
  (make-backup-files nil)
  (ring-bell-function 'ignore)
  (split-width-threshold 170)     ; So vertical splits are preferred
  (split-height-threshold nil)
  (line-number-mode 1)
  (column-number-mode t)
  (kill-do-not-save-duplicates t)
  (delete-selection-mode t)
  :init
  (context-menu-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (save-place-mode 1)
  (recentf-mode 1)
  (repeat-mode 1)
  (savehist-mode 1)
  (winner-mode)
  (xterm-mouse-mode 1) ;; REVIEW
  (file-name-shadow-mode 1) ;; REVIEW
  )
;;; ABBREV

;;; AUTH-SOURCE

;;; │ AUTO-REVERT
(use-package autorevert
  :ensure nil
  :hook (emacs-startup-hook . global-auto-revert-mode)
  :custom
  (auto-revert-remote-files nil)   ;; t makes tramp slow
  (auto-revert-verbose t)
  (auto-revert-avoid-polling t)
  (global-auto-revert-non-file-buffers t))

;;; CONF

;;; MINIBUFFER
(use-package minibuffer
  :ensure nil
  :custom
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  ;; This is defined in :config in emacs-solo, why?
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt) "Do not allow the cursor in the minibuffer prompt")
  )


;;; Jotain Emacs extra packages
;; Self-container modules that live under `lisp/' directory.
;; Each file is loaded here via `require'.
;; See `lisp/*.el' for per-module documentation.See 
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))



;;; ORGANIZE
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
