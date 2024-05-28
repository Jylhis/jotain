;;; package -- Init  -*- lexical-binding: t; -*-
;;; Commentary:

;; Bootstrap config

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "personal" user-emacs-directory))


(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning

(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
 (when (file-exists-p custom-file)
   (load custom-file 'noerror))

(load "setup-straight")
(load "setup-exec-path")

;; General performance tuning
(when (require 'gcmh)
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (add-hook 'after-init-hook (lambda ()
                               (gcmh-mode)
                               (diminish 'gcmh-mode))))

(setq jit-lock-defer-time 0)


;; TODO: diminish here

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(load "setup-defaults")
;;==================
;; Sane defaults
;;==================

;; Delete trailing spaces and add new line in the end of a file on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; use human-readable sizes in dired


(load "setup-keybindings")


;;(setq max-lisp-eval-depth 10000)

;; Minibuffer completion is essential to your Emacs workflow and
;; Vertico is currently one of the best out there. There's a lot to
;; dive in here so I recommend checking out the documentation for more
;; details: https://elpa.gnu.org/packages/vertico.html. The short and
;; sweet of it is that you search for commands with "M-x do-thing" and
;; the minibuffer will show you a filterable list of matches.
(use-package vertico
  :straight t
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(basic substring partial-completion flex))
  :init
  (vertico-mode))

;; Improve the accessibility of Emacs documentation by placing
;; descriptions directly in your minibuffer. Give it a try:
;; "M-x find-file".
(use-package marginalia
  :after vertico
  :straight t
  :init
  (marginalia-mode))

;; Adds intellisense-style code completion at point that works great
;; Add extra context to Emacs documentation to help make it easier to
;; search and understand. This configuration uses the keybindings
;; recommended by the package author.
(use-package helpful
  :straight t
  :bind (("C-h f" . #'helpful-callable)
         ("C-h v" . #'helpful-variable)
         ("C-h k" . #'helpful-key)
         ("C-c C-d" . #'helpful-at-point)
         ("C-h F" . #'helpful-function)
         ("C-h C" . #'helpful-command)))

(load "setup-projectile")

(load "setup-dashboard")

;; Extended completion utilities
(use-package consult
  :straight t
  :config
  (global-set-key [rebind switch-to-buffer] #'consult-buffer)
  (global-set-key (kbd "C-c j") #'consult-line)
  (global-set-key (kbd "C-c i") #'consult-imenu)
  (setq read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        completion-ignore-case t)

  )




;; Miscellaneous options
(setq-default major-mode8
              (lambda ()             ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

;;(setq window-resize-pixelwise t)
;;(setq frame-resize-pixelwise t)



;;===============
;; KeyBindings
;;===============

;; TODO: Map lsp-find-* to something useful

;; Move-text lines around with meta-up/down.
(use-package move-text
  :straight t
  :config
  (move-text-default-bindings))

;;===============
;; UI
;;===============

(load "extern-theme")
(load "setup-treemacs")

;;================
;; Programming
;;================


;; Automatically guess indent offsets, tab, spaces settings, etc.
(use-package dtrt-indent
  :straight t)

;; ;; Enable autocompletion by default in programming buffers
;;(add-hook 'prog-mode-hook #'corfu-mode)

(use-package jsonrpc
  :straight t)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  )

;;; Indication of local VCS changes
(use-package diff-hl
  :straight t
  :config
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  )

;; Remove trailing whitespace in files
(autoload 'nuke-trailing-whitespace "whitespace" nil t)

(use-package diff-mode
  :straight t
  :mode
  "\\.patch[0-9]*\\'"
  )

()


(load "setup-c")

(load "setup-git")


(use-package dash
  :straight t)

(load "setup-markdown")

(use-package yaml-mode
  :straight t)


(use-package highlight-indentation
  :straight t)

(use-package sphinx-mode
  :straight t
 )
(use-package sphinx-doc
  :straight t)

(load "setup-lsp-dap")
(load "setup-company")
(load "setup-flycheck")

(load "setup-docker")
(load "setup-editorconfig")
(load "setup-gitlab-ci")
(load "setup-go")
(load "setup-json")
(load "setup-jupyter")
(load "setup-nix")
(load "setup-powershell")
(load "setup-python")
(load "setup-rust")
(load "setup-yasnippet")
(load "setup-tree-sitter")


(provide 'init)
;;; init.el ends here
