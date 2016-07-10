;;; Package -- summary
;;; commentary:
;;; code:

;; http://aaronbedra.com/emacs.d/

;; Melpa packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(require 'cl)

;; thing?
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; Packages to install
(defvar jylhis/packages '(company
                          d-mode
                          iedit
                          ;auto-complete
                          ;auto-complete
                          autopair
                          cmake-ide
                          company-irony
                          company-irony-c-headers
                          cpputils-cmake
                          flycheck
                          flycheck-color-mode-line
                          flycheck-irony
                          haxe-mode
                          irony
                          json-mode
                          markdown-mode
                          neotree
                          rtags
                          rust-mode
                          gruvbox-theme
                          ;rainbow-mode
                          ;zenburn-theme
                          yasnippet
                          latex-preview-pane
                          dash
                          toml-mode
                          async
                          helm
                          helm-ls-git
                          helm-company
                          helm-flycheck)
  "Default packages")

;; Install default packages
(defun jylhis/packages-installed-p ()
  (loop for pkg in jylhis/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (jylhis/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg jylhis/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;Line numbers
(global-linum-mode t)

;; auto brackets
(autopair-global-mode)

;; disable spalsh screen and empty scratch
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Disable bars
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; Marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Async
(async-bytecomp-package-mode 1)

;; Rainbow mode
;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Helm
(require 'helm-config)
(require 'helm-ls-git)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-x C-b") 'helm-buffer-list)
(helm-mode 1)
(helm-autoresize-mode 1)

;; Use Shift+ArrowKeys to move in split panels
(windmove-default-keybindings)

;; Assign neotree to F8
(global-set-key [f8] 'neotree-toggle)

;; Indent settings
(setq-default c-basic-offset 4
                  tab-width 4
                  indent-tabs-mode nil
                  c-default-style "k&r")

;; Keybindings
(global-set-key (kbd "<f5>") 'cmake-ide-compile)

;; iedit
(require 'iedit)

;; Irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
;; replace the completion-at-point and complete-symbol bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

;; Hooks
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'irony-eldoc) ; Needed?

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;;FIXME: company-iron & irony-c-headers
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
  '(add-to-list 'company-backends '(company-irony-c-headers company-irony))

;; CMake ide
(cmake-ide-setup)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Flycheck-iron
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; color line
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

;; flycheck rust
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; rtags
(add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-common-hook 'rtags-start-process-unless-running)

;; turn on ede mode
(global-ede-mode t)

;; Snippets
(require 'yasnippet)
(yas-global-mode 1)

;; turn on Semantic
(semantic-mode t)
(global-semantic-idle-scheduler-mode t)
(global-semantic-decoration-mode t)
(global-semantic-highlight-func-mode t)
;(global-semantic-show-unmatched-syntax-mode t)

;; Disables backup & autosave
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;; Yes no shortcut
(defalias 'yes-or-no-p 'y-or-n-p)

;; Org-mode
(add-hook 'org-mode-hook   
  (lambda ()
    (linum-mode 0)))

(setq org-directory "/media/markus/Data/Dropbox/Documents/org/")
(setq org-agenda-files (list"/media/markus/Data/Dropbox/Documents/org"))
(setq org-default-notes-file (concat org-directory "/Notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;;(setq user-full-name "")
;;(setq user-mail-address "")

(provide 'init)
;;; init.el ends here
