;;; Package -- summary
;;; commentary:
;;; code:

(setq user-full-name "Markus Jylhänkangas")
(setq user-mail-address "markus@jylhis.com")

;; VIM cheat sheet: http://michael.peopleofhonoronly.com/vim/

;; http://aaronbedra.com/emacs.d/

;; NOTE: check if running in terminal
;;(if (display-graphic-p)
;;    (progn
;; if graphic
;;      (your)
;;      (code))
;; else (optional)
;;    (your)
;;    (code))

;; Melpa packages
(require 'package)
(add-to-list 'package-archives
             ;;'("melpa" . "https://mepla.org/packages/"))
             '("melpa" . "https://stable.melpa.org/packages/"))
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; FIXME: can't install packages without cl
(require 'cl)
(require 'cl-lib)

;; disable spalsh screen and empty scratch
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Disable bars
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;; highlight current line
(global-hl-line-mode 1)

;; thing?
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; Marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq select-enable-clipboard t)

;; Silence error https://github.com/syl20bnr/spacemacs/issues/192
(setq ad-redefinition-action 'accept)

;; TODO: Move installing of new packages to other file
;; TODO: Indent this stuff
;; In this file require packages exmp: (package(magit))
;; Packages to install
(defvar jylhis/packages
  '(
    ;; COMPANY
    company
    company-irony
    company-irony-c-headers

    ;; Irony
    irony

    ;; GIT
    magit
    gitignore-mode

    ;; FLYCHECK
    flycheck
    flycheck-color-mode-line
    flycheck-irony
    
    ;; HELM
    helm
    helm-ls-git
    helm-company
    helm-flycheck
    helm-gitignore
    helm-dash
    helm-package

    ;; EVIL
    evil
    evil-magit
    evil-org

    ;; Gnu Global
  ;;  ggtags
  ;;  helm-gtags

    ;; Cmake
    cmake-ide
    cpputils-cmake

    ;; Language modes
    dockerfile-mode
    haxe-mode
    json-mode
    markdown-mode
    rust-mode
    d-mode
    php-mode
    fic-mode
    toml-mode

    ;; Unorganized
    iedit
    autopair
    neotree
    rtags
    gruvbox-theme
    yasnippet
    latex-preview-pane
    dash
    async
    rainbow-mode
    ) "Default packages")

;; Line numbers
(if (require 'linum-relative nil 'noerror)
    (linum-relative-global-mode 1)
  (global-linum-mode 1))

;; Install packages
(defun jylhis/packages-installed-p ()
  (cl-loop for pkg in jylhis/packages
           when (not (package-installed-p pkg)) do (return nil)
           finally (return t)))

(unless (jylhis/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg jylhis/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; auto brackets
(autopair-global-mode)

;; Async
(async-bytecomp-package-mode 1)

;; Evil mode
(evil-mode 1)

;; Helm
(require 'helm-config)
(require 'helm-ls-git)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-x C-b") 'helm-buffer-list)
(helm-mode 1)
(helm-autoresize-mode 1)

;; Enable helm-gtags-mode
;;(add-hook 'c-mode-hook 'helm-gtags-mode)
;;(add-hook 'c++-mode-hook 'helm-gtags-mode)
;;(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; Set key bindings
;;(eval-after-load "helm-gtags"
 ;; '(progn
  ;;   (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
;;     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
;;     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
;;     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
;;     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;;     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;;     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))


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

;; rainbow
(add-hook 'css-mode '(lambda () (rainbow-mode 1)))

;; FIXME: fic-mode
(add-hook 'c++-mode-hook '(lambda () (fic-mode 1)))
(add-hook 'c-mode-hook '(lambda () (fic-mode 1)))

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
                                        ;(add-hook 'irony-mode-hook 'irony-eldoc)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony-c-headers))

;; CMake ide
(cmake-ide-setup)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Flycheck-iron
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; color line
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

;; rtags
(add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-common-hook 'rtags-start-process-unless-running)

;; Snippets
(require 'yasnippet)
(yas-global-mode 1)

;; turn on ede mode
(global-ede-mode t)

;; turn on Semantic
(semantic-mode t)
(global-semantic-idle-scheduler-mode t)
(global-semantic-decoration-mode t)
(global-semantic-highlight-func-mode t)

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

(provide 'init)
;;; init.el ends here
