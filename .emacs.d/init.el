;;; Package -- summary
;;; commentary:
;;; code:

(setq user-full-name "Markus Jylhänkangas")
(setq user-mail-address "markus@jylhis.com")

;; VIM cheat sheet: http://michael.peopleofhonoronly.com/vim/

;; Melpa packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
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
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; Marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq select-enable-clipboard t)

;; gdb
(setq
 gdb-many-windows t
 gdb-show-main t
 )

;; Silence error https://github.com/syl20bnr/spacemacs/issues/192
(setq ad-redefinition-action 'accept)

;; Packages to install
(defvar jylhis/packages
  '(
    ;; COMPANY
    company
    company-irony
    company-irony-c-headers
    company-jedi

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
    helm-gtags

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
    fic-ext-mode
    toml-mode

    ;; Themes
    ;;gruvbox-theme
    atom-one-dark-theme
    rainbow-mode

    ;; Unorganized
    disaster
    linum-relative
    iedit
    autopair
    neotree
    function-args
    yasnippet
    latex-preview-pane
    dash
    async
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
(with-eval-after-load
    (unless (jylhis/packages-installed-p)
      (message "%s" "Refreshing package database...")
      (package-refresh-contents)
      (dolist (pkg jylhis/packages)
        (when (not (package-installed-p pkg))
          (package-install pkg)))))

;; auto brackets
(autopair-global-mode)

;; iedit
(require 'iedit)

;; Async
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)
(async-bytecomp-package-mode 1)

;; Evil mode
(evil-mode 1)

;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)
(global-set-key (kbd "C-x C-b") 'helm-buffer-list)
(helm-mode 1)
(helm-autoresize-mode 1)

;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; Function-args
(add-hook 'c-mode-hook 'fa-config-default)
(add-hook 'c++-mode-hook 'fa-config-default)

;; Set key bindings
(with-eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))


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
;;(global-set-key (kbd "<f5>") 'cmake-ide-compile)

;; rainbow
(add-hook 'css-mode '(lambda () (rainbow-mode 1)))

;; FIXME: fic-mode
(add-hook 'prog-mode-hook 'fic-ext-mode 1)
;;(setq-default fic-background-color "Black")

;; Irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony company-jedi)))

;; CMake ide
(cmake-ide-setup)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Flycheck-iron
(with-eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; color line
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

;; Snippets
(require 'yasnippet)
(yas-global-mode 1)

;; EDE
;;(global-ede-mode t)

;; Semantic
(require 'semantic)
(global-semanticdb-minor-mode)
(global-semantic-idle-scheduler-mode t)
(semantic-mode t)
(semantic-add-system-include "/usr/include/boost" 'c++-mode)

;; Disables backup & autosave
;; TODO: Backup to specific directory, dont disable it
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;; Yes no shortcut
(defalias 'yes-or-no-p 'y-or-n-p)

;; Org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (linum-mode 0)))

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode 1)))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

(provide 'init)
;;; init.el ends here
