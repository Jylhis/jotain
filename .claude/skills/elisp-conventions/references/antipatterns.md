# Elisp Antipatterns → Correct Patterns

## Keybindings

```elisp
;; ❌ define-key with old syntax
(define-key global-map "\C-ca" #'my-command)
;; ✅ keymap-set with key-valid-p syntax
(keymap-set global-map "C-c a" #'my-command)

;; ❌ global-set-key
(global-set-key (kbd "C-c a") #'my-command)
;; ✅ keymap-global-set
(keymap-global-set "C-c a" #'my-command)

;; ❌ define-key for mode map
(define-key emacs-lisp-mode-map (kbd "C-c e") #'eval-defun)
;; ✅ keymap-set
(keymap-set emacs-lisp-mode-map "C-c e" #'eval-defun)
```

## Variables

```elisp
;; ❌ setq on defcustom
(setq fill-column 100)
(setq use-short-answers t)
;; ✅ setopt
(setopt fill-column 100)
(setopt use-short-answers t)

;; ❌ defvar for user options
(defvar my-option 42 "User option.")
;; ✅ defcustom with :type
(defcustom my-option 42
  "User option."
  :type 'integer
  :group 'my-package)
```

## Hooks

```elisp
;; ❌ lambda in hook
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))
;; ✅ named function
(defun my-enable-line-numbers ()
  "Enable line numbers."
  (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook #'my-enable-line-numbers)

;; ❌ eval-after-load
(eval-after-load 'org '(progn (setq org-startup-indented t)))
;; ✅ with-eval-after-load
(with-eval-after-load 'org
  (setopt org-startup-indented t))
```

## Loading

```elisp
;; ❌ require 'cl (obsolete)
(require 'cl)
;; ✅ require 'cl-lib
(require 'cl-lib)

;; ❌ :ensure t in Nix config
(use-package magit :ensure t)
;; ✅ no :ensure (Nix manages packages)
(use-package magit)

;; ❌ if-let without star (obsoleted in Emacs 31)
(if-let ((x (get-value)))
    (use x))
;; ✅ if-let* (canonical)
(if-let* ((x (get-value)))
    (use x))
```

## Naming

```elisp
;; ❌ no prefix
(defun enable-feature () ...)
;; ✅ project prefix
(defun jotain-enable-feature () ...)

;; ❌ single hyphen for internal
(defun jotain-internal-helper () ...)
;; ✅ double hyphen for internal
(defun jotain--internal-helper () ...)
```

## Package Management

```elisp
;; ❌ package.el in Nix-managed config
(package-install 'magit)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; ✅ add to default.nix withPackages
;; (pkgs.emacsPackagesFor emacs).withPackages (epkgs: [ epkgs.magit ])

;; ❌ straight.el / elpaca in Nix config
(straight-use-package 'magit)
;; ✅ Nix handles all package management

;; ❌ treesit-install-language-grammar
(treesit-install-language-grammar 'python)
;; ✅ Nix provides grammars via treesit-grammars.with-grammars
```
