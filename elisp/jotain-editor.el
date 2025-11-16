;;; jotain-editor.el --- Editor UI and completion -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas
;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; Keywords: convenience
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;; Editor interface configuration including:
;; - Themes and visual appearance
;; - Modern completion framework (vertico, corfu, consult, embark)
;; - Font management
;; - Icons and visual enhancements

;;; Code:

;;; Theme and Appearance

(use-package emacs
  :custom
  (custom-safe-themes t "Trust all themes")
  :config
  (load-theme 'modus-vivendi-tinted t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-mixed-fonts t)
  :bind ("C-c t" . modus-themes-toggle)
  :config
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)))

(use-package auto-dark
  :ensure t
  :diminish
  :after modus-themes
  :custom
  (auto-dark-themes '((modus-operandi-tinted) (modus-vivendi-tinted)))
  :init
  (auto-dark-mode 1))

(use-package diminish :ensure t)

(use-package which-key
  :ensure t
  :diminish
  :hook (after-init . which-key-mode))

;;; Visual Enhancements

(use-package hl-line
  :custom
  (global-hl-line-sticky-flag t)
  :hook (after-init . global-hl-line-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package paren
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0.1)
  (show-paren-when-point-inside-paren t))

(use-package winner
  :bind (("C-c u" . winner-undo)
         ("C-c r" . winner-redo))
  :init
  (winner-mode 1))

(use-package pixel-scroll
  :when (display-graphic-p)
  :init
  (pixel-scroll-precision-mode 1))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        `(("TODO" warning bold)
          ("FIXME" error bold)
          ("NOTE" success bold))))

(use-package breadcrumb
  :ensure t
  :hook (prog-mode . breadcrumb-mode))

;;; Icons

(use-package nerd-icons :ensure t)

(use-package nerd-icons-corfu
  :ensure t
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :init
  (nerd-icons-completion-mode)
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;; Completion Framework

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :after vertico
  :custom
  (vertico-multiform-categories
   '((file buffer grid)
     (imenu (:not indexed mouse))))
  :init
  (vertico-multiform-mode 1))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-history-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :custom
  (cape-dabbrev-min-length 3))

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x r b" . consult-bookmark)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-s l" . consult-line)
         ("C-s" . consult-line)
         ("M-s r" . consult-ripgrep)
         :map isearch-mode-map
         ("M-s l" . consult-line))
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless partial-completion flex basic))
  (completion-category-overrides
   '((file (styles partial-completion orderless)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package avy
  :ensure t
  :bind (("M-g c" . avy-goto-char)
         ("M-g w" . avy-goto-word-1)))

;;; Font Configuration

;; Font fallback lists
(defvar jotain-font-mono
  '(("JetBrainsMono Nerd Font" . 110)
    ("FiraCode Nerd Font" . 110)
    ("Hack Nerd Font" . 110)
    ("DejaVu Sans Mono" . 100))
  "Monospace font preferences with fallbacks.")

(defvar jotain-font-variable
  '(("Inter" . 120)
    ("SF Pro Text" . 120)
    ("Ubuntu" . 120)
    ("DejaVu Sans" . 110))
  "Variable pitch font preferences.")

(defun jotain--find-first-available-font (font-list)
  "Find first available font from FONT-LIST."
  (let ((available (font-family-list)))
    (seq-find (lambda (spec) (member (car spec) available)) font-list)))

(defun jotain--set-face-font (face font-spec)
  "Set FACE to use FONT-SPEC (name . height)."
  (when font-spec
    (set-face-attribute face nil
                        :family (car font-spec)
                        :height (cdr font-spec))))

(defun jotain-setup-fonts ()
  "Setup fonts for current frame."
  (when (display-graphic-p)
    (let ((mono (jotain--find-first-available-font jotain-font-mono))
          (variable (jotain--find-first-available-font jotain-font-variable)))
      (when mono
        (jotain--set-face-font 'default mono)
        (jotain--set-face-font 'fixed-pitch mono))
      (when variable
        (jotain--set-face-font 'variable-pitch variable)))
    ;; Font performance
    (setq inhibit-compacting-font-caches t)))

;; Setup fonts for current and future frames
(jotain-setup-fonts)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (jotain-setup-fonts))))

;; Ligatures
(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures
   'prog-mode
   '("--" "---" "==" "===" "!=" "!==" "=>" "->" "->>" "<-"
     "<=" ">=" "&&" "||" "++" "::" "..." "]]" "[[" "]]"))
  (global-ligature-mode t))

(provide 'jotain-editor)
;;; jotain-editor.el ends here
