;;; Init.el --- Jotain Emacs Config -*- lexical-binding:t; -*-


;;; Commentary:

;;; Code:

(message "Start of Jotain")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Store automatic customization options elsewhere
;; TODO: Where should this go?
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

;;;; Font configuration
(defvar jotain-font-preferences
  '(("JetBrainsMono Nerd Font" . 110)
    ("FiraCode Nerd Font" . 110)
    ("Iosevka Nerd Font" . 110)
    ("CaskaydiaCove Nerd Font" . 110)
    ("Hack Nerd Font" . 110)
    ("DejaVu Sans Mono" . 100))
  "Programming font preferences with fallbacks.")

(defun jotain-set-preferred-font ()
  "Set the first available font from `jotain-font-preferences'."
  (cl-loop for (font . size) in jotain-font-preferences
           when (find-font (font-spec :family font))
           return (set-face-attribute 'default nil :family font :height size)))

(jotain-set-preferred-font)
(add-hook 'server-after-make-frame-hook #'jotain-set-preferred-font)

(load-theme 'modus-operandi-tritanopia t)
;; (load-theme 'modus-vivendi-tritanopia t)

(use-package emacs
  :custom
  (fill-column 100)
  (use-short-answers t)
  (use-dialog-boxes nil)
  (delete-by-moving-to-trash t)
  (enable-recursive-minibuffers t)
  :bind
  (("C-z" . nil)
   ("C-x C-z" . nil)
   ("M-o" . other-window))
  )

;;;; Terminal compatibility
;;
;; Improvements for running `emacs -nw' inside modern terminals (Ghostty,
;; Kitty, WezTerm, Alacritty): mouse tracking, Kitty keyboard protocol so
;; Emacs can distinguish C-i/TAB, C-m/RET, C-[/ESC, and OSC 52 clipboard
;; sync that also works through SSH and tmux. See also `term-file-aliases'
;; in early-init.el for the xterm-ghostty alias.

(defun jotain-tty-setup ()
  "Configure behavior for each new terminal (tty) frame."
  (xterm-mouse-mode 1))

(add-hook 'tty-setup-hook #'jotain-tty-setup)

(use-package kkp
  :ensure t
  :config
  ;; global-kkp-mode is a no-op on GUI frames, so it's safe to enable
  ;; unconditionally and handles mixed GUI/tty daemon setups.
  (global-kkp-mode 1))

(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

(use-package nix-ts-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package markdown-mode
  :ensure t
    :mode ("README\\.md\\'" . gfm-mode)

  )

(use-package jotain-telemetry
  :custom
  (jotain-telemetry-enabled nil))

(use-package magit
  :ensure t
  :custom
    (magit-repository-directories '(("~/Developer" . 1)))
  )

;;;; Activity tracking
;;
;; Editor instrumentation for the local-first self-tracking stack:
;; - wakatime-mode      → heartbeats to a self-hosted Wakapi
;; - activity-watch-mode → heartbeats to ActivityWatch
;; - keyfreq            → command frequency stats
;; - org-clock          → task time tracking with persistence
;; - org-clock-csv      → export org-clock data for analysis

(use-package wakatime-mode
  :ensure t
  :if (executable-find "wakatime-cli")
  :custom
  (wakatime-cli-path (executable-find "wakatime-cli"))
  :config
  (global-wakatime-mode))

(use-package activity-watch-mode
  :ensure t
  :config
  (global-activity-watch-mode))

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package org-clock
  :custom
  (org-clock-persist t)
  (org-clock-idle-time 15)
  (org-clock-into-drawer t)
  :config
  (org-clock-persistence-insinuate))

(use-package org-clock-csv
  :ensure t
  :defer t)

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)
         ("M-s f" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :init
  ;; Tweak register preview for consult-register-load/store
  (advice-add #'register-preview :override #'consult-register-window)
  (setopt register-preview-delay 0.5)
  ;; Use Consult to select xref locations with preview
  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
  ;; Emacs 30: Sort completions by minibuffer history
  (setopt completions-sort 'historical)
  :config
  ;; Per-command preview-key debouncing
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setopt consult-narrow-key "<"))

(provide 'init)
;;; init.el ends here
