;;; core.el --- Core Emacs settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Core Emacs configuration including built-in packages and basic settings.

;;; Code:

(use-package emacs
  :init
  :custom
  (text-mode-ispell-word-completion nil "Emacs 30 and newer: Disable Ispell completion function.")
  (use-short-answers t "life is too short to type yes or no")
  (create-lockfiles nil)
  (delete-by-moving-to-trash t "Delete by moving to trash in interactive mode")
  (sentence-end-double-space nil "Disable the obsolete practice of end-of-line spacing from the typewriter era.")
  (word-wrap t "Continue wrapped lines at whitespace rather than breaking in the middle of a word.")
  (visible-bell nil "No blinking")
  (ring-bell-function #'ignore "No beeping")
  (scroll-preserve-screen-position 1 "keep the cursor in the same position while scrolling")
  (enable-recursive-minibuffers t "Support opening new minibuffers from inside existing minibuffers")
  (minibuffer-follows-selected-frame t "Minibuffer follows the selected frame")
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt) "Do not allow the cursor in the minibuffer prompt")
  (mouse-yank-at-point t "Make middle mouse paste at cursor position instead of click location")
  ;; Ignore case
  (completion-ignore-case t "Don't consider case significant in completion")
  (read-buffer-completion-ignore-case t "Ignore case when reading buffer name")
  (read-file-name-completion-ignore-case t "Ignore case for file completion")
  (load-prefer-newer t "Always load newer compiled files")
  :config
  ;; Enable modes
  (context-menu-mode 1)           ; Enable context menu for vertico
  (save-place-mode 1)             ; Automatically save your place in files
  (minibuffer-depth-indicate-mode 1) ; Show minibuffer depth to prevent confusion
  :bind
  (("C-z" . nil)
   ("C-x C-z" . nil)
   ("M-o" . other-window)
   ("M-p" . scroll-down-line)
   ("M-n" . scroll-up-line)))

(use-package window
  :ensure nil
  :bind ("C-x j" . my/toggle-window-split)
  :custom
  ;; Prefer side by side splitting
  (split-width-threshold 170)
  (split-height-threshold nil))

(use-package files
  :ensure nil
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p)
  :custom
  ;; Disable autosave and backups
  (auto-save-default nil "Disable separate autosave files")
  (make-backup-files nil "Disable auto backup files")
  ;; (find-file-visit-truename t "Resolve symlinks")
  (confirm-kill-processes nil "when quitting emacs, just kill processes")
  (enable-local-variables t "ask if local variables are safe once")
  (require-final-newline t "Ensure files end with newline"))

(use-package autorevert
  :ensure nil
  :custom
  (global-auto-revert-non-file-buffers t "Revert also non-file buffers")
  :init
  (global-auto-revert-mode 1)) ; Automatically refresh buffer if changed on disk

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 50 "Increase the default a bit")
  :init
  (recentf-mode 1)) ; Keep track of open files

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package simple
  :ensure nil
  :custom
  (read-extended-command-predicate
   #'command-completion-default-include-p "Hide commands in M-x which do not work in the current mode")
  (kill-do-not-save-duplicates t "Remove duplicates from the kill ring to reduce clutter")
  :config
  (line-number-mode 1)   ; Show line number in modeline
  (column-number-mode 1)) ; Show column number

(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

(use-package repeat
  :ensure nil
  :init
  (repeat-mode))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode))

(use-package dired
  :ensure nil
  :custom
  (dired-auto-revert-buffer #'dired-buffer-stale-p "Revert the Dired buffer without prompting.")
  (dired-clean-confirm-killing-deleted-buffers nil "Disable the prompt about killing the Dired buffer for a deleted directory.")
  (dired-create-destination-dirs 'ask)
  (dired-dwim-target t "Propose a target for intelligent moving or copying.")
  (dired-filter-verbose nil)
  (dired-free-space nil)
  (dired-listing-switches "-alh --group-directories-first" "In dired, show hidden files and human readable sizes")
  (dired-omit-verbose nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-vc-rename-file t)
  (dired-omit-files (concat
                     "\\`[.]?#\\|\\`[.][.]?\\'"
                     "\\|^[a-zA-Z0-9]\\.syncthing-enc\\'"
		     "\\|^\\.git\\'"
                     "\\|^\\.stfolder\\'"
                     "\\|^\\.stversions\\'"
                     "\\|^__pycache__\\'")))

(use-package dired-x
  :ensure nil
  :after dired
  :hook (dired-mode . dired-omit-mode))

(use-package ibuffer
  :ensure nil
  :bind
  (([remap list-buffers] . ibuffer)))

(use-package ffap
  :ensure nil
  :custom
  (ffap-machine-p-known 'reject)) ; Don't attempt to ping unknown hostnames

;;; Essential packages
(use-package super-save
  :ensure
  :diminish
  :custom
  (super-save-auto-save-when-idle t)
  (super-save-remote-files nil)
  (super-save-silent t)
  (super-save-delete-trailing-whitespace 'except-current-line)
  :config
  (super-save-mode 1))

(use-package vundo
  :ensure
  :bind ("C-x u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package expand-region
  :ensure
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)))

(use-package drag-stuff
  :ensure
  :diminish
  :autoload drag-stuff-define-keys
  :hook ((text-mode prog-mode) . drag-stuff-mode)
  :config
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  (drag-stuff-define-keys))

(use-package pretty-sha-path
  :ensure
  :hook ((shell-mode dired-mode) . pretty-sha-path-mode))

;; Startup performance monitoring
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %.2f seconds with %d garbage collections."
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))

(provide 'core)
;;; core.el ends here
