;;; init-core.el --- Sane defaults, GC, encoding, var/ paths -*- lexical-binding: t; -*-

;;; Commentary:

;; Settings that don't belong to any one feature: garbage collection,
;; encoding, file-handling defaults, custom-file location, and the
;; `var/' directory used to hold persistent state (recentf, savehist,
;; save-place, bookmarks, …). No third-party UI here, no completion,
;; no programming-mode tweaks — those have their own files.

;;; Code:

;;;; Persistent-state directory
;;
;; Jotain used to pull in `no-littering' to shepherd 200-odd variables
;; into `var/'. The actual set of paths *this* config writes is small
;; and stable, so we theme the relevant vars by hand from each module.
;; `jotain-var-file' is the single helper they share (used across
;; init-core, init-vc, init-project, init-systems, init-tracking).

(defconst jotain-var-dir
  (expand-file-name "var/" user-emacs-directory)
  "Directory for Jotain's persistent state files.")

(defun jotain-var-file (name)
  "Return NAME expanded under `jotain-var-dir'.
Ensures the directory exists so callers can use the path
immediately for writes."
  (make-directory jotain-var-dir t)
  (expand-file-name name jotain-var-dir))

(ignore-errors (make-directory jotain-var-dir t))

;; Restore a sane GC threshold after the early-init.el bump. 16 MiB is the
;; common compromise: high enough that typing/scrolling never trips a GC,
;; low enough that an idle GC actually completes quickly. Combined with
;; the idle-timer below, total pause time stays well under perceptible.
(setq gc-cons-threshold (* 16 1024 1024)
      gc-cons-percentage 0.1)
(run-with-idle-timer 5 t #'garbage-collect)

;; Pause GC entirely while the minibuffer is open. Completion frameworks
;; allocate aggressively and a GC mid-keystroke is the single biggest
;; source of perceptible input lag.
(add-hook 'minibuffer-setup-hook
          (lambda () (setq gc-cons-threshold most-positive-fixnum)))
(add-hook 'minibuffer-exit-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

;; UTF-8 everywhere. Modern systems are UTF-8; the locale dance only
;; matters if you ssh into something ancient.
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(use-package emacs
  :ensure nil
  :custom
  (fill-column 100)
  (use-short-answers t)
  (use-dialog-box nil)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (sentence-end-double-space nil)
  (require-final-newline t)
  (word-wrap t)
  (visible-bell nil)
  (ring-bell-function #'ignore)
  (scroll-preserve-screen-position 1)
  (mouse-yank-at-point t)
  (kill-do-not-save-duplicates t)
  (save-interprogram-paste-before-kill t)
  (set-mark-command-repeat-pop t)
  (redisplay-skip-fontification-on-input t)
  (enable-recursive-minibuffers t)
  (minibuffer-follows-selected-frame t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Case-insensitive completion everywhere — orderless still respects
  ;; case if you type uppercase, but case-blind by default.
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :config
  (context-menu-mode 1)
  (line-number-mode 1)
  (column-number-mode 1)
  (minibuffer-depth-indicate-mode 1))

(use-package saveplace
  :ensure nil
  :custom (save-place-file (jotain-var-file "save-place.el"))
  :config
  (save-place-mode 1)

  ;; Recenter the buffer after `save-place-mode' restores the cursor —
  ;; otherwise reopening a file can leave point on the bottom line.
  ;; Deferred via a zero-delay timer because the window doesn't exist
  ;; yet when `save-place-find-file-hook' fires.
  (defun jotain-core--recenter-buffer-window (buffer)
    "Recenter the window currently displaying BUFFER, if any."
    (when-let* ((win (get-buffer-window buffer)))
      (with-selected-window win
        (ignore-errors (recenter)))))

  (defun jotain-core--recenter-after-save-place (&rest _)
    "Schedule a recenter after `save-place-mode' restores point."
    (when buffer-file-name
      (run-with-timer 0 nil
                      #'jotain-core--recenter-buffer-window
                      (current-buffer))))

  (advice-add 'save-place-find-file-hook :after
              #'jotain-core--recenter-after-save-place))

;; Load diminish early so other use-package blocks can use `:diminish'
;; without producing macroexpansion errors.
(use-package diminish
  :demand t)

(declare-function jotain-core--auto-create-missing-dirs nil)
(use-package files
  :ensure nil
  :custom
  (auto-save-default nil)
  (auto-save-list-file-prefix (jotain-var-file "auto-save-list/saves-"))
  (make-backup-files nil)
  (confirm-kill-processes nil)
  :hook (after-save . executable-make-buffer-file-executable-if-script-p)
  :config
  ;; When opening foo/bar/new.txt and foo/bar/ doesn't exist, create it.
  (defun jotain-core--auto-create-missing-dirs ()
    "Create the parent directory of the visited file if it does not exist."
    (let ((target-dir (when buffer-file-name
                        (file-name-directory buffer-file-name))))
      (when (and target-dir (not (file-exists-p target-dir)))
        (make-directory target-dir t))))
  (add-to-list 'find-file-not-found-functions
               #'jotain-core--auto-create-missing-dirs))

(use-package repeat
  :ensure nil
  :config (repeat-mode 1))

(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

(use-package ibuffer
  :ensure nil
  :bind ([remap list-buffers] . ibuffer))

(use-package ffap
  :ensure nil
  :custom
  ;; Don't ping unknown hostnames in `find-file-at-point' — feels
  ;; instant instead of hanging on bad URLs.
  (ffap-machine-p-known 'reject))

(use-package time
  :ensure nil
  :commands world-clock
  :custom
  (world-clock-list
   '(("Europe/Zurich"   "Zurich")
     ("Europe/Helsinki" "Helsinki")
     ("Asia/Bangkok"    "Bangkok")
     ("Asia/Shanghai"   "Shanghai"))))

;; Standalone command for viewing logs that contain raw ANSI escapes.
;; M-x jotain-display-ansi-colors.
(defun jotain-display-ansi-colors ()
  "Render ANSI escape sequences in the current buffer."
  (interactive)
  (require 'ansi-color)
  (ansi-color-apply-on-region (point-min) (point-max)))

;;;; macOS — minimal modifier-key fix
;;
;; Reachable Meta is non-negotiable. The Cocoa default of Option-as-Meta
;; collides with typing curly braces and special characters on European
;; keyboard layouts, so we put Meta on Command and leave Right-Option
;; alone for special character entry.
(when (eq system-type 'darwin)
  (setopt mac-command-modifier      'meta
          mac-option-modifier       'super
          mac-right-option-modifier 'none)
  (setopt trash-directory "~/.Trash"))

(use-package autorevert
  :ensure nil
  :custom (global-auto-revert-non-file-buffers t)
  :config (global-auto-revert-mode 1))

(use-package recentf
  :ensure nil
  :custom
  (recentf-save-file (jotain-var-file "recentf-save.el"))
  (recentf-max-saved-items 200)
  :config
  (recentf-mode 1)
  (add-to-list 'recentf-exclude jotain-var-dir))

(use-package savehist
  :ensure nil
  :custom (savehist-file (jotain-var-file "savehist.el"))
  :config (savehist-mode 1))

(use-package bookmark
  :ensure nil
  :custom (bookmark-default-file (jotain-var-file "bookmarks.el")))

(provide 'init-core)
;;; init-core.el ends here
