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

;;; @doc Sane defaults for the bare editor — fill column, dialog/box use,
;;; lockfiles, recursive minibuffers, case-insensitive completion.
;;; Settings here are the ones that don't deserve their own module.
(use-package emacs
  :ensure nil
  :custom
  (fill-column 100)
  (use-short-answers t)
  (read-answer-short t)
  (list-matching-lines-jump-to-current-line nil)
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

;;; @doc Persist point per file across sessions. Built-in. The advice
;;; below recenters the buffer after restore so you don't reopen on
;;; the bottom line.
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

;;; @doc Hides minor-mode lighters from the modeline. Loaded early and
;;; :demand t so that downstream use-package blocks can use the
;;; :diminish keyword without macro-expansion errors.
(use-package diminish
  :demand t)

(declare-function jotain-core--auto-create-missing-dirs nil)
;;; @doc Built-in file-handling tweaks: no auto-save side files, no
;;; backup `~` files, no kill-process confirmation, plus a hook that
;;; auto-creates missing parent directories on find-file.
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

;;; @doc Repeat-mode lets you press the trailing key alone after a prefix
;;; command (e.g. C-x o o o instead of C-x o C-x o). Built-in,
;;; enabled globally. `repeat-exit-timeout' clears the transient map
;;; after two idle seconds so the user doesn't have to think about
;;; exiting it — the ergonomic "one-shot modifier" pattern. A
;;; window-resize repeat-map filling the one gap in the built-in
;;; coverage lives in init-keys.el.
(use-package repeat
  :ensure nil
  :custom
  (repeat-exit-timeout 2)
  :config (repeat-mode 1))

;;; @doc Disambiguate same-name buffers by directory prefix instead of
;;; the default `<2>` suffix. `forward` style mirrors the path.
(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

;;; @doc Replace `list-buffers` (C-x C-b) with the more capable ibuffer:
;;; dired-style filter/mark/operate on buffers.
(use-package ibuffer
  :ensure nil
  :bind ([remap list-buffers] . ibuffer))

;;; @doc Tame `find-file-at-point` so an unknown hostname doesn't block
;;; the editor on a DNS lookup — reject means "treat as not a host".
(use-package ffap
  :ensure nil
  :custom
  (ffap-machine-p-known 'reject))

;;; @doc Built-in `world-clock` for cross-timezone scheduling. Loaded on
;;; demand only.
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

(declare-function profiler-start "profiler" (mode))
(declare-function profiler-stop "profiler")
(declare-function profiler-report "profiler")

(defvar jotain-profiler--running nil
  "Non-nil when `jotain-profile-toggle' is mid-recording.")

(defun jotain-profile-toggle ()
  "Toggle CPU+memory profiling; show the report on the second call.
First call starts the profiler; second call stops it and pops the
`*CPU/Memory Profiler Report*' buffer.  Useful for diagnosing
freezes — start, reproduce, stop."
  (interactive)
  (if jotain-profiler--running
      (progn (profiler-stop)
             (setq jotain-profiler--running nil)
             (profiler-report)
             (message "Profiler stopped — see *CPU/Memory Profiler Report*"))
    (profiler-start 'cpu+mem)
    (setq jotain-profiler--running t)
    (message "Profiler started — run `M-x jotain-profile-toggle' again to report")))

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

;;; @doc Inherits PATH, MANPATH, and other shell-managed vars from the
;;; user's login shell so GUI / launchd / systemd-spawned Emacs
;;; matches what the terminal sees. module.nix prepends Nix-store
;;; binaries (rg, fd, git, direnv, coreutils) to the wrapper's
;;; PATH — this picks up ~/.nix-profile and user toolchains.
(use-package exec-path-from-shell
  :if (or (daemonp)
          (memq window-system '(mac ns x)))
  :demand t
  :functions (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments nil) ; faster: skip -i
  :config
  (exec-path-from-shell-initialize))

;;; @doc Auto-revert buffers when the underlying file changes on disk —
;;; essential for branch switches and external edits. Also covers
;;; non-file buffers (dired, magit) so they refresh too.
(use-package autorevert
  :ensure nil
  :custom (global-auto-revert-non-file-buffers t)
  :config (global-auto-revert-mode 1))

;;; @doc Built-in recently-visited files list. Used by consult-recent-file
;;; and the bookmarks UI; state file lives under var/ to keep the
;;; repo root clean.
(use-package recentf
  :ensure nil
  :custom
  (recentf-save-file (jotain-var-file "recentf-save.el"))
  (recentf-max-saved-items 200)
  :config
  (recentf-mode 1)
  (add-to-list 'recentf-exclude jotain-var-dir))

;;; @doc Persist minibuffer history (M-p / M-n, vertico ordering, etc.)
;;; across sessions. Built-in, enabled globally.
(use-package savehist
  :ensure nil
  :custom
  (savehist-file (jotain-var-file "savehist.el"))
  :config
  (savehist-mode 1))

;;; @doc Built-in bookmark store. State file is themed under var/ so it
;;; joins the rest of Jotain's persistent state. `save-flag 1` writes
;;; on every change so an Emacs crash never loses bookmarks; the fringe
;;; glyph is suppressed because it adds visual noise without info.
(use-package bookmark
  :ensure nil
  :custom
  (bookmark-default-file (jotain-var-file "bookmarks.el"))
  (bookmark-fringe-mark nil)
  (bookmark-save-flag 1))

;;; @doc Lazy-count isearch matches in the prompt — "(3/12)" tells you
;;; where you are without leaving the search.
(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) ")
  (lazy-count-suffix-format nil))

;;; @doc Built-in HTML renderer used by eww, gnus, elfeed. Suppress page
;;; colours and proportional fonts so rendered HTML inherits the
;;; theme and the user's monospace face — better contrast,
;;; predictable layout.
(use-package shr
  :ensure nil
  :custom
  (shr-use-colors nil)
  (shr-use-fonts nil))

(provide 'init-core)
;;; init-core.el ends here
