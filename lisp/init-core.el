;;; init-core.el --- Sane defaults, GC, encoding, var/ paths -*- lexical-binding: t; -*-

;;; Commentary:

;; Settings that belong to no single feature: GC, encoding, file
;; handling, and the `var/' directory for persistent state (recentf,
;; savehist, save-place, bookmarks, ...).

;;; Code:

;;;; Persistent-state directory
;;
;; No `no-littering': the set of paths this config writes is small,
;; so each module themes its vars by hand via `jotain-var-file'.

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

;; Restore the GC threshold after the early-init.el bump on
;; `emacs-startup-hook' (late depth), so all of init runs under the bump.
;; 16 MiB: high enough that typing/scrolling rarely trips a GC, low
;; enough that an idle GC finishes quickly. If startup aborts before the
;; hook runs, the threshold stays at `most-positive-fixnum' until the
;; first minibuffer exit (see the hooks below).
(defconst jotain-core-gc-cons-threshold (* 16 1024 1024)
  "Steady-state `gc-cons-threshold' after startup.")

(defun jotain-core--gc-restore-after-startup ()
  "Drop `gc-cons-threshold' to its steady-state value once startup is done."
  (setq gc-cons-threshold jotain-core-gc-cons-threshold
        gc-cons-percentage 0.1))
(add-hook 'emacs-startup-hook #'jotain-core--gc-restore-after-startup 90)

;; Collect on idle only once a quarter of the threshold has been consed.
;; Measured against the live threshold, this also skips the collection
;; while the minibuffer hooks below hold it at `most-positive-fixnum'.
(defun jotain-core--gc-idle-collect ()
  "Run a GC when Emacs has been idle, if enough has been allocated."
  (garbage-collect-maybe 4))
(run-with-idle-timer 5 t #'jotain-core--gc-idle-collect)

;; Pause GC while the minibuffer is open: completion allocates heavily
;; and a GC mid-keystroke is felt as input lag. The depth guard keeps the
;; pause while an outer minibuffer is still active (recursive minibuffers
;; are enabled).
(defun jotain-core--gc-defer ()
  "Pause GC for the duration of a minibuffer session."
  (setq gc-cons-threshold most-positive-fixnum))
(defun jotain-core--gc-restore ()
  "Restore steady-state GC once the outermost minibuffer exits."
  (when (< (minibuffer-depth) 2)
    (setq gc-cons-threshold jotain-core-gc-cons-threshold)))
(add-hook 'minibuffer-setup-hook #'jotain-core--gc-defer)
(add-hook 'minibuffer-exit-hook #'jotain-core--gc-restore)

;; UTF-8 everywhere.
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;;; @doc Sane defaults for the bare editor — fill column, dialog/box use,
;;; lockfiles, recursive minibuffers, case-insensitive completion.
(use-package emacs
  :ensure nil
  :custom
  (fill-column 100)
  (use-short-answers t)
  (read-answer-short t)
  (list-matching-lines-jump-to-current-line nil)
  ;; Read-only files open in `view-mode' (SPC/DEL to page, q to quit).
  (view-read-only t)
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
  ;; Drag a region to move/copy it (also to and from other programs);
  ;; drag the mode-line buffer name to move the buffer to another window.
  (mouse-drag-and-drop-region t)
  (mouse-drag-and-drop-region-cross-program t)
  (mouse-drag-mode-line-buffer t)
  (kill-do-not-save-duplicates t)
  (save-interprogram-paste-before-kill t)
  (set-mark-command-repeat-pop t)
  (redisplay-skip-fontification-on-input t)
  (enable-recursive-minibuffers t)
  (minibuffer-follows-selected-frame t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Case-insensitive completion everywhere.
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :config
  (context-menu-mode 1)
  (line-number-mode 1)
  (column-number-mode 1)
  (minibuffer-depth-indicate-mode 1))

;;; @doc Persist point per file across sessions. Built-in.
(use-package saveplace
  :ensure nil
  :custom (save-place-file (jotain-var-file "save-place.el"))
  :config
  (save-place-mode 1)

  ;; Recenter after `save-place-mode' restores point, or a reopened file
  ;; can leave point on the bottom line. Deferred with a zero-delay timer
  ;; because the window doesn't exist yet when the hook fires.
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
  (defun jotain-core--auto-create-missing-dirs ()
    "Create the parent directory of the visited file if it does not exist."
    (let ((target-dir (when buffer-file-name
                        (file-name-directory buffer-file-name))))
      (when (and target-dir (not (file-exists-p target-dir)))
        (make-directory target-dir t))))
  (add-to-list 'find-file-not-found-functions
               #'jotain-core--auto-create-missing-dirs))

;;;; custom-file writes never prompt

;; `custom-file' is write-only (init.el) and rewritten on every
;; `custom-save-all', e.g. when package.el persists
;; `package-selected-packages'. When two Emacs sessions share `var/', the
;; file changes on disk under the writer and the save blocks on the
;; "changed on disk; really edit the buffer?" prompt (a declined prompt
;; aborts the write). The file is disposable, so clobbering it is right:
;; suppress the prompt during the save.
(defun jotain-core--custom-save-without-supersession (orig &rest args)
  "Run ORIG (`custom-save-all') with ARGS, never prompting on disk changes."
  (let ((saved (symbol-function 'ask-user-about-supersession-threat)))
    (unwind-protect
        (progn
          (fset 'ask-user-about-supersession-threat #'ignore)
          (apply orig args))
      (fset 'ask-user-about-supersession-threat saved))))
(advice-add 'custom-save-all :around
            #'jotain-core--custom-save-without-supersession)

;;;; package.el conveniences (newcomers-presets theme)

;; Emacs 31, guarded for 30: `package-autosuggest-mode' offers to install
;; a mode for unknown file types; `package-menu-use-current-if-no-marks'
;; nil makes package-menu actions apply only to marked entries, never to
;; the line at point. `package' is already loaded (init.el).
(when (boundp 'package-menu-use-current-if-no-marks)
  (setopt package-menu-use-current-if-no-marks nil))
(when (fboundp 'package-autosuggest-mode)
  (package-autosuggest-mode 1))

;;; @doc Repeat-mode lets you press the trailing key alone after a prefix
;;; command (e.g. C-x o o o instead of C-x o C-x o). Built-in, enabled
;;; globally. `repeat-exit-timeout' drops the repeat map after two idle
;;; seconds. Built-in maps already cover window resizing
;;; (`C-x ^ ^ v'); init-keys.el only adds a map for the Emacs 31
;;; `window-layout-*' commands.
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
  :bind ([remap list-buffers] . ibuffer)
  :config
  ;; Emacs 31+: human-readable Size column (KB/MB).
  (when (boundp 'ibuffer-human-readable-size)
    (setopt ibuffer-human-readable-size t)))

;;; @doc Tame `find-file-at-point` so an unknown hostname doesn't block
;;; the editor on a DNS lookup — reject means "treat as not a host".
(use-package ffap
  :ensure nil
  ;; On-demand only, yet an eager load cost ~67ms at startup (isolated
  ;; Emacs 31 measurement). Its autoloads load it when a ffap command
  ;; runs, and the `:custom' value applies then.
  :defer t
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
     ("Asia/Shanghai"   "Shanghai")))
  :config
  ;; Emacs 31+: list zones in chronological order.
  (when (boundp 'world-clock-sort-order)
    (setopt world-clock-sort-order "%FT%T")))

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
Useful for diagnosing freezes: start, reproduce, stop."
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
;; Option-as-Meta breaks typing braces and special characters on
;; European layouts, so Meta goes on Command and Right-Option stays free
;; for character entry.
(when (eq system-type 'darwin)
  (setopt mac-command-modifier      'meta
          mac-option-modifier       'super
          mac-right-option-modifier 'none)
  (setopt trash-directory "~/.Trash"))

;;; @doc Inherits PATH, MANPATH, and other shell-managed vars from the
;;; user's login shell so a GUI, launchd, or systemd-spawned Emacs sees
;;; what the terminal sees. The Nix wrapper already puts the tools the
;;; config needs (rg, fd, git, jj, zoxide, coreutils) on PATH; this
;;; adds ~/.nix-profile and user toolchains.
(use-package exec-path-from-shell
  :if (or (daemonp)
          (memq window-system '(mac ns x pgtk)))
  :functions (exec-path-from-shell-initialize)
  ;; Forking the login shell is the largest cost on the init path, so it
  ;; runs on `after-init-hook': the first frame draws first, and PATH is
  ;; set before the command loop starts.
  ;;
  ;; Load-time `executable-find' guards in other modules see the
  ;; pre-import PATH. That is safe only because the tools they probe
  ;; (zoxide, Darwin's gls) come from the Nix wrapper PATH
  ;; (nix/runtime-deps.nix), not the login shell. Keep it that way.
  :hook (after-init . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments nil)) ; skip the -i round-trip

;;; @doc Auto-revert buffers when their file changes on disk (branch
;;; switches, external edits), and non-file buffers such as dired.
(use-package autorevert
  :ensure nil
  :custom (global-auto-revert-non-file-buffers t)
  :config (global-auto-revert-mode 1))

;;; @doc Built-in recently-visited files list, used by
;;; `consult-recent-file'. State lives under var/.
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
  ;; Also persist the kill and search rings. savehist drops a variable
  ;; whole if any part is unprintable; these hold only strings.
  ;; `register-alist' is excluded: one marker (C-x r SPC) or window
  ;; configuration (C-x r w) register would discard every register.
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring))
  :config
  (savehist-mode 1))

;;; @doc Built-in bookmark store, kept under var/. `save-flag 1` writes
;;; on every change so a crash never loses bookmarks; no fringe mark.
(use-package bookmark
  :ensure nil
  ;; Nothing at startup uses bookmarks; the customs apply when
  ;; `bookmark-jump'/`consult-bookmark' load it.
  :defer t
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

;;; @doc Live-highlight regexp constructs (groups, alternation, escapes,
;;; char classes) in the minibuffer while typing a regexp for
;;; `query-replace-regexp', `isearch-*-regexp', `keep-lines', etc.
;;; Built-in since Emacs 30.
(minibuffer-regexp-mode 1)

;;; @doc Built-in HTML renderer used by eww, Gnus, and elfeed. Page
;;; colours and proportional fonts are off, so rendered HTML follows the
;;; theme and the default face.
(use-package shr
  :ensure nil
  ;; On-demand only, yet an eager load cost ~74ms at startup (isolated
  ;; Emacs 31 measurement). Callers require it; the customs apply then.
  :defer t
  :custom
  (shr-use-colors nil)
  (shr-use-fonts nil))

;;; @doc Built-in GnuTLS, hardened: a failed certificate check aborts
;;; the connection instead of continuing (`gnutls-verify-error`), and
;;; weak Diffie-Hellman primes are rejected (`gnutls-min-prime-bits`).
(use-package gnutls
  :ensure nil
  :custom
  (gnutls-verify-error t)
  (gnutls-min-prime-bits 3072))

;;; @doc Built-in Network Security Manager. `network-security-level`
;;; 'high applies the strictest checks (certificate changes, weak
;;; ciphers, downgrades). Settings file under var/.
(use-package nsm
  :ensure nil
  :custom
  (network-security-level 'high)
  (nsm-settings-file (jotain-var-file "network-security.data")))

(provide 'init-core)
;;; init-core.el ends here
