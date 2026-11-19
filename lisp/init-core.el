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
  "Directory for the configuration's persistent state files.")

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

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(use-package emacs
  :ensure nil
  :custom
  (fill-column 100)
  (use-short-answers t)
  (read-answer-short t)
  (use-dialog-box nil)
  (create-lockfiles nil)
  (delete-by-moving-to-trash t)
  (sentence-end-double-space nil)
  (require-final-newline t)
  (word-wrap t)
  (ring-bell-function #'ignore)
  (scroll-preserve-screen-position 1)
  (kill-do-not-save-duplicates t)
  (set-mark-command-repeat-pop t)
  (redisplay-skip-fontification-on-input t)
  (enable-recursive-minibuffers t)
  ;; One user, one keyboard: a prompt opened on another terminal, or on
  ;; the frameless daemon, must not lock input out of the GUI frame.
  (multiple-terminals-merge-keyboards t)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  (read-extended-command-predicate #'command-completion-default-include-p)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :config
  (minibuffer-depth-indicate-mode 1))

(use-package saveplace
  :ensure nil
  :custom (save-place-file (jotain-var-file "save-place.el"))
  :config
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

(use-package repeat
  :ensure nil
  :custom
  (repeat-exit-timeout 2))

(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

(use-package ibuffer
  :ensure nil
  :bind ([remap list-buffers] . ibuffer)
  :config
  (setopt ibuffer-human-readable-size t))

(use-package ffap
  :ensure nil
  ;; On-demand only, yet an eager load cost ~67ms at startup (isolated
  ;; Emacs 31 measurement). Its autoloads load it when a ffap command
  ;; runs, and the `:custom' value applies then.
  :defer t
  :custom
  (ffap-machine-p-known 'reject))

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
  (setopt world-clock-sort-order "%FT%T"))

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

(use-package exec-path-from-shell
  :if (or (daemonp)
          (memq window-system '(x pgtk)))
  :functions (exec-path-from-shell-initialize)
  ;; Forking the login shell is the largest cost on the init path, so it
  ;; runs on `after-init-hook': the first frame draws first, and PATH is
  ;; set before the command loop starts.
  ;;
  ;; Load-time `executable-find' guards in other modules see the
  ;; pre-import PATH. That is safe only because the tools they probe
  ;; (zoxide) come from the Nix wrapper PATH
  ;; (nix/runtime-deps.nix), not the login shell. Keep it that way.
  :hook (after-init . exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-arguments '("-l"))) ; login shell only, skip -i

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
  (add-to-list 'recentf-exclude jotain-var-dir))

(use-package savehist
  :ensure nil
  :custom
  (savehist-file (jotain-var-file "savehist.el"))
  ;; Also persist the kill and search rings. savehist drops a variable
  ;; whole if any part is unprintable; these hold only strings.
  ;; `register-alist' is excluded: one marker (C-x r SPC) or window
  ;; configuration (C-x r w) register would discard every register.
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring)))

(use-package bookmark
  :ensure nil
  ;; Nothing at startup uses bookmarks; the customs apply when
  ;; `bookmark-jump'/`consult-bookmark' load it.
  :defer t
  :custom
  (bookmark-default-file (jotain-var-file "bookmarks.el"))
  (bookmark-fringe-mark nil)
  (bookmark-save-flag 1))

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (lazy-count-prefix-format "(%s/%s) "))

(use-package shr
  :ensure nil
  ;; On-demand only, yet an eager load cost ~74ms at startup (isolated
  ;; Emacs 31 measurement). Callers require it; the customs apply then.
  :defer t
  :custom
  (shr-use-colors nil)
  (shr-use-fonts nil))

(use-package gnutls
  :ensure nil
  :custom
  (gnutls-verify-error t)
  (gnutls-min-prime-bits 3072))

(use-package nsm
  :ensure nil
  :custom
  (network-security-level 'high)
  (nsm-settings-file (jotain-var-file "network-security.data")))

(provide 'init-core)
;;; init-core.el ends here
