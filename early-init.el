;;; early-init.el --- Pre-init hooks -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>

;;; Commentary:

;; Loaded before package.el, the first frame, and init.el. Only what
;; must happen before a frame is drawn or packages activate lives here.

;;; Code:

;; Effectively no GC during startup; `init-core' restores a runtime value.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; LTR-only: skip bidi reordering and the bidi parenthesis algorithm,
;; which otherwise run on every redisplay and cost in large files.
(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right)
(setopt bidi-inhibit-bpa t)

;; Skip .elc mtime checks, except in batch where stale .elc files bite.
(setq load-prefer-newer noninteractive)

;; `package-quickstart-file' must be pinned here, before `startup.el'
;; runs `package-activate-all': its default path is outside `var/', so
;; without the pin quickstart is never loaded yet still pays its refresh
;; cost on every `package-install'. The file caches absolute /nix/store
;; load-path entries, so delete it whenever the Nix generation (hashed
;; from EMACSLOADPATH) changes, or a redeploy could activate stale
;; package versions.
(let* ((qs (expand-file-name "var/package-quickstart.el" user-emacs-directory))
       (stamp (expand-file-name "var/package-quickstart.gen" user-emacs-directory))
       (gen (secure-hash 'sha256 (or (getenv "EMACSLOADPATH") "")))
       (old (ignore-errors (with-temp-buffer
                             (insert-file-contents stamp) (buffer-string)))))
  (when (and (file-exists-p qs) (not (equal gen old)))
    (ignore-errors (delete-file qs))
    (ignore-errors (delete-file (concat qs "c"))))
  (unless (equal gen old)
    (ignore-errors
      (make-directory (file-name-directory stamp) t)
      (write-region gen nil stamp nil 'silent))))
(defvar package-quickstart nil)
(setq package-quickstart-file
      (expand-file-name "var/package-quickstart.el" user-emacs-directory)
      package-quickstart t)

;; `package-quickstart-refresh' (run after every `package-install')
;; re-calls `package-initialize', tripping a false-positive "Unnecessary
;; call to `package-initialize' in init file" warning. The internal call
;; can't be told apart from a user one, so suppress the warning type.
(defvar warning-suppress-log-types nil)
(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-log-types '(package reinitialization)))

;; Configure use-package before init.el loads any `use-package` form.
;; With `always-ensure', built-ins must opt out with `:ensure nil'.
(defvar use-package-enable-imenu-support nil)
(defvar use-package-always-ensure nil)
(setq use-package-enable-imenu-support t
      use-package-always-ensure t)

;; Opt-in startup profiling: with JOTAIN_PROFILE_STARTUP set, use-package
;; records per-block timings for `M-x use-package-report'. Off by default
;; because collecting them has a per-block cost.
(defvar use-package-compute-statistics nil)
(when (getenv "JOTAIN_PROFILE_STARTUP")
  (setq use-package-compute-statistics t))

;; Disable UI chrome before the first frame is drawn, which is cheaper
;; than toggling the modes off afterwards. The menu bar stays on.
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist))

;; Don't resize the frame when the font, fringes, or bars change during
;; startup: each implied resize is a round-trip to the window system.
(setq frame-inhibit-implied-resize t)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

;; Silence obsolete-symbol warnings from third-party packages.
(setq byte-compile-warnings '(not obsolete))

;; Don't compact font caches during GC: a little RAM for smoother
;; redisplay, especially with many installed fonts.
(setq inhibit-compacting-font-caches t)

;; Thinner font smoothing matches the system rendering on Retina.
(defvar ns-use-thin-smoothing nil)
(when (eq system-type 'darwin)
  (setq ns-use-thin-smoothing t))

;; Native compilation: eln-cache under var/. Async warnings keep their
;; default: since 30, `native-comp-async-warnings-errors-kind' limits them
;; to errors and important warnings, and NEWS.30 advises against silencing
;; them.
(defvar native-comp-speed nil)
(defvar native-comp-async-jobs-number 0)
;; Emacs 31+ defcustom in the not-yet-loaded comp-run.el: pre-declare it
;; so the value set below survives its later defcustom.
(defvar native-comp-async-on-battery-power nil)
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Async jobs: half the logical cores, clamped to 1..3. The cap keeps
  ;; a background recompile from starving redisplay and input on big
  ;; machines; small hosts (2-core VMs, nix-on-droid) get one job.
  (setq native-comp-speed 2
        native-comp-async-jobs-number (max 1 (min 3 (/ (num-processors) 2)))
        ;; Emacs 31+: no background native compilation on battery.
        ;; Inert on Emacs 30.
        native-comp-async-on-battery-power nil)
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory))))
  ;; JOTAIN_ELN_PATH: store-resident AOT .eln for this config
  ;; (nix/config-compiled.nix), exported by module.nix's wrapper when
  ;; `services.jotain.nativeCompile.enable' is on and by
  ;; `just run-built-fast'. Unset elsewhere; JIT into var/eln-cache works
  ;; as usual.
  ;;
  ;; Append, never prepend: `startup-redirect-eln-cache' `setcar's the
  ;; head of the list, and the head must stay the writable cache, since
  ;; Emacs writes JIT output to the first writable entry.
  (let ((store (getenv "JOTAIN_ELN_PATH")))
    (when (and store (file-directory-p store))
      (add-to-list 'native-comp-eln-load-path
                   (file-name-as-directory store)
                   t))))

;; Tree-sitter grammars need no setup here: Nixpkgs' site-start.el sets
;; `treesit-extra-load-path' (init-prog.el forwards it to native-comp).

;; Emacs ships no term/xterm-ghostty.el; aliasing Ghostty's TERM to
;; xterm-256color loads term/xterm.el (modifyOtherKeys, 24-bit colour).
;; Must be set here: tty-run-terminal-initialization runs before init.el.
;; ghostel buffers also use TERM=xterm-ghostty, so this covers nested
;; `emacs -nw' there too (rest of the terminal setup: init-terminal.el).
(add-to-list 'term-file-aliases '("xterm-ghostty" . "xterm-256color"))

(provide 'early-init)
;;; early-init.el ends here
