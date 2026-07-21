;;; early-init.el --- Pre-init hooks -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>

;;; Commentary:

;; Loaded before package.el, before the GUI initialises, before init.el.
;; Anything that *must* happen before a frame is drawn or before
;; `package-initialize' runs lives here. Everything else lives in init.el
;; or one of the lisp/init-*.el modules.

;;; Code:

;; Garbage collection: bump the threshold to "effectively never" during
;; startup. `init-core' restores a sane runtime value.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Disable bidirectional text scanning for LTR-only usage.  Emacs runs
;; the bidirectional parenthesis algorithm on every redisplay otherwise,
;; which adds noticeable cost in large files.
(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right)
(setopt bidi-inhibit-bpa t)

;; Don't bother validating .elc mtimes at startup, except in batch sessions
;; where stale .elc files cause hard-to-debug surprises.
(setq load-prefer-newer noninteractive)

;; package.el stays on. Nix (and, in dev, the devenv shell) provides most
;; packages via load-path, so they load instantly without touching the
;; network. Anything Nix doesn't ship falls through to MELPA at install
;; time — see init.el for the archive list.
;;
;; `package-quickstart-file' must be pinned HERE, before `startup.el' runs
;; its automatic `package-activate-all'. Without this pin the file ends up
;; at its default `<user-emacs-directory>/package-quickstart.el' path,
;; which is outside `var/' and never gets loaded — quickstart then pays
;; its refresh + byte-compile cost on every `package-install' without
;; providing any startup speedup.
;;
;; The quickstart file caches absolute load-path entries — under the Nix
;; distribution those are /nix/store paths that go stale whenever the
;; closure changes, while var/ persists across deployments. Invalidate
;; the cache when the Nix load-path generation (EMACSLOADPATH already
;; encodes the deps derivation store path) changes, so a redeploy never
;; silently activates old package versions from a stale quickstart.
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

;; `package-quickstart-refresh' (called after every `package-install' via
;; `package--quickstart-maybe-refresh') internally re-calls
;; `package-initialize', which trips the user-facing "Unnecessary call to
;; `package-initialize' in init file" warning as a false positive. There
;; is no way to distinguish the legitimate internal call from a user one,
;; so suppress the warning type directly.
(defvar warning-suppress-log-types nil)
(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-log-types '(package reinitialization)))

;; use-package is built in since Emacs 29; configure it before init.el
;; loads any `use-package` form. `always-ensure' means every block defaults
;; to `:ensure t' — built-ins must opt out with `:ensure nil'.
(defvar use-package-enable-imenu-support nil)
(defvar use-package-always-ensure nil)
(setq use-package-enable-imenu-support t
      use-package-always-ensure t)

;; Disable UI chrome before the first frame is drawn — much faster than
;; toggling the modes off after the fact.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

;; Silence the firehose of obsolete-symbol warnings from third-party
;; packages we don't control. Real warnings still surface.
(setq byte-compile-warnings '(not obsolete))

;; Don't compact font caches during GC. Trades a little RAM for noticeably
;; smoother redisplay, especially on systems with many installed fonts.
(setq inhibit-compacting-font-caches t)

;; macOS: thinner font smoothing matches the system rendering on Retina.
(defvar ns-use-thin-smoothing nil)
(when (eq system-type 'darwin)
  (setq ns-use-thin-smoothing t))

;; Native compilation (Emacs 30+): keep eln-cache out of the config dir,
;; and silence the firehose of async warnings during init.
(defvar native-comp-async-report-warnings-errors nil)
(defvar native-comp-speed nil)
(defvar native-comp-async-jobs-number 0)
;; Emacs 31+ option (a defcustom in comp-run.el). comp-run.el is NOT
;; preloaded, so the symbol is unbound here at early-init time on *every*
;; Emacs version — a `boundp' guard would therefore no-op even on Emacs
;; 31. Pre-declaring with `defvar' both silences the byte-compiler on
;; Emacs 30 and seeds the value so it survives until comp-run.el loads
;; (its defcustom won't clobber an already-set value). Mirrors the
;; `native-comp-speed' / `native-comp-async-jobs-number' handling above.
(defvar native-comp-async-on-battery-power nil)
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Async (background) native compilation jobs: half the logical cores,
  ;; capped at 3, min 1. The cap keeps a background recompile from
  ;; starving redisplay and input on big machines (responsiveness over
  ;; eln-cache warm-up speed); the floor keeps small hosts (2-core VMs,
  ;; nix-on-droid) at a single job. `num-processors' shipped in 28.1,
  ;; below the Emacs 30 floor, so no guard is needed.
  (setq native-comp-async-report-warnings-errors nil
        native-comp-speed 2
        native-comp-async-jobs-number (max 1 (min 3 (/ (num-processors) 2)))
        ;; Emacs 31+: pause background native compilation while on
        ;; battery so a recompile doesn't spin the fans on an unplugged
        ;; laptop. Inert on Emacs 30 (the symbol is just an unused var).
        native-comp-async-on-battery-power nil)
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory)))))

;; Tree-sitter grammars come from Nixpkgs' own site-start.el, which sets
;; `treesit-extra-load-path' to the bundled grammar directory in the full
;; distribution (see init-prog.el, which propagates that path to async
;; native-comp workers).  No `TREE_SITTER_DIR' handling is needed here.

;; Ghostty advertises TERM=xterm-ghostty but Emacs doesn't ship a matching
;; term/xterm-ghostty.el. Aliasing to xterm-256color makes term/xterm.el
;; load instead, which restores modifyOtherKeys + 24-bit colour. Must be
;; set here because tty-run-terminal-initialization fires before init.el.
;; The rest of the terminal story (the ghostel terminal emulator, kkp,
;; clipetty) lives in lisp/init-terminal.el; ghostel's buffers also use
;; TERM=xterm-ghostty, so this alias covers nested `emacs -nw' there.
(add-to-list 'term-file-aliases '("xterm-ghostty" . "xterm-256color"))

(provide 'early-init)
;;; early-init.el ends here
