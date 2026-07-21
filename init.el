;;; init.el --- Jotain Emacs configuration -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; URL: https://github.com/Jylhis/jotain
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Entry point. Responsibilities kept to an absolute minimum:
;;
;;   1. Register MELPA as a fallback archive for anything Nix doesn't ship.
;;   2. Put lisp/ on the load-path.
;;   3. Load per-concern modules in the right order.
;;
;; Every module is named `init-<concern>.el' and owns *everything* related
;; to that concern — built-in and third-party alike. A package that only
;; exists to enhance a built-in (dirvish → dired, magit → vc) lives in the
;; same file as the built-in it enhances. There is no "builtins.el" or
;; "third-party.el" split; that split ages badly.
;;
;; Package provenance: Nix (or the devenv shell in dev) provides most
;; packages via load-path, so `use-package' finds them without touching
;; the network. Anything Nix doesn't ship falls through to MELPA at
;; install time. `use-package-always-ensure' is t (set in early-init.el),
;; so every block defaults to "install if missing" — built-ins must opt
;; out with `:ensure nil'.

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/")     t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; custom.el is write-only — we never load it back, to keep the declarative
;; config in git as the single source of truth. It exists only so that
;; `M-x customize' has somewhere to scribble without touching init.el.
(setq custom-file (locate-user-emacs-file "var/custom.el"))

;; Package archive fetching is OPTIONAL and must never block startup or
;; crash the daemon.  Nix (and, in dev, the devenv shell) provides every
;; package this config `:ensure's, so a fresh archive download is only
;; needed for ad-hoc hand installs.  Two rules keep it cheap:
;;
;;   1. The "package is missing" case is left to package.el itself:
;;      `package-install' calls `package--archives-initialize', which
;;      reads the on-disk archive cache (no network) and only refreshes
;;      when that cache is genuinely empty.  We add nothing for it.
;;   2. The "cache is stale" case is handled here, in the background,
;;      once Emacs is idle, and only when the newest cached
;;      `archive-contents' is older than `jotain-package-refresh-max-age'
;;      (or an archive has no cache at all).
;;
;; The background refresh is async and wrapped in `with-demoted-errors',
;; so a dead network downgrades to a log line instead of a fatal error.
;; Per-host opt-out via the JOTAIN_NO_PACKAGE_REFRESH env var (e.g. set it
;; in the systemd unit on offline machines), no need to edit this shared
;; config.
;;
;; Why a disk-mtime check and not `package-archive-contents': this config
;; sets `package-quickstart' (see early-init.el), so `package-activate-all'
;; never runs the full `package-initialize' and `package-archive-contents'
;; is nil on every startup.  A guard on that variable would therefore fire
;; a refresh on every launch; the cache files on disk are the reliable
;; signal for "when did we last refresh".
(defvar jotain-refresh-package-archives
  (not (getenv "JOTAIN_NO_PACKAGE_REFRESH"))
  "When non-nil, warm package archives in the background after startup.")

(defvar jotain-package-refresh-max-age (* 7 24 60 60) ; 7 days
  "Seconds before the cached package archives are considered stale.
The background warm-up refreshes archives only when the newest cached
`archive-contents' file is older than this, or when a configured archive
has no cached file at all.")

(defun jotain--package-archives-stale-p ()
  "Return non-nil when the on-disk package archive cache is missing or old.
Missing means some archive in `package-archives' lacks a cached
`archive-contents'; old means the newest cached file predates
`jotain-package-refresh-max-age'."
  (let ((files (mapcar (lambda (archive)
                         (expand-file-name
                          (format "archives/%s/archive-contents" (car archive))
                          package-user-dir))
                       package-archives)))
    (or (seq-some (lambda (f) (not (file-exists-p f))) files)
        (> (- (float-time)
              (seq-max (mapcar (lambda (f)
                                 (float-time
                                  (file-attribute-modification-time
                                   (file-attributes f))))
                               files)))
           jotain-package-refresh-max-age))))

;; Warm the archive cache in the background once Emacs is idle and only
;; when it is actually stale, so the next hand install doesn't pay the
;; network cost.  Async download + demoted errors: never blocks the
;; daemon, never fails the service.
(defun jotain--refresh-package-archives-maybe ()
  "Refresh the package archive cache in the background when it is stale."
  (when (jotain--package-archives-stale-p)
    (with-demoted-errors "jotain: background package refresh failed: %S"
      (package-refresh-contents t))))

(defun jotain--schedule-package-refresh ()
  "Schedule a background archive refresh for the next idle moment."
  (run-with-idle-timer 2 nil #'jotain--refresh-package-archives-maybe))

(when jotain-refresh-package-archives
  (add-hook 'emacs-startup-hook #'jotain--schedule-package-refresh))

(require 'init-core)         ; GC, encoding, var/ paths, sane defaults
(require 'init-keys)         ; Global keymap and leader-key setup
(require 'init-ui)           ; Theme, modeline, fonts, frame tweaks
(require 'init-tabs)         ; Workspace tabs via tab-bar-mode
(require 'init-help)         ; helpful + built-in help tweaks
(require 'init-docs)         ; Surface jotain.info under C-h i
(require 'init-editing)      ; Electric pairs, delsel, whitespace, region tools
(require 'init-completion)   ; Vertico, marginalia, orderless, consult, corfu
(require 'init-navigation)   ; Dired + dirvish, project, windmove, winner
(require 'init-vc)           ; vc + magit + diff-hl + forge
(require 'init-prog)         ; prog-mode, treesit, eglot, flymake, eldoc, compile
(require 'init-snippets)     ; tempel snippets + eglot-tempel LSP expansion
(require 'init-project)      ; project + projection + compile-multi
(require 'init-devenv)       ; devenv.sh: tasks, processes, env, LSP, MCP
(require 'init-ai)           ; claude-code-ide, gptel, mcp
(require 'init-shell)        ; eshell, comint, ielm
(require 'init-terminal)     ; ghostel terminal + tty integration (kkp, clipetty)
(require 'init-systems)      ; sops, logview, auth-source-1password
(require 'init-tracking)     ; keyfreq, wakatime, activity-watch
(require 'init-writing)      ; jinx, markdown-mode, denote, pdf-tools
(require 'init-org)          ; org, org-modern, capture templates

;; Languages. The well-supported ones (Nix, Rust, Python, Go) each have
;; their own file; less-used modes are grouped by concern so init.el
;; doesn't grow one line per MELPA package.
(require 'init-lang-nix)
(require 'init-lang-rust)
(require 'init-lang-python)
(require 'init-lang-go)
(require 'init-lang-web)       ; TS/TSX/CSS/HTML/JSON/web-mode
(require 'init-lang-devops)    ; Dockerfile, terraform, just, ansible
(require 'init-lang-data)      ; yaml, csv, sql, jinja2, gnuplot
(require 'init-lang-systems)   ; C/C++, CMake, Meson, Haskell, Zig

(provide 'init)
;;; init.el ends here
