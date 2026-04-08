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
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/")        t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu"       . "https://elpa.nongnu.org/nongnu/")    t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; custom.el is write-only — we never load it back, to keep the declarative
;; config in git as the single source of truth. It exists only so that
;; `M-x customize' has somewhere to scribble without touching init.el.
(setq custom-file (locate-user-emacs-file "var/custom.el"))

;; Bootstrap guard: after `just clean-all' the quickstart file may be absent
;; or broken from a partially-completed prior bootstrap. Disable quickstart
;; during bootstrap so each package-install doesn't regenerate a broken
;; quickstart file that prevents subsequent installs from working.
(defvar jotain--bootstrapping-p (not (package-installed-p 'diminish))
  "Non-nil during the first successful run after `just clean-all'.")

(when jotain--bootstrapping-p
  (setq package-quickstart nil)
  (let ((qs (locate-user-emacs-file "package-quickstart.el"))
        (qsc (locate-user-emacs-file "package-quickstart.elc")))
    (when (file-exists-p qs)  (delete-file qs))
    (when (file-exists-p qsc) (delete-file qsc)))
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(require 'init-core)         ; GC, encoding, no-littering, sane defaults
(require 'init-keys)         ; Global keymap and leader-key setup
(require 'init-ui)           ; Theme, modeline, fonts, frame tweaks
(require 'init-help)         ; helpful + built-in help tweaks
(require 'init-editing)      ; Electric pairs, delsel, whitespace, region tools
(require 'init-completion)   ; Vertico, marginalia, orderless, consult, corfu
(require 'init-navigation)   ; Dired + dirvish, project, windmove, winner
(require 'init-vc)           ; vc + magit + diff-hl + forge
(require 'init-prog)         ; prog-mode, treesit, eglot, flymake, eldoc, compile
(require 'init-project)      ; project + projection + compile-multi
(require 'init-ai)           ; claude-code-ide, gptel, mcp
(require 'init-shell)        ; eshell, vterm, comint
(require 'init-systems)      ; sops, logview, auth-source-1password
(require 'init-tracking)     ; keyfreq, wakatime, activity-watch
(require 'init-writing)      ; jinx, markdown-mode, denote, pdf-tools
(require 'init-org)          ; org, org-modern, capture templates

;; Languages. The big three (Nix, Rust, Python) each have their own
;; file; less-used modes are grouped by concern so init.el doesn't
;; grow one line per MELPA package.
(require 'init-lang-nix)
(require 'init-lang-rust)
(require 'init-lang-python)
(require 'init-lang-web)       ; TS/TSX/CSS/HTML/JSON/web-mode
(require 'init-lang-devops)    ; Dockerfile, terraform, just, ansible
(require 'init-lang-data)      ; yaml, csv, sql, jinja2, gnuplot
(require 'init-lang-systems)   ; Go, C/C++, CMake, Haskell

;; After a successful bootstrap, re-enable quickstart and generate a valid
;; file so subsequent startups are fast again.
(when jotain--bootstrapping-p
  (setq package-quickstart t)
  (package-quickstart-refresh))

(provide 'init)
;;; init.el ends here
