;;; init.el --- Jotain Emacs configuration -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; URL: https://github.com/Jylhis/jotain
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Entry point: register MELPA/NonGNU as fallback archives, put lisp/ on
;; the load-path, and load the per-concern modules in order.
;;
;; Each `init-<concern>.el' owns everything for its concern, built-in and
;; third-party alike: a package that enhances a built-in (dirvish, magit)
;; lives with that built-in (dired, vc). There is deliberately no
;; builtins/third-party split.
;;
;; Nix puts most packages on load-path, so `use-package' finds them
;; without the network; anything else installs from the archives.
;; `use-package-always-ensure' is t (early-init.el), so built-ins must
;; opt out with `:ensure nil'.

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/")     t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; custom.el is write-only: never loaded back, so the config in git stays
;; the single source of truth. It only gives Customize somewhere to write.
(setq custom-file (locate-user-emacs-file "var/custom.el"))

;; The archives are never fetched on the startup path: a launch-time
;; download only slows startup and can take a daemon down on a flaky
;; network. `package-install' downloads only when the on-disk cache is
;; empty; `M-x package-refresh-contents' / `list-packages' refresh.

(require 'init-core)         ; GC, encoding, var/ paths, sane defaults
(require 'init-keys)         ; Global bindings, which-key labels, repeat maps
(require 'init-ui)           ; Theme, modeline, fonts, frame tweaks
(require 'init-tabs)         ; Workspace tabs via tab-bar-mode
(require 'init-help)         ; helpful + built-in help tweaks
(require 'init-docs)         ; Surface jotain.info under C-h i
(require 'init-editing)      ; Electric pairs, delsel, whitespace, region tools
(require 'init-completion)   ; Vertico, marginalia, orderless, consult, corfu
(require 'init-navigation)   ; Dired + dirvish, winner
(require 'init-casual)       ; Transient menus for dired/calc/isearch/ibuffer/Info
(require 'init-vc)           ; vc + magit + diff-hl + forge
(require 'init-prog)         ; prog-mode, treesit, eglot, flymake, eldoc, compile
(require 'init-snippets)     ; tempel snippets + eglot-tempel LSP expansion
(require 'init-project)      ; project + projection + compile-multi
(require 'init-devenv)       ; devenv.sh: tasks, processes, env, LSP, MCP
(require 'init-ai)           ; eca, claude-code-ide, gptel, mcp
(require 'init-shell)        ; eshell, comint, ielm
(require 'init-terminal)     ; ghostel terminal + tty integration (kkp, clipetty)
(require 'init-systems)      ; sops, logview, auth-source-1password
(require 'init-writing)      ; jinx, markdown-mode, denote, pdf-tools
(require 'init-org)          ; org, org-modern, capture templates
(require 'init-http)         ; verb HTTP/REST client (org-based)

;; Languages: Nix, Rust, Python, Go, and QML get their own files;
;; less-used modes are grouped by concern.
(require 'init-lang-nix)
(require 'init-lang-rust)
(require 'init-lang-python)
(require 'init-lang-go)
(require 'init-lang-web)       ; TS/TSX/CSS/HTML/JSON/web-mode
(require 'init-lang-devops)    ; Dockerfile, terraform, just, ansible
(require 'init-lang-data)      ; yaml, csv, sql, jinja2, gnuplot
(require 'init-lang-systems)   ; C/C++, CMake, Meson, Haskell, OCaml, Zig
(require 'init-lang-qml)        ; QML (Quickshell / Qt Quick)

(provide 'init)
;;; init.el ends here
