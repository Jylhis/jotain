;;; init-prog.el --- Programming-mode glue: treesit, eglot, flymake, eldoc -*- lexical-binding: t; -*-

;;; Commentary:

;; The shared substrate every language module builds on. Eglot lives here
;; (not in its own file) because it's the glue between `prog-mode',
;; `flymake', `eldoc', and `xref' — splitting it out would just spread
;; one feature across five files.
;;
;; Per-language `eglot-ensure' hooks live here so all LSP wiring is
;; visible in one place. Language modules own mode registration and
;; language-specific indentation/settings.

;;; Code:

;;;; prog-mode

;;; @doc Built-in `prog-mode` parent. Just turns on the fill-column
;;; indicator — hl-line and show-paren live in init-ui so they
;;; apply outside programming buffers too.
(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . display-fill-column-indicator-mode))

;;;; Tree-sitter

;;; @doc Built-in tree-sitter substrate. Bumped to font-lock level 4 to
;;; enable every available syntactic decoration.
(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

;;; @doc Auto-routes `foo-mode` → `foo-ts-mode` whenever the grammar is
;;; loadable. Grammar installation is suppressed because Nix
;;; provides them via `treesit-extra-load-path` (set from
;;; $TREE_SITTER_DIR in early-init.el). We use only the alist hook
;;; — `global-treesit-auto-mode` advises `set-auto-mode-0` and
;;; costs ~3.6 s per find-file in this config.
(use-package treesit-auto
  :demand t
  :custom (treesit-auto-install nil)
  :config
  ;; Populate auto-mode-alist with foo-ts-mode entries for every grammar
  ;; Nix provides.  This is sufficient — global-treesit-auto-mode is NOT
  ;; used because it advises set-auto-mode-0 with a hook that rebuilds
  ;; major-mode-remap-alist on every call (62 treesit-ready-p checks,
  ;; ~1.2 s), and set-auto-mode-0 fires 3× per file open, adding ~3.6 s
  ;; per find-file.  The alist entries achieve the same mode routing with
  ;; zero overhead.
  (treesit-auto-add-to-auto-mode-alist 'all))

;;; @doc Code folding driven by treesit syntax nodes — folds along
;;; functions/classes/blocks instead of indentation guesses. Fringe
;;; indicators show fold state.
(use-package treesit-fold
  :diminish
  :hook (after-init . global-treesit-fold-indicators-mode)
  :custom (treesit-fold-indicators-priority -1))

;;; @doc Structural editing via treesit (move/clone/raise nodes,
;;; transpose siblings). Heavy enough to be opt-in per buffer via
;;; M-x combobulate-mode or .dir-locals.el. Provided by Nix.
(use-package combobulate
  :ensure nil ; Provided by Nix
  :defer t
  :commands combobulate-mode
  :custom (combobulate-key-prefix "C-c o"))

;;;; Eglot

;; Modern LSP servers routinely send multi-megabyte responses.  Bumping
;; the read buffer cuts the number of read(2) calls dramatically.
(setopt read-process-output-max (* 4 1024 1024))

;;; @doc Built-in LSP client. Per-language `eglot-ensure` hooks live
;;; here so all LSP wiring is visible in one place; per-language
;;; mode regexes stay in their `init-lang-*` file. C-c r is the
;;; refactor prefix (rename/format/code-actions).
(use-package eglot
  :ensure nil
  ;; Per-language modules register modes only; auto-start lives here.
  :hook
  ((dockerfile-mode
    go-mode go-ts-mode
    nix-ts-mode
    python-mode python-ts-mode
    rust-mode rust-ts-mode
    tsx-ts-mode
    typescript-mode typescript-ts-mode)
   . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-send-changes-idle-time 0.5)
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-report-progress nil)
  :bind
  (:map eglot-mode-map
        ("C-c r r" . eglot-rename)
        ("C-c r f" . eglot-format)
        ("C-c r a" . eglot-code-actions)
        ("C-c r o" . eglot-code-action-organize-imports)
        ("C-c r q" . eglot-code-action-quickfix)
        ("C-h ."   . eldoc-doc-buffer))
  :config
  ;; Compose eldoc sources eagerly — it's the more readable variant
  ;; than Eglot's default in most modes.
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local eldoc-documentation-strategy
                          #'eldoc-documentation-compose-eagerly)))

  ;; Inlay hints, opt-in per major mode. Add to this list as you grow.
  (when (fboundp 'eglot-inlay-hints-mode)
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (when (apply #'derived-mode-p
                             '(go-mode go-ts-mode
                               rust-mode rust-ts-mode
                               typescript-mode typescript-ts-mode
                               python-mode python-ts-mode))
                  (eglot-inlay-hints-mode 1)))))

  ;; Server overrides — most languages don't need an entry, eglot has
  ;; sensible defaults. Add only when you want a specific server name.
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls")))
  (add-to-list 'eglot-server-programs
               '(dockerfile-mode . ("docker-langserver" "--stdio")))

  ;; rassumfrassum (`rass`) multiplexes several real LSP servers behind a
  ;; single stdio connection so eglot effectively drives multiple servers
  ;; per buffer.  Provided by the devenv shell; on bare hosts where `rass'
  ;; is missing, eglot falls back to its built-in lookup.
  (when (executable-find "rass")
    (add-to-list 'eglot-server-programs
                 '((tsx-ts-mode typescript-ts-mode)
                   . ("rass"
                      "--" "typescript-language-server" "--stdio"
                      "--" "eslint-lsp" "--stdio"
                      "--" "tailwindcss-language-server" "--stdio")))
    (add-to-list 'eglot-server-programs
                 '((python-mode python-ts-mode)
                   . ("rass" "python"))))

  ;; Silence eglot's JSON-RPC event log entirely.
  (fset #'jsonrpc--log-event #'ignore))

;;; @doc Consult-driven workspace symbol search — C-M-. opens an
;;; orderless-filtered list of symbols across the LSP workspace.
(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

;;; @doc Embark integration for consult-eglot — gives every workspace
;;; symbol an action menu (jump to def, find refs, rename, …).
(use-package consult-eglot-embark
  :after (consult eglot embark)
  :demand t)

;;;; SonarLint (SonarCloud connected mode)

;; SonarLint provides cross-language code-quality and security diagnostics
;; via the sonarlint-ls language server (nixpkgs).  It runs as a secondary
;; eglot connection alongside the primary language server for the buffer.
;;
;; SonarCloud connected mode is configured per-project via .dir-locals.el:
;;
;;   ((nil . ((eglot-workspace-configuration
;;             . (:sonarlint
;;                (:connectedMode
;;                 (:connections
;;                  (:sonarcloud
;;                   [(:organizationKey "myorg" :token "...")])
;;                  :project
;;                  (:connectionId "myorg" :projectKey "myproject"))
;;                 :disableTelemetry t))))))

(defun jotain-sonarlint ()
  "Start SonarLint analysis in the current project.
Launches the SonarLint language server as a secondary eglot
connection alongside any existing language server."
  (interactive)
  (let ((eglot-server-programs
         `((,major-mode . ("sonarlint-ls" "-stdio")))))
    (call-interactively #'eglot)))

;;;; Flymake / eldoc

;;; @doc Built-in inline diagnostic display. Indicator chars (! ? ·) and
;;; end-of-line message rendering keep diagnostics legible without
;;; opening a side window. M-n / M-p navigate.
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-show-diagnostics-at-end-of-line t) ; Built-in inline display
  (flymake-margin-indicators-string
   '((error   "!" compilation-error)
     (warning "?" compilation-warning)
     (note    "·" compilation-info)))
  :bind
  (:map flymake-mode-map
        ("M-n"     . flymake-goto-next-error)
        ("M-p"     . flymake-goto-prev-error)
        ("C-c ! l" . flymake-show-buffer-diagnostics)
        ("C-c ! p" . flymake-show-project-diagnostics))
  :config
  (defun jotain-prog--disable-flymake-byte-compile ()
    "Disable `elisp-flymake-byte-compile' in non-file buffers like *scratch*."
    (when (and (derived-mode-p 'emacs-lisp-mode)
               (not buffer-file-name))
      (remove-hook 'flymake-diagnostic-functions #'elisp-flymake-byte-compile t)))
  (add-hook 'flymake-mode-hook #'jotain-prog--disable-flymake-byte-compile))

;;; @doc Built-in echo-area documentation. Single-line display plus
;;; small idle delay so it feels responsive without flashing while
;;; you type.
(use-package eldoc
  :ensure nil
  :diminish
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-print-after-edit t)
  (eldoc-idle-delay 0.2)
  (eldoc-echo-area-display-truncation-message nil))

;;;; xref

;;; @doc Built-in cross-reference engine. Pinned to ripgrep (in the
;;; devenv shell) for orders-of-magnitude faster project-wide
;;; lookups than default grep.
(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep))

;;;; Compile

;;; @doc Built-in compile / recompile. Auto-scroll until the first error
;;; and skip the "save?" prompt — the annoying defaults that make
;;; people reach for projectile or compilation-multi alternatives.
(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-ask-about-save nil)
  (compilation-always-kill t))

;;;; editorconfig (built-in since 30)

;;; @doc Honour `.editorconfig` files (indent style/width, line endings,
;;; trailing whitespace). Built-in since Emacs 30.
(use-package editorconfig
  :ensure nil
  :diminish
  :hook (prog-mode . editorconfig-mode))

;;;; Per-project environment + format-on-save + grep refactor

;;; @doc Apply the project's `.envrc` / Nix shell to every buffer in
;;; that project. Strictly better than direnv-mode because the env
;;; is buffer-local, not global — multiple projects can coexist in
;;; one Emacs without leaking environment.
(use-package envrc
  :demand t
  :functions (envrc-global-mode)
  :config
  (envrc-global-mode)
  (add-to-list 'warning-suppress-types '(envrc))

  (defun jotain-prog--envrc-blocked-p (buffer-name _action)
    "Non-nil when the *envrc* buffer reports a blocked .envrc."
    (and (equal buffer-name "*envrc*")
         (when-let* ((buf (get-buffer buffer-name)))
           (with-current-buffer buf
             (save-excursion
               (goto-char (point-max))
               (search-backward "is blocked" nil t))))))

  (add-to-list 'display-buffer-alist
               '(jotain-prog--envrc-blocked-p
                 (display-buffer-no-window)
                 (allow-no-window . t))))

;;; @doc Async format-on-save through external formatters (ruff, nixfmt,
;;; rustfmt, prettier, …). Replaces hand-rolled per-language hooks
;;; with one place to look. Per-buffer override safe-local-variable
;;; lets `.dir-locals.el` opt out.
(use-package apheleia
  :diminish apheleia-mode
  :functions (apheleia-global-mode)
  :config
  (apheleia-global-mode 1)
  (put 'apheleia-mode 'safe-local-variable #'booleanp))

;;; @doc Edit grep / ripgrep result buffers in place; saving propagates
;;; edits to every matched file. Powers the project-wide refactor
;;; flow: consult-ripgrep → C-c C-o (embark-export) → C-x C-q
;;; (wgrep) → edit → C-c C-c.
(use-package wgrep
  :defer t
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;;; @doc Detect indentation width from file contents — saves us from
;;; having to special-case every project's tab/space convention.
(use-package dtrt-indent
  :diminish
  :hook (prog-mode . dtrt-indent-mode))

(provide 'init-prog)
;;; init-prog.el ends here
