;;; init-prog.el --- Programming-mode glue: treesit, eglot, flymake, eldoc -*- lexical-binding: t; -*-

;;; Commentary:

;; The shared substrate every language module builds on. Eglot lives here
;; (not in its own file) because it's the glue between `prog-mode',
;; `flymake', `eldoc', and `xref' — splitting it out would just spread
;; one feature across five files.
;;
;; Per-language hooks (eglot-ensure, treesit grammar pinning, formatter
;; choice) live in `init-lang-*.el'. This file owns the substrate.

;;; Code:

;;;; prog-mode

(use-package prog-mode
  :ensure nil
  :hook
  ;; hl-line and show-paren are enabled globally in init-ui.el.
  (prog-mode . display-fill-column-indicator-mode))

;;;; Tree-sitter

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

;; Auto-route foo-mode → foo-ts-mode when grammar is loadable. Grammar
;; installation is suppressed because Nix provides them via
;; treesit-extra-load-path (set in early-init.el from $TREE_SITTER_DIR).
(use-package treesit-auto
  :demand t
  :custom (treesit-auto-install nil)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Code folding driven by treesit nodes.
(use-package treesit-fold
  :diminish
  :hook (after-init . global-treesit-fold-indicators-mode)
  :custom (treesit-fold-indicators-priority -1))

;; Combobulate: structural editing via treesit. Loaded on demand only —
;; enable per buffer with `M-x combobulate-mode' or via .dir-locals.el.
(use-package combobulate
  :ensure nil ; Provided by Nix
  :defer t
  :commands combobulate-mode
  :custom (combobulate-key-prefix "C-c o"))

;;;; Eglot

;; Modern LSP servers routinely send multi-megabyte responses.  Bumping
;; the read buffer cuts the number of read(2) calls dramatically.
(setopt read-process-output-max (* 4 1024 1024))

(use-package eglot
  :ensure nil
  ;; Per-language modules add themselves to eglot-ensure in their own files.
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

  ;; Silence eglot's JSON-RPC event log entirely.
  (fset #'jsonrpc--log-event #'ignore))

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

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

(use-package eldoc
  :ensure nil
  :diminish
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-print-after-edit t)
  (eldoc-idle-delay 0.2)
  (eldoc-echo-area-display-truncation-message nil))

;;;; xref

(use-package xref
  :ensure nil
  :custom
  ;; ripgrep is in the devenv shell. Big speedup over default grep.
  (xref-search-program 'ripgrep))

;;;; Compile

(use-package compile
  :ensure nil
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-ask-about-save nil)
  (compilation-always-kill t))

;;;; editorconfig (built-in since 30)

(use-package editorconfig
  :ensure nil
  :diminish
  :hook (prog-mode . editorconfig-mode))

;;;; Per-project environment + format-on-save + grep refactor

;; envrc gives each buffer its project's nix/devenv environment. Strictly
;; better than direnv-mode because the env is buffer-local, not global.
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
                 (display-buffer-no-window))))

;; Async format-on-save via external formatters (ruff, nixfmt, rustfmt,
;; prettier, etc.). Replaces hand-rolled before-save hooks per language.
(use-package apheleia
  :diminish apheleia-mode
  :functions (apheleia-global-mode)
  :config
  (apheleia-global-mode 1)
  (put 'apheleia-mode 'safe-local-variable #'booleanp))

;; Edit grep/ripgrep result buffers in place; saving propagates edits
;; to all matched files. The classic Emacs project-wide refactor flow:
;; consult-ripgrep → C-c C-o (embark-export) → C-x C-q (wgrep) → edit → C-c C-c.
(use-package wgrep
  :defer t
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; Detect indent width from file contents.
(use-package dtrt-indent
  :diminish
  :hook (prog-mode . dtrt-indent-mode))

(provide 'init-prog)
;;; init-prog.el ends here
