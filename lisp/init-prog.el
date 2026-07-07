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
  (prog-mode . display-fill-column-indicator-mode)
  :config
  ;; Emacs 31+: don't draw the indicator in a warning face past the
  ;; column. Guarded so the config still loads on Emacs 30.
  (when (boundp 'display-fill-column-indicator-warning)
    (setopt display-fill-column-indicator-warning nil)))

;;;; Tree-sitter

;;; @doc Built-in tree-sitter substrate. Bumped to font-lock level 4 to
;;; enable every available syntactic decoration.
(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

;;; @doc Defer fontification of newly exposed text by a few hundredths of
;;; a second so heavy treesit level-4 font-lock never blocks keystrokes or
;;; scrolling. Pairs with `redisplay-skip-fontification-on-input' (set in
;;; init-core) — matters more on this slower Intel CPU.
(use-package jit-lock
  :ensure nil
  :custom
  (jit-lock-defer-time 0.05))

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

;;;; Non-tree-sitter mode diagnostic

;;; @doc Log to *Messages* whenever a buffer lands in a classic major
;;; mode even though a tree-sitter variant with a loadable grammar
;;; exists. Surfaces gaps where the config should route to `-ts-mode'.
;;; Set `jotain-prog-warn-non-ts-mode' to nil to silence.
(eval-when-compile (require 'treesit-auto))
(declare-function treesit-ready-p "treesit" (language &optional quiet))
(declare-function treesit-auto-recipe-ts-mode "treesit-auto" (recipe))
(declare-function treesit-auto-recipe-lang "treesit-auto" (recipe))
(declare-function treesit-auto-recipe-remap "treesit-auto" (recipe))

(defcustom jotain-prog-warn-non-ts-mode t
  "When non-nil, log if a classic major mode is used despite a ready ts-mode.
The notice goes to *Messages* on `after-change-major-mode-hook' only
when the corresponding tree-sitter grammar is actually loadable."
  :type 'boolean
  :group 'jotain)

(defvar jotain-prog--non-ts-remap-table nil
  "Hash table mapping a classic major-mode symbol to (TS-MODE . LANG).
Built once from `treesit-auto-recipe-list'.")

(defun jotain-prog--build-non-ts-remap-table ()
  "Populate `jotain-prog--non-ts-remap-table' from treesit-auto recipes."
  (when (bound-and-true-p treesit-auto-recipe-list)
    (let ((table (make-hash-table :test #'eq)))
      (dolist (recipe treesit-auto-recipe-list)
        (let ((ts-mode (treesit-auto-recipe-ts-mode recipe))
              (lang (treesit-auto-recipe-lang recipe))
              (remap (treesit-auto-recipe-remap recipe)))
          (dolist (classic (if (proper-list-p remap) remap (list remap)))
            (when (and classic ts-mode lang)
              (puthash classic (cons ts-mode lang) table)))))
      (setq jotain-prog--non-ts-remap-table table))))

(defun jotain-prog--warn-non-ts-mode ()
  "Log when `major-mode' is classic but a ready tree-sitter mode exists.
Runs on `after-change-major-mode-hook'; O(1) hash lookup on the miss path."
  (when (and jotain-prog-warn-non-ts-mode
             jotain-prog--non-ts-remap-table)
    (when-let* ((entry (gethash major-mode jotain-prog--non-ts-remap-table))
                (ts-mode (car entry))
                (lang (cdr entry))
                ((fboundp ts-mode))
                ((treesit-ready-p lang t)))
      (message "jotain: %s opened in %s; %s (tree-sitter, grammar `%s') is available"
               (buffer-name) major-mode ts-mode lang))))

(jotain-prog--build-non-ts-remap-table)
(add-hook 'after-change-major-mode-hook #'jotain-prog--warn-non-ts-mode)

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
  :custom (combobulate-key-prefix "C-c o")
  :bind (:map combobulate-key-map
              ("M-P" . combobulate-drag-up)
              ("M-N" . combobulate-drag-down)))

;;;; Eglot

;; Modern LSP servers routinely send multi-megabyte responses.  Bumping
;; the read buffer cuts the number of read(2) calls dramatically.
(setopt read-process-output-max (* 4 1024 1024))

;;; @doc Security gate for JavaScript-config-evaluating language servers.
;;; ESLint and Tailwind LSPs can execute project-controlled JS config, so
;;; keep them opt-in.
(defcustom jotain-prog-enable-risky-js-lsp nil
  "When non-nil, include ESLint and Tailwind LSP servers in TS/TSX `rass` sessions.
These servers may evaluate project JavaScript configuration files."
  :type 'boolean
  :group 'jotain)

;;; @doc Built-in LSP client. Per-language `eglot-ensure` hooks live
;;; here so all LSP wiring is visible in one place; per-language
;;; mode regexes stay in their `init-lang-*` file. C-c r is the
;;; refactor prefix (rename/format/code-actions).
;;;
;;; Beyond the curated hook list, `jotain-prog--maybe-eglot-ensure'
;;; auto-starts eglot for ANY project file whose language server is on the
;;; buffer's (envrc-applied) PATH — so enabling `languages.X.enable' in a
;;; project's devenv lights up its LSP in Emacs with no per-language config.
(use-package eglot
  :ensure nil
  :preface
  ;; Declared so the byte-compiler stays quiet in these helpers even when
  ;; eglot itself is not loaded at compile time.
  (declare-function eglot-ensure "eglot")
  (declare-function eglot--guess-contact "eglot")
  (defvar eglot--managed-mode)

  (defun jotain-prog--eglot-guess-program ()
    "Executable eglot would use for this buffer, or nil.
Resolved via `eglot--guess-contact', which evaluates function-valued
`eglot-server-programs' entries, so it reflects the project's env.  Returns
nil for exotic contacts (TCP, class forms) we can't cheaply inspect.  Assumes
eglot is loaded."
    (ignore-errors
      (let ((contact (nth 3 (eglot--guess-contact))))
        (cond ((stringp contact) contact)
              ((and (consp contact) (stringp (car contact))) (car contact))))))

  (defun jotain-prog--maybe-eglot-ensure ()
    "Auto-start eglot when the project's env provides a server for this buffer.
Deferred to an idle timer so envrc has already applied the buffer-local
`exec-path'/PATH (envrc runs on `after-change-major-mode-hook', after
`prog-mode-hook').  Skips remote files, non-file and Lisp buffers (no server,
and we must not pull eglot into every elisp buffer), and already-managed
buffers."
    (when (and buffer-file-name
               (not (file-remote-p default-directory))
               (not (derived-mode-p 'emacs-lisp-mode 'lisp-data-mode))
               (not (bound-and-true-p eglot--managed-mode))
               (project-current))
      (let ((buf (current-buffer)))
        (run-with-idle-timer
         0 nil
         (lambda ()
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (unless (bound-and-true-p eglot--managed-mode)
                 (require 'eglot)
                 (when-let* ((prog (jotain-prog--eglot-guess-program)))
                   (when (executable-find prog) ; uses buffer-local exec-path
                     (eglot-ensure)))))))))))

  (defun jotain-prog--risky-js-extras ()
    "Optional ESLint/Tailwind `rass' companions, gated on the risky-JS opt-in.
These servers may evaluate project-controlled JavaScript config, so they are
added only when `jotain-prog-enable-risky-js-lsp' is non-nil and they are on
PATH."
    (let (extras)
      (when jotain-prog-enable-risky-js-lsp
        (dolist (s '("eslint-lsp" "tailwindcss-language-server"))
          (when (executable-find s)
            (setq extras (append extras (list "--" s "--stdio"))))))
      extras))

  (defun jotain-prog--ts-server (&optional _interactive)
    "Resolve the TS/TSX server contact against the buffer's (project) PATH.
Prefers the `rass' multiplexer when it and typescript-language-server are on
PATH, else a plain typescript-language-server.  Called at eglot connect time,
so it sees the project's devenv env — not Jotain's own shell."
    (if (and (executable-find "rass")
             (executable-find "typescript-language-server"))
        (append '("rass" "--" "typescript-language-server" "--stdio")
                (jotain-prog--risky-js-extras))
      '("typescript-language-server" "--stdio")))

  (defun jotain-prog--python-server (&optional _interactive)
    "Resolve the Python server contact against the buffer's (project) PATH.
Prefers the bundled `rass python' preset (basedpyright + ruff), then a lone
basedpyright/pyright, then pylsp.  Resolved at connect time in the project env."
    (cond ((and (executable-find "rass")
                (executable-find "basedpyright")
                (executable-find "ruff"))
           '("rass" "python"))
          ((executable-find "basedpyright") '("basedpyright-langserver" "--stdio"))
          ((executable-find "pyright-langserver") '("pyright-langserver" "--stdio"))
          (t '("pylsp"))))
  :init
  ;; General devenv-aware auto-start for languages beyond the curated list.
  ;; Registered outside eglot's deferral so it also fires for the first file
  ;; of an as-yet-unloaded language.
  (add-hook 'prog-mode-hook #'jotain-prog--maybe-eglot-ensure)
  ;; Per-language modules register modes only; curated auto-start lives here.
  ;; Kept as an explicit, zero-regression safety net alongside the general
  ;; hook above (`eglot-ensure' is idempotent, so the two compose cleanly).
  :hook
  ((dockerfile-mode
    go-ts-mode go-mod-ts-mode go-work-ts-mode
    nix-ts-mode
    python-mode python-ts-mode
    rust-mode rust-ts-mode
    tsx-ts-mode
    typescript-mode typescript-ts-mode
    zig-ts-mode)
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

  ;; Emacs 31+: render LSP hover/signature docs through the tree-sitter
  ;; markdown viewer instead of the plain-text fallback. Guarded so the
  ;; config still loads on Emacs 30 where the option doesn't exist.
  (when (and (boundp 'eglot-documentation-renderer)
             (fboundp 'markdown-ts-view-mode))
    (setopt eglot-documentation-renderer 'markdown-ts-view-mode))

  ;; Emacs 31+: suppress the new inline "a code action is available here"
  ;; indicators — some servers make them noisy. Guarded for Emacs 30.
  (when (boundp 'eglot-code-action-indications)
    (setopt eglot-code-action-indications nil))

  ;; Inlay hints, opt-in per major mode. Add to this list as you grow.
  (when (fboundp 'eglot-inlay-hints-mode)
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (when (apply #'derived-mode-p
                             '(go-ts-mode
                               rust-mode rust-ts-mode
                               typescript-mode typescript-ts-mode
                               python-mode python-ts-mode
                               zig-ts-mode))
                  (eglot-inlay-hints-mode 1)))))

  ;; Server overrides — most languages don't need an entry, eglot has
  ;; sensible defaults. Add only when you want a specific server name.
  (add-to-list 'eglot-server-programs
               '((go-ts-mode go-mod-ts-mode go-work-ts-mode) . ("gopls")))
  (add-to-list 'eglot-server-programs
               '(dockerfile-mode . ("docker-langserver" "--stdio")))

  ;; gopls workspace configuration is set buffer-locally in init-lang-go
  ;; (`jotain-go--eglot-workspace-config') rather than globally here, so
  ;; it never leaks into other languages' eglot sessions.

  ;; rassumfrassum (`rass`) multiplexes several real LSP servers behind a
  ;; single stdio connection so eglot effectively drives multiple servers
  ;; per buffer.  Registered as function-valued contacts so discovery runs
  ;; at eglot connect time in the buffer's (project/devenv) environment —
  ;; not once at startup against Jotain's own shell.  Each resolver falls
  ;; back to a plain single server when `rass' or a companion is absent, so
  ;; a project that lacks them still gets LSP.
  (add-to-list 'eglot-server-programs
               (cons '(tsx-ts-mode typescript-ts-mode typescript-mode)
                     #'jotain-prog--ts-server))
  (add-to-list 'eglot-server-programs
               (cons '(python-mode python-ts-mode)
                     #'jotain-prog--python-server))

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

;;;; Debugging — dape (Debug Adapter Protocol)

;;; @doc Debug Adapter Protocol client — the debugging counterpart to
;;; eglot. Ships adapter configs for dlv (Go), debugpy (Python),
;;; codelldb (Rust/C/C++), and more; the adapter binary (e.g. `dlv`)
;;; comes from the project/host PATH, same convention as the LSP
;;; servers. `C-x C-a` is the prefix (the gud convention); stepping
;;; commands carry repeat-maps, so `C-x C-a n n n` keeps stepping.
(use-package dape
  :bind-keymap ("C-x C-a" . dape-global-map)
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-default-breakpoints-file (jotain-var-file "dape-breakpoints"))
  :config
  ;; Restore the previous session's breakpoints on first use; persist
  ;; them at exit. Both run in :config, so a session that never loads
  ;; dape never touches the breakpoints file.
  (dape-breakpoint-load)
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save))

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
  (eldoc-echo-area-display-truncation-message nil)
  ;; Prefer the dedicated doc buffer over a truncated echo-area line when
  ;; one is already visible — pairs well with `eldoc-help-at-pt' below.
  (eldoc-echo-area-prefer-doc-buffer t)
  :config
  ;; Emacs 31+: also surface `help-at-pt' text (e.g. flymake diagnostics,
  ;; button help) through eldoc, no explicit command needed. Guarded so
  ;; the config still loads on Emacs 30 where the option doesn't exist.
  (when (boundp 'eldoc-help-at-pt)
    (setopt eldoc-help-at-pt t)))

;;;; xref

;;; @doc Built-in cross-reference engine. Pinned to ripgrep (in the
;;; devenv shell) for orders-of-magnitude faster project-wide
;;; lookups than default grep.
(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep))

;;;; tagref

;;; @doc Cross-reference checker for `[tag:x]'/`[ref:x]' directives. Adds
;;; completion, xref navigation (M-. jumps from a ref to its tag, M-? finds
;;; references), and `M-x tagref-check' (clickable compilation buffer).
;;; Needs the `tagref' CLI on PATH (dev shell / Home Manager wrapper).
;;; Provided by Nix (not on MELPA).
(use-package tagref
  :ensure nil ; Provided by Nix
  :commands (tagref-mode)
  :hook (prog-mode . jotain-tagref--maybe-enable)
  :init
  (defun jotain-tagref--maybe-enable ()
    "Enable `tagref-mode' only inside a project.
`tagref-mode' signals a `user-error' when there is no project (e.g. the
daemon's *scratch* buffer in `lisp-interaction-mode'), which aborts
daemon startup before `server-start' with exit 255.  Decline silently
outside a project."
    (when (project-current)
      (tagref-mode 1))))

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
;;; one Emacs without leaking environment. `C-c e' is envrc's command
;;; prefix: `C-c e a' allow, `C-c e r' reload, `C-c e R' reload-all
;;; (the last picks up a `direnv allow' run in a terminal). Manual
;;; allow is kept as the secure default — no auto-allow whitelist.
(use-package envrc
  :demand t
  :functions (envrc-global-mode)
  :bind-keymap ("C-c e" . envrc-command-map)
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
  (add-to-list 'apheleia-formatters
               '(meson-format . ("meson" "format"
                                  "--source-file-path" filepath
                                  "-")))
  (add-to-list 'apheleia-mode-alist '(meson-mode . meson-format))
  (add-to-list 'apheleia-formatters
               '(zig-fmt . ("zig" "fmt" "--stdin")))
  (add-to-list 'apheleia-mode-alist '(zig-ts-mode . zig-fmt))
  ;; Apheleia ships gofmt/goimports/gofumpt formatters but maps Go
  ;; modes to plain gofmt; prepend a goimports mapping (gofmt plus
  ;; import management) to shadow it.  The binary comes from the
  ;; project/host PATH, like gopls and dlv.
  (add-to-list 'apheleia-mode-alist '(go-ts-mode . goimports))
  ;; buildifier reads from stdin; `-path' lets it infer the Starlark
  ;; dialect (BUILD vs WORKSPACE vs .bzl).  Mapping the `bazel-mode'
  ;; parent covers every derived Starlark-family mode (build/workspace/
  ;; module/repo/starlark) while leaving the conf-derived bazelrc /
  ;; bazeliskrc / bazelignore modes untouched.
  (add-to-list 'apheleia-formatters
               '(buildifier . ("buildifier" "-path" (or filepath "BUILD"))))
  (add-to-list 'apheleia-mode-alist '(bazel-mode . buildifier))
  ;; Apheleia ships gofmt/goimports/gofumpt formatters but maps Go
  ;; modes to plain gofmt; prepend a goimports mapping (gofmt plus
  ;; import management) to shadow it.  The binary comes from the
  ;; project/host PATH, like gopls and dlv.
  (add-to-list 'apheleia-mode-alist '(go-ts-mode . goimports))
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
