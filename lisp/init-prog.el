;;; init-prog.el --- Programming-mode glue: treesit, eglot, flymake, eldoc -*- lexical-binding: t; -*-

;;; Commentary:

;; The shared substrate every language module builds on.  Eglot lives
;; here because it is the glue between `prog-mode', `flymake', `eldoc' and
;; `xref'.  All LSP wiring (auto-start, server programs) and formatter
;; wiring stays in this file; language modules own mode registration and
;; language-specific settings.

;;; Code:

;;;; prog-mode

;;; @doc Built-in `prog-mode` parent. Turns on the fill-column
;;; indicator; hl-line and show-paren live in init-ui, as they apply
;;; beyond code.
(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  :config
  ;; Emacs 31+: no warning face past the column.
  (when (boundp 'display-fill-column-indicator-warning)
    (setopt display-fill-column-indicator-warning nil)))

;;;; Tree-sitter

;;; @doc Built-in tree-sitter. Font-lock level 4 enables every
;;; available syntactic decoration.
(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

;;; @doc Defer fontification of newly exposed text by 50 ms so heavy
;;; level-4 treesit font-lock never blocks keystrokes or scrolling.
;;; Pairs with `redisplay-skip-fontification-on-input' (init-core).
(use-package jit-lock
  :ensure nil
  :custom
  (jit-lock-defer-time 0.05))

;;; @doc Route classic major modes to their tree-sitter variants through
;;; `major-mode-remap-alist' (no `treesit-auto': Nix ships every grammar).
;;; Remapping the chosen mode means the classic modes' `auto-mode-alist'
;;; entries stay the source of truth. A language whose grammar is not
;;; loadable keeps its classic mode.
(declare-function treesit-ready-p "treesit" (language &optional quiet))

(defvar jotain-prog-ts-remaps
  '((bash        bash-ts-mode        sh-mode)
    (cmake       cmake-ts-mode       cmake-mode)
    (dockerfile  dockerfile-ts-mode  dockerfile-mode)
    (json        json-ts-mode        js-json-mode json-mode)
    (toml        toml-ts-mode        conf-toml-mode)
    (yaml        yaml-ts-mode        yaml-mode)
    ;; Modes routed by their own `init-lang-*' file are listed too, so a
    ;; stray classic-mode path (a package autoload, a stale quickstart
    ;; entry) still lands in the tree-sitter mode.
    (python      python-ts-mode      python-mode)
    (rust        rust-ts-mode        rust-mode)
    (css         css-ts-mode         css-mode scss-mode)
    (javascript  js-ts-mode          js-mode javascript-mode js2-mode)
    (typescript  typescript-ts-mode  typescript-mode)
    ;; C/C++ (init-lang-systems, which also maps `.cu'/`.cuh' straight
    ;; to its own `cuda-ts-mode').
    (c           c-ts-mode           c-mode)
    (cpp         c++-ts-mode         c++-mode))
  "Tree-sitter routing table: entries (LANG TS-MODE CLASSIC-MODE...).
For each entry whose grammar LANG is loadable, every CLASSIC-MODE is
remapped to TS-MODE through `major-mode-remap-alist'.  Go is handled in
`init-lang-go.el', which additionally strips classic go-mode autoloads.")

(defun jotain-prog--apply-ts-remaps ()
  "Populate `major-mode-remap-alist' from `jotain-prog-ts-remaps'.
Only remaps a language whose grammar is loadable (`treesit-ready-p') and
whose tree-sitter mode is defined, so a build without a given grammar
leaves the classic mode in place."
  (dolist (entry jotain-prog-ts-remaps)
    (let ((lang (car entry))
          (ts-mode (cadr entry))
          (classics (cddr entry)))
      (when (and (fboundp ts-mode) (treesit-ready-p lang t))
        (dolist (classic classics)
          (add-to-list 'major-mode-remap-alist (cons classic ts-mode)))))))

(jotain-prog--apply-ts-remaps)

;; Async native-comp workers skip site files, so they miss the
;; `treesit-extra-load-path' Nixpkgs' site-start.el sets.  Pass it on, or
;; compiling a `*-ts-mode' file logs a spurious "grammar unavailable"
;; warning.
(defvar treesit-extra-load-path)
(defvar native-comp-async-env-modifier-form)
(setq native-comp-async-env-modifier-form
      `(setq treesit-extra-load-path ',treesit-extra-load-path))

;;;; Non-tree-sitter mode diagnostic

;;; @doc Log to *Messages* when a buffer opens in a classic major mode
;;; although a tree-sitter variant with a loadable grammar exists: a gap
;;; detector for languages missing from `jotain-prog-ts-remaps'. Silence
;;; it with `jotain-prog-warn-non-ts-mode', or per mode with
;;; `jotain-prog-warn-non-ts-exclude'.
(defcustom jotain-prog-warn-non-ts-mode t
  "When non-nil, log if a classic major mode is used despite a ready ts-mode.
Modes in `jotain-prog-warn-non-ts-exclude' are never reported."
  :type 'boolean
  :group 'jotain)

(defcustom jotain-prog-warn-non-ts-exclude nil
  "Classic major modes the non-ts diagnostic deliberately skips."
  :type '(repeat symbol)
  :group 'jotain)

(defun jotain-prog--warn-non-ts-mode ()
  "Log when `major-mode' is classic but a ready tree-sitter mode exists.
Derives `foo-ts-mode' and the grammar name from `foo-mode', so irregular
names (`sh-mode' to `bash-ts-mode') are missed; those are remapped in
`jotain-prog-ts-remaps' anyway."
  (when (and jotain-prog-warn-non-ts-mode
             (not (memq major-mode jotain-prog-warn-non-ts-exclude)))
    (let ((name (symbol-name major-mode)))
      (when (and (string-suffix-p "-mode" name)
                 (not (string-suffix-p "-ts-mode" name)))
        (let* ((base (string-remove-suffix "-mode" name))
               (ts-mode (intern-soft (concat base "-ts-mode")))
               (lang (intern base)))
          (when (and ts-mode (fboundp ts-mode) (treesit-ready-p lang t))
            (message "jotain: %s opened in %s; %s (tree-sitter, grammar `%s') is available"
                     (buffer-name) major-mode ts-mode lang)))))))

(add-hook 'after-change-major-mode-hook #'jotain-prog--warn-non-ts-mode)

;;; @doc Code folding along treesit syntax nodes (functions, classes,
;;; blocks). Fringe indicators show fold state.
(use-package treesit-fold
  :hook (after-init . global-treesit-fold-indicators-mode)
  :custom (treesit-fold-indicators-priority -1))

;;; @doc Structural editing via treesit (move/clone/raise nodes,
;;; transpose siblings). Opt-in per buffer via M-x combobulate-mode or
;;; .dir-locals.el. Provided by Nix.
(use-package combobulate
  :ensure nil
  :defer t
  :commands combobulate-mode
  :custom (combobulate-key-prefix "C-c o")
  :bind (:map combobulate-key-map
              ("M-P" . combobulate-drag-up)
              ("M-N" . combobulate-drag-down)))

;;;; Eglot

;; LSP servers send multi-megabyte responses; a bigger read buffer means
;; far fewer read(2) calls.
(setopt read-process-output-max (* 4 1024 1024))

;;; @doc Security gate: the ESLint and Tailwind language servers can
;;; execute project-controlled JS config, so they are opt-in.
(defcustom jotain-prog-enable-risky-js-lsp nil
  "When non-nil, include ESLint and Tailwind LSP servers in TS/TSX `rass` sessions.
These servers may evaluate project JavaScript configuration files."
  :type 'boolean
  :group 'jotain)

(defvar eglot-workspace-configuration) ; defined in eglot.el

(defun jotain-eglot-set-workspace-config (key settings)
  "Contribute SETTINGS under KEY to eglot's workspace configuration.
Eglot reads the global value of `eglot-workspace-configuration' (it
evaluates it in a temp buffer), so a buffer-local binding never reaches
the server.  Each language merges its own section into the default
value instead; `copy-sequence' avoids mutating the shared default.  A
project `.dir-locals.el' entry still shadows the whole value.

Deferred until eglot loads.  Call it from the `init-lang-*' file that
owns the settings."
  (with-eval-after-load 'eglot
    (setq-default eglot-workspace-configuration
                  (plist-put (copy-sequence
                              (default-value 'eglot-workspace-configuration))
                             key settings))))

;;; @doc Built-in LSP client. All LSP wiring lives here; mode regexes
;;; stay in the `init-lang-*` files. C-c r is the refactor prefix
;;; (rename/format/code-actions). Eglot auto-starts for any project file
;;; whose language server is on the buffer's devenv-applied PATH, so
;;; enabling `languages.X.enable` in a project's devenv lights up its LSP
;;; with no per-language config.
(use-package eglot
  :ensure nil
  :preface
  ;; eglot is not loaded at compile time.
  (declare-function eglot-ensure "eglot")
  (declare-function eglot--guess-contact "eglot")
  (declare-function eglot-alternatives "eglot")
  (defvar eglot--managed-mode)

  (defun jotain-prog--eglot-guess-program ()
    "Executable eglot would use for this buffer, or nil.
Uses `eglot--guess-contact', which evaluates function-valued
`eglot-server-programs' entries in the project's env.  Returns nil for
contacts that are not a program (TCP, class forms).  Assumes eglot is
loaded."
    (ignore-errors
      (let ((contact (nth 3 (eglot--guess-contact))))
        (cond ((stringp contact) contact)
              ((and (consp contact) (stringp (car contact))) (car contact))))))

  (declare-function devenv-env-loading-p "devenv")
  (defun jotain-prog--maybe-eglot-ensure ()
    "Auto-start eglot when the project's env provides a server for this buffer.
Runs from `prog-mode-hook' but defers to an idle timer: `devenv-env-mode'
turns on later, from `after-change-major-mode-hook', and connecting
earlier would use the global environment instead of the project's.
Skips remote, non-file and already-managed buffers, and Lisp buffers
(no server, and eglot must not load for every Elisp buffer)."
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
                 (cond
                  ;; Env still loading, so `executable-find' cannot see
                  ;; devenv-only servers yet.  devenv's advice holds this
                  ;; `eglot-ensure' and replays it once the env lands.
                  ((and (fboundp 'devenv-env-loading-p)
                        (devenv-env-loading-p))
                   (eglot-ensure))
                  ((when-let* ((prog (jotain-prog--eglot-guess-program)))
                     (executable-find prog)) ; uses buffer-local exec-path
                   (eglot-ensure)))))))))))

  (defun jotain-prog--risky-js-extras ()
    "Optional ESLint/Tailwind `rass' companions, gated on the risky-JS opt-in.
Added only when `jotain-prog-enable-risky-js-lsp' is non-nil and the
servers are on PATH."
    (let (extras)
      (when jotain-prog-enable-risky-js-lsp
        (dolist (s '("eslint-lsp" "tailwindcss-language-server"))
          (when (executable-find s)
            (setq extras (append extras (list "--" s "--stdio"))))))
      extras))

  (defun jotain-prog--ts-server (&optional _interactive)
    "Resolve the TS/TSX server contact against the buffer's (project) PATH.
When `rass' and typescript-language-server are both on PATH, `rass'
wraps it (plus any risky-JS companions); else plain
typescript-language-server."
    (if (and (executable-find "rass")
             (executable-find "typescript-language-server"))
        (append '("rass" "--" "typescript-language-server" "--stdio")
                (jotain-prog--risky-js-extras))
      '("typescript-language-server" "--stdio")))

  (defun jotain-prog--python-server (&optional _interactive)
    "Resolve the Python server contact against the buffer's (project) PATH.
Prefers the `rass python' preset (basedpyright + ruff), then a lone
basedpyright/pyright, then pylsp."
    (cond ((and (executable-find "rass")
                (executable-find "basedpyright")
                (executable-find "ruff"))
           '("rass" "python"))
          ((executable-find "basedpyright") '("basedpyright-langserver" "--stdio"))
          ((executable-find "pyright-langserver") '("pyright-langserver" "--stdio"))
          (t '("pylsp"))))

  (defun jotain-prog--likec4-server (&optional _interactive)
    "Resolve the LikeC4 server contact against the buffer's PATH.
Prefers the standalone `likec4-lsp' (on the distribution wrapper PATH),
falling back to the `likec4' CLI's `lsp' subcommand."
    (if (executable-find "likec4-lsp")
        '("likec4-lsp" "--stdio")
      '("likec4" "lsp" "--stdio")))

  (defun jotain-prog--robot-server (&optional _interactive)
    "Resolve the Robot Framework server contact against the buffer's PATH.
Prefers `robotcode language-server', falling back to `robotframework_ls'.
Both speak stdio by default."
    (if (executable-find "robotcode")
        '("robotcode" "language-server")
      '("robotframework_ls")))
  :init
  ;; One devenv-aware auto-start for every language, instead of per-mode
  ;; `eglot-ensure' hooks, which would connect before the devenv env exists.
  (add-hook 'prog-mode-hook #'jotain-prog--maybe-eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-confirm-server-edits nil)
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
  ;; Show all eldoc sources together rather than Eglot's default strategy.
  (defun jotain-prog--eldoc-compose-eagerly ()
    "Compose eldoc sources eagerly in Eglot-managed buffers."
    (setq-local eldoc-documentation-strategy
                #'eldoc-documentation-compose-eagerly))
  (add-hook 'eglot-managed-mode-hook #'jotain-prog--eldoc-compose-eagerly)

  ;; Emacs 31+: render hover/signature docs with the tree-sitter markdown
  ;; viewer.  markdown-ts-mode.el has no autoloads, so require it.
  (when (and (boundp 'eglot-documentation-renderer)
             (require 'markdown-ts-mode nil t)
             (fboundp 'markdown-ts-view-mode))
    (setopt eglot-documentation-renderer 'markdown-ts-view-mode))

  ;; Emacs 31+: hide the inline "code action available" indicators.
  (when (boundp 'eglot-code-action-indications)
    (setopt eglot-code-action-indications nil))

  (defun jotain-prog--maybe-enable-inlay-hints ()
    "Enable inlay hints in the major modes that opt in to them."
    (when (apply #'derived-mode-p
                 '(go-ts-mode
                   rust-mode rust-ts-mode
                   typescript-mode typescript-ts-mode
                   python-mode python-ts-mode
                   neocaml-mode neocaml-interface-mode
                   zig-ts-mode
                   c-mode c++-mode c-ts-mode c++-ts-mode cuda-ts-mode
                   nix-ts-mode
                   haskell-ts-mode))
      (eglot-inlay-hints-mode 1)))
  (add-hook 'eglot-managed-mode-hook #'jotain-prog--maybe-enable-inlay-hints)

  (add-to-list 'eglot-server-programs
               '((go-ts-mode go-mod-ts-mode go-work-ts-mode) . ("gopls")))
  ;; Classic modes stay in the keys below so a build without the grammar
  ;; still finds a server.
  (add-to-list 'eglot-server-programs
               '((dockerfile-ts-mode dockerfile-mode) . ("docker-langserver" "--stdio")))
  ;; QML (init-lang-qml).  `-E' makes qmlls honour QML_IMPORT_PATH from
  ;; the project env.  qmlls is appended to the wrapper PATH
  ;; (nix/runtime-deps.nix), so a project-provided qmlls wins.
  (add-to-list 'eglot-server-programs
               '((qml-ts-mode) . ("qmlls" "-E")))

  ;; HTML (init-lang-web).  `mhtml-ts-mode' does not derive from the
  ;; classic modes eglot's default HTML entry is keyed on, so mirror that
  ;; entry for the tree-sitter modes.
  (add-to-list 'eglot-server-programs
               (cons '(mhtml-ts-mode html-ts-mode)
                     (eglot-alternatives
                      '(("vscode-html-language-server" "--stdio")
                        ("html-languageserver" "--stdio")))))

  ;; C/C++/CUDA (init-lang-systems).  clangd treats `.cu' as CUDA by
  ;; extension; `.cuh' and a full index need a project
  ;; `compile_commands.json' (and, on Nix, `--cuda-path').
  (add-to-list 'eglot-server-programs
               '((cuda-ts-mode c-ts-mode c++-ts-mode c-mode c++-mode) . ("clangd")))

  ;; OCaml via neocaml (init-lang-systems).  neocaml also registers this
  ;; itself; kept here so all LSP wiring is in one place.
  (add-to-list 'eglot-server-programs
               '((neocaml-mode neocaml-interface-mode) . ("ocamllsp")))

  ;; Function-valued contacts resolve at connect time against the buffer's
  ;; project env, not once at startup.  `rass' (rassumfrassum) multiplexes
  ;; several servers behind one stdio connection; each resolver falls back
  ;; to a single server when it or a companion is absent.
  (add-to-list 'eglot-server-programs
               (cons '(tsx-ts-mode typescript-ts-mode typescript-mode)
                     #'jotain-prog--ts-server))
  (add-to-list 'eglot-server-programs
               (cons '(python-mode python-ts-mode)
                     #'jotain-prog--python-server))
  ;; LikeC4 architecture models (init-lang-devops).
  (add-to-list 'eglot-server-programs
               (cons '(likec4-mode) #'jotain-prog--likec4-server))
  ;; Robot Framework (init-lang-devops).
  (add-to-list 'eglot-server-programs
               (cons '(robot-mode) #'jotain-prog--robot-server)))

;;; @doc Wrap local stdio language servers in emacs-lsp-booster, which
;;; converts server JSON into Elisp bytecode and buffers I/O so a busy
;;; server cannot block the UI. Function-valued contacts (the `rass`
;;; resolvers) are boosted too. The binary rides the distribution wrapper
;;; PATH (nix/runtime-deps.nix) because the mode resolves it once at
;;; enable time. Remote (TRAMP) contacts are left unboosted, since the
;;; booster would have to exist on the remote host. Provided by Nix (not
;;; on MELPA); `:if' skips the block when the library is absent.
(use-package eglot-booster
  :ensure nil
  :if (locate-library "eglot-booster")
  :after eglot
  :custom (eglot-booster-no-remote-boost t)
  :config (eglot-booster-mode))

;;; @doc Workspace symbol search through consult: C-M-. lists symbols
;;; across the LSP workspace.
(use-package consult-eglot
  ;; Only needs `eglot-mode-map'; `consult-eglot-symbols' is autoloaded.
  :after eglot
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

;;; @doc Embark actions on consult-eglot workspace symbols.
(use-package consult-eglot-embark
  :after (consult-eglot embark)
  :demand t)

;;;; Debugging — dape (Debug Adapter Protocol)

;;; @doc Debug Adapter Protocol client, the debugging counterpart to
;;; eglot. Ships adapter configs for dlv (Go), debugpy (Python),
;;; codelldb (Rust/C/C++) and more; adapter binaries come from the
;;; project/host PATH. `C-x C-a` is the prefix (the gud convention);
;;; stepping commands repeat, so `C-x C-a n n n` keeps stepping.
(use-package dape
  :bind-keymap ("C-x C-a" . dape-global-map)
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-default-breakpoints-file (jotain-var-file "dape-breakpoints"))
  :config
  ;; In :config, so a session that never loads dape never touches the file.
  (dape-breakpoint-load)
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)

  ;; Cargo launch config: dape's `lldb-dap' default (`:program "a.out"')
  ;; does not fit a cargo layout.  The `:program' prompt checks
  ;; `enable-recursive-minibuffers' because `dape--minibuffer-hint'
  ;; evaluates properties with it nil, where `read-file-name' would error.
  ;; The lambda is comma-unquoted so it compiles to a closure.
  (add-to-list 'dape-configs
               `(cargo-lldb
                 modes (rust-ts-mode rust-mode)
                 ensure dape-ensure-command
                 command "lldb-dap"
                 command-cwd dape-command-cwd
                 compile "cargo build"
                 :type "lldb-dap"
                 :request "launch"
                 :cwd "."
                 :program ,(lambda ()
                             (if enable-recursive-minibuffers
                                 (read-file-name "Binary: " (dape-cwd) nil t
                                                 "target/debug/")
                               "target/debug/")))))

;;;; SonarLint (SonarCloud connected mode)

;; SonarCloud connected mode is opt-in per project via a .dir-locals.el
;; `eglot-workspace-configuration' entry (:sonarlint :connectedMode with
;; the :sonarcloud org key and token, connectionId/projectKey).

(defun jotain-sonarlint ()
  "Start SonarLint analysis in the current project.
Launches sonarlint-ls as a secondary eglot connection alongside any
existing language server, adding code-quality and security diagnostics."
  (interactive)
  (require 'eglot)
  (let ((eglot-server-programs
         (cons `(,major-mode . ("sonarlint-ls" "-stdio"))
               eglot-server-programs)))
    (call-interactively #'eglot)))

;;;; Flymake / eldoc

;;; @doc Built-in diagnostics, on in every programming buffer. Indicator
;;; chars (! ? ·) and end-of-line messages keep them visible
;;; without a side window; `short' shows only the most severe diagnostic
;;; per line. M-n / M-p navigate.
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  ;; `short' needs Flymake 1.3.6+.
  (flymake-show-diagnostics-at-end-of-line 'short)
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

;;; @doc Built-in echo-area documentation: single-line display and a
;;; short idle delay.
(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-print-after-edit t)
  (eldoc-idle-delay 0.2)
  (eldoc-echo-area-display-truncation-message nil)
  ;; Use the doc buffer instead of the echo area when it is visible.
  (eldoc-echo-area-prefer-doc-buffer t)
  :config
  ;; Emacs 31+: also show `help-at-pt' text (flymake diagnostics, button
  ;; help) through eldoc.
  (when (boundp 'eldoc-help-at-pt)
    (setopt eldoc-help-at-pt t)))

;;;; xref

;;; @doc Built-in cross-reference engine. Searches with ripgrep (on the
;;; wrapper PATH) instead of grep.
(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep)
  :config
  ;; Emacs 31+: mouse-1 on an identifier jumps to its definition.
  (when (fboundp 'global-xref-mouse-mode)
    (global-xref-mouse-mode 1))
  ;; Regenerates a project TAGS table on demand, a definition source for
  ;; buffers without an LSP server.
  (when (fboundp 'etags-regen-mode)
    (etags-regen-mode 1)))

;;;; imenu

;;; @doc Built-in symbol index (behind `consult-imenu', M-g i), rescanned
;;; automatically so it never goes stale.
(use-package imenu
  :ensure nil
  :custom
  (imenu-auto-rescan t))

;;;; tagref

;;; @doc Cross-reference checker for `[tag:x]'/`[ref:x]' directives. Adds
;;; completion, xref navigation (M-. jumps from a ref to its tag, M-? finds
;;; references), and `M-x tagref-check' (clickable compilation buffer).
;;; Needs the `tagref' CLI on PATH (dev shell / Home Manager wrapper).
;;; Provided by Nix (not on MELPA).
(use-package tagref
  :ensure nil
  :commands (tagref-mode)
  :hook (prog-mode . jotain-tagref--maybe-enable)
  :init
  (defun jotain-tagref--maybe-enable ()
    "Enable `tagref-mode' only inside a project, when tagref is installed.
Outside a project `tagref-mode' signals a `user-error', which in the
daemon's *scratch* buffer aborts startup before `server-start'.  The
library is Nix-only; without it the `:commands' stub would error in
every prog-mode buffer."
    (when (and (project-current)
               (require 'tagref nil t))
      (tagref-mode 1))))

;;;; Compile

;;; @doc Built-in compile / recompile. Scrolls output until the first
;;; error, skips the save prompt, and kills a running compilation
;;; without asking.
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
  :hook (prog-mode . editorconfig-mode))

;;;; Per-project environment + format-on-save + grep refactor

;; The per-project environment comes from devenv (init-devenv.el), applied
;; buffer-locally; envrc is deliberately not enabled.

;;; @doc Async format-on-save through external formatters (ruff, nixfmt,
;;; rustfmt, prettier, …), all mapped in one place. `apheleia-mode` is a
;;; safe local variable, so `.dir-locals.el` can opt a project out.
(use-package apheleia
  :hook (after-init . apheleia-global-mode)
  :config
  (add-to-list 'apheleia-formatters
               '(meson-format . ("meson" "format"
                                  "--source-file-path" filepath
                                  "-")))
  (add-to-list 'apheleia-mode-alist '(meson-mode . meson-format))
  (add-to-list 'apheleia-formatters
               '(zig-fmt . ("zig" "fmt" "--stdin")))
  (add-to-list 'apheleia-mode-alist '(zig-ts-mode . zig-fmt))
  ;; goimports instead of apheleia's default gofmt for Go.
  (add-to-list 'apheleia-mode-alist '(go-ts-mode . goimports))
  ;; `-path' lets buildifier infer the Starlark dialect.  The `bazel-mode'
  ;; parent covers every Starlark-family mode but not the conf-derived
  ;; bazelrc/bazelignore modes.
  (add-to-list 'apheleia-formatters
               '(buildifier . ("buildifier" "-path" (or filepath "BUILD"))))
  (add-to-list 'apheleia-mode-alist '(bazel-mode . buildifier))
  ;; Newer nixfmt deprecates bare stdin invocation; pass "-" explicitly.
  (add-to-list 'apheleia-formatters '(nixfmt . ("nixfmt" "-")))
  ;; qmlformat edits in place (`-i'), so give it apheleia's `inplace'
  ;; temp copy.
  (add-to-list 'apheleia-formatters '(qmlformat . ("qmlformat" "-i" inplace)))
  (add-to-list 'apheleia-mode-alist '(qml-ts-mode . qmlformat))
  ;; apheleia keys clang-format on the classic cc-mode modes only.
  ;; `cuda-ts-mode' counts as `c++-ts-mode' (init-lang-systems).
  (add-to-list 'apheleia-mode-alist '(c-ts-mode . clang-format))
  (add-to-list 'apheleia-mode-alist '(c++-ts-mode . clang-format))
  ;; apheleia keys ocamlformat on tuareg/caml modes only.
  (add-to-list 'apheleia-mode-alist '(neocaml-mode . ocamlformat))
  (add-to-list 'apheleia-mode-alist '(neocaml-interface-mode . ocamlformat))
  ;; robotidy also edits in place.
  (add-to-list 'apheleia-formatters '(robotidy . ("robotidy" inplace)))
  (add-to-list 'apheleia-mode-alist '(robot-mode . robotidy))
  (put 'apheleia-mode 'safe-local-variable #'booleanp))

;;; @doc Edit grep result buffers in place and write the changes back to
;;; every matched file: consult-ripgrep, C-c C-o (embark-export),
;;; C-x C-q, edit, C-c C-c.
(use-package wgrep
  :defer t
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;;; @doc Detect indentation width and tabs/spaces from file contents.
(use-package dtrt-indent
  ;; 0 silences the per-file "adjusted" message; detection is unaffected.
  :custom (dtrt-indent-verbosity 0)
  :hook (prog-mode . dtrt-indent-mode))

(provide 'init-prog)
;;; init-prog.el ends here
