;;; init-prog.el --- Programming-mode glue: treesit, eglot, flymake, eldoc -*- lexical-binding: t; -*-

;;; Commentary:

;; The shared substrate every language module builds on.  Eglot lives
;; here because it is the glue between `prog-mode', `flymake', `eldoc' and
;; `xref'.  All LSP wiring (auto-start, server programs) and formatter
;; wiring stays in this file; language modules own mode registration and
;; language-specific settings.

;;; Code:

(defgroup jotain-prog nil
  "Programming-mode, tree-sitter and LSP settings."
  :group 'convenience)

;;;; prog-mode

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . display-fill-column-indicator-mode))

;;;; Tree-sitter

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

(use-package jit-lock
  :ensure nil
  :custom
  (jit-lock-defer-time 0.05))

;; Emacs 31 keeps the ts-mode remaps in `treesit-major-mode-remap-alist' and
;; applies the ones named here.  There is no grammar check: the
;; distribution ships every grammar.
(setopt treesit-enabled-modes
        '(bash-ts-mode cmake-ts-mode dockerfile-ts-mode json-ts-mode
          lua-ts-mode toml-ts-mode yaml-ts-mode python-ts-mode rust-ts-mode
          css-ts-mode js-ts-mode typescript-ts-mode c-ts-mode c++-ts-mode
          go-ts-mode go-mod-ts-mode go-work-ts-mode))

;; Classic modes that Emacs 31 does not remap itself, so a stray package
;; autoload still lands in the tree-sitter mode.
(dolist (remap '((js-mode  . js-ts-mode)
                 (js2-mode . js-ts-mode)
                 (json-mode . json-ts-mode)))
  (add-to-list 'major-mode-remap-alist remap))

;; Async native-comp workers skip site files, so they miss the
;; `treesit-extra-load-path' Nixpkgs' site-start.el sets.  Pass it on, or
;; compiling a `*-ts-mode' file logs a spurious "grammar unavailable"
;; warning.
(defvar treesit-extra-load-path)
(defvar native-comp-async-env-modifier-form)
(setq native-comp-async-env-modifier-form
      `(setq treesit-extra-load-path ',treesit-extra-load-path))

(use-package treesit-fold
  :hook (after-init . global-treesit-fold-indicators-mode)
  :custom (treesit-fold-indicators-priority -1))

(use-package combobulate
  :ensure nil
  :defer t
  :commands combobulate-mode
  :bind (:map combobulate-key-map
              ("M-P" . combobulate-drag-up)
              ("M-N" . combobulate-drag-down)))

;;;; Eglot

;; LSP servers send multi-megabyte responses; a bigger read buffer means
;; far fewer read(2) calls.
(setopt read-process-output-max (* 4 1024 1024))

(defcustom jotain-prog-enable-risky-js-lsp nil
  "When non-nil, include ESLint and Tailwind LSP servers in TS/TSX `rass` sessions.
These servers may evaluate project JavaScript configuration files."
  :type 'boolean
  :group 'jotain-prog)

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

  (defun jotain-prog--kotlin-server (&optional _interactive)
    "Resolve the Kotlin server contact against the buffer's (project) PATH.
Prefers JetBrains `kotlin-lsp', then the community
kotlin-language-server."
    (if (executable-find "kotlin-lsp")
        '("kotlin-lsp" "--stdio")
      '("kotlin-language-server")))

  (defun jotain-prog--likec4-server (&optional _interactive)
    "Resolve the LikeC4 server contact against the buffer's PATH.
Prefers the standalone `likec4-lsp' (on the distribution wrapper PATH),
falling back to the `likec4' CLI's `lsp' subcommand."
    (if (executable-find "likec4-lsp")
        '("likec4-lsp" "--stdio")
      '("likec4" "lsp" "--stdio")))

  (defun jotain-prog--astro-tsdk ()
    "TypeScript lib dir with the JS API for astro-ls, or nil.
Prefers the project's node_modules, then the package of the `tsc' on
PATH: an npm symlink (bin/tsc -> ../lib) or a Nix wrapper (bin/tsc
beside lib/node_modules/typescript).  TypeScript 7 ships no
lib/typescript.js, so each candidate is checked."
    (let ((tsc (executable-find "tsc")))
      (seq-find
       (lambda (dir) (file-exists-p (expand-file-name "typescript.js" dir)))
       (delq nil
             (list (when-let* ((pr (project-current)))
                     (expand-file-name "node_modules/typescript/lib"
                                       (project-root pr)))
                   (when tsc
                     (expand-file-name "../lib" (file-name-directory
                                                 (file-truename tsc))))
                   (when tsc
                     (expand-file-name "../lib/node_modules/typescript/lib"
                                       (file-name-directory tsc))))))))

  (defun jotain-prog--astro-server (&optional _interactive)
    "Resolve the astro-ls contact, passing a usable TypeScript SDK.
astro-ls needs `typescript.tsdk'; see `jotain-prog--astro-tsdk'."
    `("astro-ls" "--stdio"
      ,@(when-let* ((tsdk (jotain-prog--astro-tsdk)))
          `(:initializationOptions (:typescript (:tsdk ,tsdk))))))

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
  (defun jotain-prog--eldoc-compose-eagerly ()
    "Compose eldoc sources eagerly in Eglot-managed buffers."
    (setq-local eldoc-documentation-strategy
                #'eldoc-documentation-compose-eagerly))
  (add-hook 'eglot-managed-mode-hook #'jotain-prog--eldoc-compose-eagerly)

  ;; Render hover/signature docs with the tree-sitter markdown viewer.
  ;; markdown-ts-mode.el has no autoloads, so require it.
  (require 'markdown-ts-mode)
  (setopt eglot-documentation-renderer 'markdown-ts-view-mode)

  (setopt eglot-code-action-indications nil)

  (defun jotain-prog--maybe-enable-inlay-hints ()
    "Enable inlay hints in the major modes that opt in to them."
    (when (apply #'derived-mode-p
                 '(go-ts-mode
                   rust-mode rust-ts-mode
                   typescript-mode typescript-ts-mode
                   python-mode python-ts-mode
                   neocaml-mode neocaml-interface-mode
                   zig-ts-mode
                   kotlin-ts-mode
                   c-mode c++-mode c-ts-mode c++-ts-mode cuda-ts-mode
                   nix-ts-mode
                   haskell-ts-mode))
      (eglot-inlay-hints-mode 1)))
  (add-hook 'eglot-managed-mode-hook #'jotain-prog--maybe-enable-inlay-hints)

  ;; `-E' makes qmlls honour QML_IMPORT_PATH from
  ;; the project env.  qmlls is appended to the wrapper PATH
  ;; (nix/runtime-deps.nix), so a project-provided qmlls wins.
  (add-to-list 'eglot-server-programs
               '((qml-ts-mode) . ("qmlls" "-E")))

  ;; `mhtml-ts-mode' does not derive from the
  ;; classic modes eglot's default HTML entry is keyed on, so mirror that
  ;; entry for the tree-sitter modes.
  (add-to-list 'eglot-server-programs
               (cons '(mhtml-ts-mode html-ts-mode)
                     (eglot-alternatives
                      '(("vscode-html-language-server" "--stdio")
                        ("html-languageserver" "--stdio")))))

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
  (add-to-list 'eglot-server-programs
               (cons '(kotlin-ts-mode) #'jotain-prog--kotlin-server))
  (add-to-list 'eglot-server-programs
               (cons '(jotain-likec4-mode) #'jotain-prog--likec4-server))
  (add-to-list 'eglot-server-programs
               (cons '(robot-mode) #'jotain-prog--robot-server))
  (add-to-list 'eglot-server-programs
               (cons '(astro-ts-mode) #'jotain-prog--astro-server)))

;; The binary rides the distribution wrapper PATH
;; (nix/runtime-deps.nix) because the mode resolves it once at enable time.
(use-package eglot-booster
  :ensure nil
  :if (locate-library "eglot-booster")
  :after eglot
  :custom (eglot-booster-no-remote-boost t)
  :config (eglot-booster-mode))

(use-package consult-eglot
  ;; Only needs `eglot-mode-map'; `consult-eglot-symbols' is autoloaded.
  :after eglot
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

(use-package consult-eglot-embark
  :after (consult-eglot embark)
  :demand t)

;;;; Debugging (dape)

(defun jotain-prog--kotlin-main-class ()
  "Guess the JVM main class of the current Kotlin file.
A top-level `main' in `Foo.kt' of package `a.b' compiles to `a.b.FooKt'."
  (let ((class (concat (file-name-base (or buffer-file-name "Main")) "Kt")))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (if (re-search-forward "^package[ \t]+\\([[:alnum:]_.]+\\)" nil t)
            (concat (match-string-no-properties 1) "." class)
          class)))))

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
                               "target/debug/"))))

  ;; Kotlin/JVM launch config for fwcd's kotlin-debug-adapter, which dape
  ;; does not ship.  The adapter reads the compiled classes of the Gradle
  ;; build, so `compile' builds them first.
  (add-to-list 'dape-configs
               `(kotlin-debug-adapter
                 modes (kotlin-ts-mode)
                 ensure dape-ensure-command
                 command "kotlin-debug-adapter"
                 command-cwd dape-command-cwd
                 compile "./gradlew --console=plain classes"
                 :type "kotlin"
                 :request "launch"
                 :projectRoot dape-cwd
                 :mainClass ,(lambda ()
                               (let ((guess (jotain-prog--kotlin-main-class)))
                                 (if enable-recursive-minibuffers
                                     (read-string "Main class: " guess)
                                   guess))))))

;;;; SonarLint

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

(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-suppress-zero-counters t)
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

(use-package eldoc
  :ensure nil
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-print-after-edit t)
  (eldoc-idle-delay 0.2)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  :config
  (setopt eldoc-help-at-pt t))

;;;; xref

(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep))

;;;; tagref

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

(use-package compile
  :ensure nil
  :custom
  (compilation-ask-about-save nil)
  (compilation-always-kill t))

;;;; Per-project environment + format-on-save + grep refactor

;; The per-project environment comes from devenv (init-devenv.el), applied
;; buffer-locally; envrc is deliberately not enabled.

(use-package apheleia
  :hook (after-init . apheleia-global-mode)
  :config
  (add-to-list 'apheleia-formatters
               '(meson-format . ("meson" "format"
                                  "--source-file-path" filepath
                                  "-")))
  (add-to-list 'apheleia-mode-alist '(meson-mode . meson-format))
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
  ;; robotidy also edits in place.
  (add-to-list 'apheleia-formatters '(robotidy . ("robotidy" inplace)))
  (add-to-list 'apheleia-mode-alist '(robot-mode . robotidy))
  ;; ktfmt instead of apheleia's default ktlint for Kotlin.  The
  ;; kotlinlang style indents by 4, as `kotlin-ts-mode' does; ktfmt's
  ;; default style indents by 2.
  (add-to-list 'apheleia-formatters
               '(ktfmt . ("ktfmt" "--kotlinlang-style" "-")))
  (add-to-list 'apheleia-mode-alist '(kotlin-ts-mode . ktfmt))
  ;; ruff instead of apheleia's default black.
  (add-to-list 'apheleia-mode-alist '(python-mode . ruff))
  (add-to-list 'apheleia-mode-alist '(python-ts-mode . ruff))
  (put 'apheleia-mode 'safe-local-variable #'booleanp))

(use-package wgrep
  :defer t
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package dtrt-indent
  ;; 0 silences the per-file "adjusted" message; detection is unaffected.
  :custom (dtrt-indent-verbosity 0)
  :hook (prog-mode . dtrt-indent-mode))

(provide 'init-prog)
;;; init-prog.el ends here
