;;; init-lang-devops.el --- Infrastructure-as-code language modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Major modes for files you edit when wiring up CI, containers, and
;; infrastructure: Dockerfile, docker-compose, terraform, gitlab-ci,
;; justfile, ansible. None of these are huge — they mostly exist to
;; pin a `:mode' regex and provide font-lock.
;;
;; Also here: two small hand-rolled major modes for the architecture-as-
;; code C4-model DSLs — Structurizr (`.dsl') and LikeC4 (`.c4'/`.likec4').
;; Neither is a package, so they need no MELPA entry and are invisible to
;; the use-package scanner in nix/use-package.nix. LikeC4 additionally
;; gets an eglot LSP (the `likec4-lsp' server) wired in init-prog.el.

;;; Code:

(defgroup jotain-devops nil
  "Docker/Podman and infrastructure-as-code settings."
  :group 'convenience)

;; Forward declaration so `jotain--apply-docker-backend' byte-compiles
;; cleanly under `byte-compile-error-on-warn'.  The defcustom below
;; supplies the real binding; this only silences the compiler.
(defvar jotain-docker-backend)

(defun jotain--apply-docker-backend ()
  "Apply `jotain-docker-backend' to dockerfile-mode.
Sets the runtime command used by `dockerfile-mode'.  The variable is
only touched once its owning package has loaded, so this is safe to
call before or after."
  (let ((cmd (if (eq jotain-docker-backend 'podman) "podman" "docker")))
    (when (boundp 'dockerfile-mode-command)
      (setopt dockerfile-mode-command cmd))))

(defcustom jotain-docker-backend 'podman
  "Container runtime that Docker-aware packages should drive.
`podman' (default) is rootless and daemonless; `docker' uses the
classic dockerd pair.  Changing this re-applies the runtime command
name used by `dockerfile-mode'."
  :type '(choice (const :tag "Podman" podman)
                 (const :tag "Docker" docker))
  :group 'jotain-devops
  :set (lambda (sym val)
         (set-default-toplevel-value sym val)
         (jotain--apply-docker-backend)))

;;; @doc Dockerfile major mode — syntax highlighting plus build
;;; commands (`M-x dockerfile-build-buffer`). The runtime command
;;; name comes from `jotain-docker-backend`.
(use-package dockerfile-mode
  :defer t
  :config (jotain--apply-docker-backend))

;;; @doc YAML-flavoured docker-compose syntax with awareness of compose
;;; keywords and service references.
(use-package docker-compose-mode
  :defer t)

;;; @doc HCL-aware Terraform mode for `.tf` files. Loaded by the mode
;;; regex; LSP comes from terraform-ls (configured in init-prog).
(use-package terraform-mode
  :defer t
  :mode "\\.tf\\'")

;;; @doc YAML mode tuned for `.gitlab-ci.yml` keywords — includes,
;;; rules, jobs. Saves a lot of typo'd job names on CI debugging
;;; days.
(use-package gitlab-ci-mode
  :defer t)

;;; @doc Tree-sitter major mode for `Justfile` — the project-aware command
;;; runner Jotain itself uses (the `just` grammar). Pairs with
;;; compile-multi for project commands. just-ts-mode also self-registers a
;;; Justfile auto-mode entry; the `:mode' regexes here are belt-and-suspenders
;;; and also map the `.just' extension used for included/modular recipes.
(use-package just-ts-mode
  :mode (("/[Jj]ustfile\\'" . just-ts-mode)
         ("\\.just\\'" . just-ts-mode)))

;;; @doc Ansible minor mode layered on top of yaml-mode for playbook
;;; files. Adds module-name completion and Jinja2 highlighting.
(use-package ansible
  :defer t)

;;; @doc Bazel/Starlark support — major modes for `BUILD`, `WORKSPACE`,
;;; `MODULE.bazel`, `REPO.bazel`, `*.bzl`, `.bazelrc`, `.bazelignore`
;;; and `.bazeliskrc` (auto-mode-alist comes from the package's own
;;; autoloads). `C-c C-f` runs buildifier; format-on-save for the
;;; Starlark-family buffers is wired through apheleia in init-prog.
(use-package bazel
  :defer t)

;;; Structurizr DSL ---------------------------------------------------
;;
;; `.dsl' files otherwise fall to the built-in `dsssl-mode', which is
;; derived from `scheme-mode' and indents with `lisp-indent-line'.  That
;; treats Structurizr's `{'/`}' blocks as Lisp forms, so electric
;; indentation mangles every line.  This mode indents purely by brace
;; nesting, knows the `//', `#', and `/* */' comment forms, and dedents
;; electrically when a closing brace is typed.

(defcustom jotain-structurizr-indent-offset 4
  "Columns of indentation per `{'/`}' nesting level in Structurizr DSL."
  :type 'natnum
  :group 'jotain-devops)

(defvar jotain-structurizr-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Braces delimit blocks — make them paren pairs so sexp motion,
    ;; `show-paren-mode', and depth-based indentation all work.
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\" "\"" table)
    ;; `#' is a line comment; `//' and `/* */' are the C-style forms.
    (modify-syntax-entry ?#  "<"      table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    ;; Identifiers and relationship arrows keep `-' and `_' together.
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?- "_" table)
    table)
  "Syntax table for `jotain-structurizr-mode'.")

(defvar jotain-structurizr-font-lock-keywords
  (let ((keywords '("workspace" "model" "views" "styles" "style" "branding"
                    "terminology" "configuration" "properties" "group"
                    "enterprise" "person" "softwareSystem" "container"
                    "component" "deploymentEnvironment" "deploymentNode"
                    "infrastructureNode" "softwareSystemInstance"
                    "containerInstance" "element" "relationship"
                    "systemLandscape" "systemContext" "filtered" "dynamic"
                    "deployment" "custom" "image" "include" "exclude"
                    "animation" "autoLayout" "autolayout" "extends" "this")))
    `(("!\\w+" . font-lock-preprocessor-face)
      (,(regexp-opt keywords 'symbols) . font-lock-keyword-face)
      ("->" . font-lock-function-name-face)
      (,(rx symbol-start (group (+ (any word "_"))) (* space) "=")
       (1 font-lock-variable-name-face))))
  "Font-lock rules for `jotain-structurizr-mode'.")

(defun jotain-structurizr-indent-line ()
  "Indent the current Structurizr line by its `{'/`}' nesting depth."
  (let ((depth (save-excursion
                 (back-to-indentation)
                 (let ((open (car (syntax-ppss))))
                   ;; A line that leads with `}' closes the block above,
                   ;; so it belongs one level out.
                   (if (looking-at-p "}") (1- open) open)))))
    (indent-line-to (* jotain-structurizr-indent-offset (max depth 0)))))

(define-derived-mode jotain-structurizr-mode prog-mode "Structurizr"
  "Major mode for editing Structurizr DSL architecture models."
  :syntax-table jotain-structurizr-mode-syntax-table
  (setq-local comment-start "// "
              comment-end ""
              comment-start-skip "\\(?://+\\|#+\\|/\\*+\\)[ \t]*"
              indent-line-function #'jotain-structurizr-indent-line
              font-lock-defaults '(jotain-structurizr-font-lock-keywords nil t))
  ;; Dedent the line when the block-closing brace is typed.
  (setq-local electric-indent-chars (cons ?} electric-indent-chars)))

;; Win over the built-in `.dsl' -> `dsssl-mode' entry by prepending.
(add-to-list 'auto-mode-alist '("\\.dsl\\'" . jotain-structurizr-mode))

;;; LikeC4 -----------------------------------------------------------
;;
;; LikeC4 (https://likec4.dev) is the other architecture-as-code C4-model
;; DSL, sibling to Structurizr above.  Files are `.c4'/`.likec4' and have
;; no built-in major mode.  Like Structurizr this is a hand-rolled mode
;; (not a package), so it needs no MELPA entry and is invisible to the
;; use-package scanner.  It indents purely by `{'/`}' nesting and knows the
;; C-style `//' and `/* */' comment forms.  LSP (via the bundled
;; `likec4-lsp' server) is wired in init-prog.el, keyed on `likec4-mode'.

(defcustom jotain-likec4-indent-offset 2
  "Columns of indentation per `{'/`}' nesting level in LikeC4 DSL."
  :type 'natnum
  :group 'jotain-devops)

(defvar jotain-likec4-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Braces delimit blocks — make them paren pairs so sexp motion,
    ;; `show-paren-mode', and depth-based indentation all work.
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\" "\"" table)
    ;; `//' and `/* */' are the only comment forms (no `#' unlike
    ;; Structurizr).
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    ;; Identifiers and relationship arrows keep `-' and `_' together.
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?- "_" table)
    table)
  "Syntax table for `likec4-mode'.")

(defvar jotain-likec4-font-lock-keywords
  (let ((keywords '("specification" "model" "views" "view" "element" "tag"
                    "relationship" "color" "technology" "style" "styles"
                    "extend" "extends" "link" "icon" "title" "description"
                    "of" "include" "exclude" "group" "with" "dynamic"
                    "navigateTo" "autoLayout" "autolayout" "this" "it"
                    "person" "system" "container" "component" "actor")))
    `((,(regexp-opt keywords 'symbols) . font-lock-keyword-face)
      ("->" . font-lock-function-name-face)
      (,(rx symbol-start (group (+ (any word "_"))) (* space) (any "=:"))
       (1 font-lock-variable-name-face))))
  "Font-lock rules for `likec4-mode'.")

(defun jotain-likec4-indent-line ()
  "Indent the current LikeC4 line by its `{'/`}' nesting depth."
  (let ((depth (save-excursion
                 (back-to-indentation)
                 (let ((open (car (syntax-ppss))))
                   ;; A line that leads with `}' closes the block above,
                   ;; so it belongs one level out.
                   (if (looking-at-p "}") (1- open) open)))))
    (indent-line-to (* jotain-likec4-indent-offset (max depth 0)))))

(define-derived-mode likec4-mode prog-mode "LikeC4"
  "Major mode for editing LikeC4 architecture models."
  :syntax-table jotain-likec4-mode-syntax-table
  (setq-local comment-start "// "
              comment-end ""
              comment-start-skip "\\(?://+\\|/\\*+\\)[ \t]*"
              indent-line-function #'jotain-likec4-indent-line
              font-lock-defaults '(jotain-likec4-font-lock-keywords nil t))
  ;; Dedent the line when the block-closing brace is typed.
  (setq-local electric-indent-chars (cons ?} electric-indent-chars)))

(add-to-list 'auto-mode-alist '("\\.c4\\'" . likec4-mode))
(add-to-list 'auto-mode-alist '("\\.likec4\\'" . likec4-mode))

(provide 'init-lang-devops)
;;; init-lang-devops.el ends here
