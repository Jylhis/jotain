;;; init-lang-devops.el --- Infrastructure-as-code language modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Modes for CI, containers, and infrastructure: Dockerfile, Terraform,
;; GitLab CI, Justfile, Ansible, Bazel, and Robot Framework.
;;
;; Also two hand-rolled modes for the C4-model DSLs, Structurizr (`.dsl')
;; and LikeC4 (`.c4'/`.likec4'), sharing one brace-depth indenter.

;;; Code:

(defgroup jotain-devops nil
  "Docker/Podman and infrastructure-as-code settings."
  :group 'convenience)

;; Defined by the defcustom below, whose :set calls this function.
(defvar jotain-docker-backend)

(defun jotain--apply-docker-backend ()
  "Set `dockerfile-mode-command' from `jotain-docker-backend'.
A no-op until dockerfile-mode has loaded, so safe to call any time."
  (let ((cmd (if (eq jotain-docker-backend 'podman) "podman" "docker")))
    (when (boundp 'dockerfile-mode-command)
      (setopt dockerfile-mode-command cmd))))

(defcustom jotain-docker-backend 'podman
  "Container runtime that Docker-aware packages should drive.
`podman' (default) or `docker'.  Setting it updates
`dockerfile-mode-command'."
  :type '(choice (const :tag "Podman" podman)
                 (const :tag "Docker" docker))
  :group 'jotain-devops
  :set (lambda (sym val)
         (set-default-toplevel-value sym val)
         (jotain--apply-docker-backend)))

(use-package dockerfile-mode
  :defer t
  :config (jotain--apply-docker-backend))

(use-package terraform-mode
  :defer t
  :mode "\\.tf\\'")

(use-package gitlab-ci-mode
  :defer t)

(use-package just-ts-mode
  :mode (("/\\.?[Jj]ustfile\\'" . just-ts-mode)
         ("\\.just\\'" . just-ts-mode)))

(use-package feature-mode
  :mode "\\.feature\\'")

(use-package ansible
  :defer t)

(use-package bazel
  :defer t)

(use-package robot-mode
  :mode (("\\.robot\\'" . robot-mode)
         ("\\.resource\\'" . robot-mode)))

;;;; Brace-block DSLs

(defun jotain-devops--brace-indent-line (offset)
  "Indent the current line by OFFSET columns per `{'/`}' nesting level.
A line that leads with `}' closes the block above, so it belongs one
level out."
  (let ((depth (save-excursion
                 (back-to-indentation)
                 (let ((open (car (syntax-ppss))))
                   (if (looking-at-p "}") (1- open) open)))))
    (indent-line-to (* offset (max depth 0)))))

(defun jotain-devops--setup-brace-mode (comment-skip indent-function keywords)
  "Set up the current buffer as a brace-block DSL.
COMMENT-SKIP is `comment-start-skip', INDENT-FUNCTION the
`indent-line-function' and KEYWORDS the symbol holding the font-lock
rules."
  (setq-local comment-start "// "
              comment-end ""
              comment-start-skip comment-skip
              indent-line-function indent-function
              font-lock-defaults (list keywords nil t))
  ;; Dedent the line when the block-closing brace is typed.
  (setq-local electric-indent-chars (cons ?} electric-indent-chars)))

;;;; Structurizr DSL

;; `.dsl' otherwise opens in the built-in `dsssl-mode', whose Lisp
;; indentation mangles Structurizr's `{'/`}' blocks.

(defcustom jotain-structurizr-indent-offset 4
  "Columns of indentation per `{'/`}' nesting level in Structurizr DSL."
  :type 'natnum
  :group 'jotain-devops)

(defvar jotain-structurizr-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\" "\"" table)
    ;; `#' is a line comment; `//' and `/* */' are the C-style forms.
    (modify-syntax-entry ?#  "< b"    table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
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
  (jotain-devops--brace-indent-line jotain-structurizr-indent-offset))

(define-derived-mode jotain-structurizr-mode prog-mode "Structurizr"
  "Major mode for editing Structurizr DSL architecture models."
  :syntax-table jotain-structurizr-mode-syntax-table
  (jotain-devops--setup-brace-mode
   "\\(?://+\\|#+\\|/\\*+\\)[ \t]*"
   #'jotain-structurizr-indent-line
   'jotain-structurizr-font-lock-keywords))

;; Win over the built-in `.dsl' -> `dsssl-mode' entry by prepending.
(add-to-list 'auto-mode-alist '("\\.dsl\\'" . jotain-structurizr-mode))

;;;; LikeC4

;; https://likec4.dev: brace-depth indentation with only the `//' and
;; `/* */' comment forms.

(defcustom jotain-likec4-indent-offset 2
  "Columns of indentation per `{'/`}' nesting level in LikeC4 DSL."
  :type 'natnum
  :group 'jotain-devops)

(defvar jotain-likec4-mode-syntax-table
  (let ((table (make-syntax-table jotain-structurizr-mode-syntax-table)))
    (modify-syntax-entry ?# "." table)
    table)
  "Syntax table for `jotain-likec4-mode'.")

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
  "Font-lock rules for `jotain-likec4-mode'.")

(defun jotain-likec4-indent-line ()
  "Indent the current LikeC4 line by its `{'/`}' nesting depth."
  (jotain-devops--brace-indent-line jotain-likec4-indent-offset))

(define-derived-mode jotain-likec4-mode prog-mode "LikeC4"
  "Major mode for editing LikeC4 architecture models."
  :syntax-table jotain-likec4-mode-syntax-table
  (jotain-devops--setup-brace-mode
   "\\(?://+\\|/\\*+\\)[ \t]*"
   #'jotain-likec4-indent-line
   'jotain-likec4-font-lock-keywords))

(add-to-list 'auto-mode-alist '("\\.c4\\'" . jotain-likec4-mode))
(add-to-list 'auto-mode-alist '("\\.likec4\\'" . jotain-likec4-mode))

(provide 'init-lang-devops)
;;; init-lang-devops.el ends here
