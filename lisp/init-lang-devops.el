;;; init-lang-devops.el --- Infrastructure-as-code language modes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Modes for CI, containers, and infrastructure: Dockerfile, Terraform,
;; GitLab CI, Justfile, Ansible, Bazel, and Robot Framework.
;;
;; Also two hand-rolled modes for the C4-model DSLs, Structurizr (`.dsl')
;; and LikeC4 (`.c4'/`.likec4').  They are not packages, so the
;; use-package scanner (nix/use-package.nix) never sees them.

;;; Code:

(defgroup my-devops nil
  "Docker/Podman and infrastructure-as-code settings."
  :group 'convenience)

;; Defined by the defcustom below, whose :set calls this function.
(defvar my-docker-backend)

(defun my--apply-docker-backend ()
  "Set `dockerfile-mode-command' from `my-docker-backend'.
A no-op until dockerfile-mode has loaded, so safe to call any time."
  (let ((cmd (if (eq my-docker-backend 'podman) "podman" "docker")))
    (when (boundp 'dockerfile-mode-command)
      (setopt dockerfile-mode-command cmd))))

(defcustom my-docker-backend 'podman
  "Container runtime that Docker-aware packages should drive.
`podman' (default) or `docker'.  Setting it updates
`dockerfile-mode-command'."
  :type '(choice (const :tag "Podman" podman)
                 (const :tag "Docker" docker))
  :group 'my-devops
  :set (lambda (sym val)
         (set-default-toplevel-value sym val)
         (my--apply-docker-backend)))

;;; @doc Dockerfile major mode with build commands (`M-x
;;; dockerfile-build-buffer`). The runtime command comes from
;;; `my-docker-backend`.
(use-package dockerfile-mode
  :defer t
  :config (my--apply-docker-backend))

;;; @doc Terraform mode for `.tf` files. eglot's built-in entry starts
;;; terraform-ls when it is on PATH.
(use-package terraform-mode
  :defer t
  :mode "\\.tf\\'")

;;; @doc `yaml-mode` derivative for `.gitlab-ci.yml` with GitLab CI
;;; keyword highlighting and completion.
(use-package gitlab-ci-mode
  :defer t)

;;; @doc Tree-sitter major mode for `Justfile` (the `just` grammar). The
;;; package registers Justfile itself; the `:mode` regexes add the
;;; dotfile `.justfile` and the `.just` extension used for modular
;;; recipes.
(use-package just-ts-mode
  :mode (("/\\.?[Jj]ustfile\\'" . just-ts-mode)
         ("\\.just\\'" . just-ts-mode)))

;;; @doc Gherkin major mode (MELPA) for Cucumber `.feature` files:
;;; keyword highlighting, indentation and step navigation.
(use-package feature-mode
  :mode "\\.feature\\'")

;;; @doc Ansible minor mode for playbooks: keyword and Jinja2
;;; highlighting plus `ansible-vault` helpers. Not enabled
;;; automatically; use `M-x ansible-mode`.
(use-package ansible
  :defer t)

;;; @doc Bazel/Starlark major modes for `BUILD`, `WORKSPACE`,
;;; `MODULE.bazel`, `REPO.bazel`, `*.bzl`, `.bazelrc`, `.bazelignore` and
;;; `.bazeliskrc` (registered by the package). `C-c C-f` runs
;;; buildifier; format-on-save goes through apheleia (init-prog).
(use-package bazel
  :defer t)

;;; @doc Major mode for Robot Framework suites and resource files
;;; (`.robot`/`.resource`). LSP (robotcode or robotframework_ls) and
;;; format-on-save (robotidy) are wired in init-prog, from the project
;;; PATH.
(use-package robot-mode
  :mode (("\\.robot\\'" . robot-mode)
         ("\\.resource\\'" . robot-mode)))

;;; Structurizr DSL ---------------------------------------------------
;;
;; `.dsl' otherwise opens in the built-in `dsssl-mode', whose Lisp
;; indentation mangles Structurizr's `{'/`}' blocks.  This mode indents by
;; brace depth and knows the `//', `#', and `/* */' comment forms.

(defcustom my-structurizr-indent-offset 4
  "Columns of indentation per `{'/`}' nesting level in Structurizr DSL."
  :type 'natnum
  :group 'my-devops)

(defvar my-structurizr-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Braces as paren pairs, for sexp motion, `show-paren-mode', and
    ;; depth-based indentation.
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\" "\"" table)
    ;; `#' is a line comment; `//' and `/* */' are the C-style forms.
    (modify-syntax-entry ?#  "< b"    table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    ;; Identifiers and relationship arrows keep `-' and `_' together.
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?- "_" table)
    table)
  "Syntax table for `my-structurizr-mode'.")

(defvar my-structurizr-font-lock-keywords
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
  "Font-lock rules for `my-structurizr-mode'.")

(defun my-structurizr-indent-line ()
  "Indent the current Structurizr line by its `{'/`}' nesting depth."
  (let ((depth (save-excursion
                 (back-to-indentation)
                 (let ((open (car (syntax-ppss))))
                   ;; A line that leads with `}' closes the block above,
                   ;; so it belongs one level out.
                   (if (looking-at-p "}") (1- open) open)))))
    (indent-line-to (* my-structurizr-indent-offset (max depth 0)))))

(define-derived-mode my-structurizr-mode prog-mode "Structurizr"
  "Major mode for editing Structurizr DSL architecture models."
  :syntax-table my-structurizr-mode-syntax-table
  (setq-local comment-start "// "
              comment-end ""
              comment-start-skip "\\(?://+\\|#+\\|/\\*+\\)[ \t]*"
              indent-line-function #'my-structurizr-indent-line
              font-lock-defaults '(my-structurizr-font-lock-keywords nil t))
  ;; Dedent the line when the block-closing brace is typed.
  (setq-local electric-indent-chars (cons ?} electric-indent-chars)))

;; Win over the built-in `.dsl' -> `dsssl-mode' entry by prepending.
(add-to-list 'auto-mode-alist '("\\.dsl\\'" . my-structurizr-mode))

;;; LikeC4 -----------------------------------------------------------
;;
;; LikeC4 (https://likec4.dev): same brace-depth approach as Structurizr,
;; with only the `//' and `/* */' comment forms.  The eglot server
;; (`likec4-lsp', bundled on the distribution PATH) is wired in init-prog.

(defcustom my-likec4-indent-offset 2
  "Columns of indentation per `{'/`}' nesting level in LikeC4 DSL."
  :type 'natnum
  :group 'my-devops)

(defvar my-likec4-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23"   table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?_ "_" table)
    (modify-syntax-entry ?- "_" table)
    table)
  "Syntax table for `my-likec4-mode'.")

(defvar my-likec4-font-lock-keywords
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
  "Font-lock rules for `my-likec4-mode'.")

(defun my-likec4-indent-line ()
  "Indent the current LikeC4 line by its `{'/`}' nesting depth."
  (let ((depth (save-excursion
                 (back-to-indentation)
                 (let ((open (car (syntax-ppss))))
                   (if (looking-at-p "}") (1- open) open)))))
    (indent-line-to (* my-likec4-indent-offset (max depth 0)))))

(define-derived-mode my-likec4-mode prog-mode "LikeC4"
  "Major mode for editing LikeC4 architecture models."
  :syntax-table my-likec4-mode-syntax-table
  (setq-local comment-start "// "
              comment-end ""
              comment-start-skip "\\(?://+\\|/\\*+\\)[ \t]*"
              indent-line-function #'my-likec4-indent-line
              font-lock-defaults '(my-likec4-font-lock-keywords nil t))
  (setq-local electric-indent-chars (cons ?} electric-indent-chars)))

(add-to-list 'auto-mode-alist '("\\.c4\\'" . my-likec4-mode))
(add-to-list 'auto-mode-alist '("\\.likec4\\'" . my-likec4-mode))

(provide 'init-lang-devops)
;;; init-lang-devops.el ends here
