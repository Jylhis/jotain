;;; init-project.el --- Per-project commands and compile pickers -*- lexical-binding: t; -*-

;;; Commentary:

;; project.el setup, plus two complementary answers to "which command do
;; I run here?":
;;
;;   - `projection' (`C-x P') auto-detects the project type (Makefile,
;;     justfile, Cargo.toml, ...); commands can be overridden as
;;     safe-local variables in .dir-locals.el.
;;
;;   - `compile-multi' is a per-major-mode picker of named compile
;;     commands, configured below.

;;; Code:

;;;; project.el (built-in)

(defcustom jotain-repositories-roots
  (list "~/Developer" "~/Projects")
  "Roots whose immediate subdirectories are treated as repositories.
Feeds `jotain-find-projects-and-switch' (C-x p P) and
`magit-repository-directories' (init-vc.el).  Missing roots are
skipped."
  :type '(repeat directory)
  :group 'project)

(declare-function project-remember-project "project" (pr &optional no-write))

(defun jotain-find-projects-and-switch ()
  "Scan `jotain-repositories-roots', pick a project, remember and open it.
Labels include the parent root, so same-named projects stay distinct."
  (interactive)
  (let* ((dirs (cl-loop for root in jotain-repositories-roots
                        when (file-directory-p root)
                        nconc (directory-files root t "\\`[^.]" t)))
         (choices (cl-loop for d in dirs
                           when (file-directory-p d)
                           for name = (file-name-nondirectory d)
                           for parent = (abbreviate-file-name
                                         (directory-file-name
                                          (file-name-directory d)))
                           collect (cons (format "%s (%s)" name parent) d))))
    (unless choices
      (user-error "No project candidates under %s" jotain-repositories-roots))
    (let* ((pick (completing-read "Project: " choices nil t))
           (dir  (cdr (assoc pick choices))))
      (when dir
        (when-let* ((proj (project-current nil dir)))
          (project-remember-project proj))
        (project-switch-project dir)))))

;;; @doc Built-in project tracker. The extra root markers make a
;;; directory with, e.g., `flake.nix` or `go.mod` a project root, even
;;; without VCS or inside a larger repository. The project list lives
;;; under var/. `C-x p P` scans `jotain-repositories-roots` to add and
;;; open projects not yet known.
(use-package project
  :ensure nil
  :bind (:map project-prefix-map ("P" . jotain-find-projects-and-switch))
  :custom
  (project-list-file (jotain-var-file "projects.el"))
  (project-buffers-viewer 'project-list-buffers-ibuffer)
  ;; go.mod scopes gopls, `project-find-file' and `consult-ripgrep' to the
  ;; module, not the git root.  The deepest marker wins, so a module's
  ;; go.mod shadows an ancestor go.work; to scope to a go.work workspace
  ;; instead, replace "go.mod" with "go.work".
  (project-vc-extra-root-markers
   '(".project" "package.json" "Cargo.toml" "pyproject.toml" "flake.nix"
     "devenv.nix" "go.mod")))

;;; @doc project.el backend for the Nix (and Guix) store: each store
;;; directory is a project root, so `project-find-file` works while
;;; reading a dependency's source. Store paths stay out of the saved
;;; project list.
(use-package project-nix-store
  :ensure nil
  :after project
  :init
  ;; Prepended, so it runs before `project-try-vc' as upstream recommends
  ;; for performance.
  (add-hook 'project-find-functions #'project-nix-store-try)
  :config
  ;; `project-list-exclude' is newer than Emacs 30.1.
  (when (boundp 'project-list-exclude)
    (add-to-list 'project-list-exclude #'project-nix-store-p)))

;;;; projection — per-project commands keyed off .dir-locals.el

;;; @doc Per-project commands (configure, build, test, run, package,
;;; install) under C-x P, auto-detected from Makefile/justfile/Cargo.toml
;;; and friends and overridable from `.dir-locals.el`.
(use-package projection
  :hook (after-init . global-projection-hook-mode)
  :bind-keymap ("C-x P" . projection-map)
  :config
  ;; Let .dir-locals.el set the command strings without prompting.
  (dolist (sym '(projection-commands-configure-project
                 projection-commands-build-project
                 projection-commands-test-project
                 projection-commands-run-project
                 projection-commands-package-project
                 projection-commands-install-project))
    (put sym 'safe-local-variable #'stringp)))

;;; @doc Bridges projection with compile-multi: `C-x p RET` picks from
;;; every named compile command available in this project.
(use-package projection-multi
  :after projection
  :bind (:map project-prefix-map
              ("RET" . projection-multi-compile)))

;;; @doc Embark actions on projection-multi entries.
(use-package projection-multi-embark
  :after (embark projection-multi)
  :functions (projection-multi-embark-setup-command-map)
  :demand t
  :config (projection-multi-embark-setup-command-map))

;;;; compile-multi — named compile commands per major mode

;;; @doc Per-major-mode picker for named compile commands ("go test",
;;; "pytest file", "nix flake check", …). Complements projection; neither
;;; fully covers the other.
(use-package compile-multi
  :defer t
  :commands (compile-multi)
  :custom
  (compile-multi-config
   `((go-ts-mode   . (("go test"         . "go test ./...")
                      ("go test current" . "go test .")
                      ("go test -race"   . "go test -race ./...")
                      ("go build"        . "go build ./...")
                      ("go run"          . "go run .")
                      ("go vet"          . "go vet ./...")
                      ("golangci-lint"   . "golangci-lint run")))
     (go-mod-ts-mode . (("go mod tidy"     . "go mod tidy")
                        ("go mod download" . "go mod download")))
     ;; `python-base-mode' covers both `python-mode' and `python-ts-mode'.
     ;; "pytest file" is a function because compile-multi has no file
     ;; placeholder.
     (python-base-mode . (("pytest"      . "pytest")
                          ("pytest file" . ,(lambda ()
                                              (concat "pytest "
                                                      (shell-quote-argument
                                                       (or (buffer-file-name) ".")))))))
     (haskell-mode . (("stack test"     . "stack test")
                      ("cabal test"     . "cabal test")))
     (tuareg-mode  . (("dune build"     . "dune build")
                      ("dune test"      . "dune test")
                      ("dune runtest"   . "dune runtest")
                      ("dune fmt"       . "dune build @fmt")))
     ;; The neocaml modes do not derive from tuareg-mode.
     (neocaml-base-mode . (("dune build"   . "dune build")
                           ("dune test"    . "dune test")
                           ("dune runtest" . "dune runtest")
                           ("dune fmt"     . "dune build @fmt")))
     (meson-mode   . (("meson setup"    . "meson setup builddir")
                      ("meson compile"  . "meson compile -C builddir")
                      ("meson test"     . "meson test -C builddir")))
     (nix-ts-mode  . (("nix flake check" . "nix flake check")
                      ("nix fmt"        . "nix fmt")
                      ("devenv test"    . "devenv -q test")
                      ("devenv build"   . "devenv -q build")
                      ("devenv up"      . "devenv -q up")))
     (rust-ts-mode . (("cargo test"     . "cargo test")
                      ("cargo clippy"   . "cargo clippy --all-targets")
                      ("cargo build"    . "cargo build")))
     (zig-ts-mode  . (("zig build"      . "zig build")
                      ("zig test"       . "zig build test")
                      ("zig run"        . "zig build run")))
     ;; A list trigger is `eval'd, so one entry covers all three modes.
     ;; Override per project via projection / .dir-locals.el.
     ((apply #'derived-mode-p '(typescript-ts-mode tsx-ts-mode js-ts-mode))
      . (("npm test"  . "npm test")
         ("npm build" . "npm run build")
         ("npm lint"  . "npm run lint")))
     ;; C / C++: CMake and CTest.
     ((apply #'derived-mode-p '(c-mode c++-mode c-ts-mode c++-ts-mode))
      . (("cmake configure" . "cmake -B build")
         ("cmake build"     . "cmake --build build")
         ("ctest"           . "ctest --test-dir build --output-on-failure"))))))

;;; @doc Renders compile-multi pickers through consult.
(use-package consult-compile-multi
  :after compile-multi
  :functions (consult-compile-multi-mode)
  :demand t
  :config (consult-compile-multi-mode))

;;; @doc Nerd-font icons for compile-multi entries by command type.
(use-package compile-multi-nerd-icons
  :after (compile-multi nerd-icons-completion)
  :demand t)

;;; @doc Embark actions on compile-multi entries.
(use-package compile-multi-embark
  :after (embark compile-multi)
  :functions (compile-multi-embark-mode)
  :demand t
  :config (compile-multi-embark-mode 1))

(provide 'init-project)
;;; init-project.el ends here
