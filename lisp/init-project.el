;;; init-project.el --- Per-project commands and compile pickers -*- lexical-binding: t; -*-

;;; Commentary:

;; Two complementary systems for "what command should I run for this
;; project right now?":
;;
;;   - `projection' stores commands as safe-local variables in
;;     .dir-locals.el and exposes them via `C-x P'. It auto-discovers
;;     project types from Makefiles, justfiles, Cargo.toml, etc.
;;
;;   - `compile-multi' is a per-major-mode picker for named compile
;;     commands ("go test", "pytest file", "nix flake check"). Each
;;     mode has its own list configured in this file.
;;
;; They overlap but neither covers the other completely. Use whichever
;; reaches your hand first.

;;; Code:

;;;; project.el (built-in)

(defcustom jotain-repositories-roots
  (list "~/Developer" "~/Projects")
  "Roots whose immediate subdirectories are treated as repositories.
Feeds `jotain-find-projects-and-switch' (C-x p P) here and
`magit-repository-directories' (init-vc.el). Roots that do not
exist on this machine are silently skipped, so the defaults are
harmless wherever the config is deployed."
  :type '(repeat directory)
  :group 'project)

(declare-function project-remember-project "project" (pr &optional no-write))

(defun jotain-find-projects-and-switch ()
  "Scan `jotain-repositories-roots', pick a project, remember and open it.
Completion labels include the full abbreviated parent root, so two
projects sharing a basename across different roots stay distinct."
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

;;; @doc Built-in project tracker. Extra root markers below mean a
;;; project is recognised when any of these is present, not just
;;; on a VCS root. Project list lives under var/. `C-x p P' scans
;;; `jotain-repositories-roots' to add+open projects not yet in
;;; the known list.
(use-package project
  :ensure nil
  :bind (:map project-prefix-map ("P" . jotain-find-projects-and-switch))
  :custom
  (project-list-file (jotain-var-file "projects.el"))
  (project-buffers-viewer 'project-list-buffers-ibuffer)
  (project-vc-extra-root-markers
   '(".project" "package.json" "Cargo.toml" "pyproject.toml" "flake.nix"
     "devenv.nix")))

;;;; projection — per-project commands keyed off .dir-locals.el

;;; @doc `.dir-locals.el`-driven per-project commands (configure, build,
;;; test, run, package, install) auto-detected from
;;; Makefile/justfile/Cargo.toml/etc and exposed under C-x P.
(use-package projection
  :hook (after-init . global-projection-hook-mode)
  :bind-keymap ("C-x P" . projection-map)
  :config
  ;; Mark all the projection-commands-* variables as safe local
  ;; variables so .dir-locals.el can set them without prompting.
  (dolist (sym '(projection-commands-configure-project
                 projection-commands-build-project
                 projection-commands-test-project
                 projection-commands-run-project
                 projection-commands-package-project
                 projection-commands-install-project))
    (put sym 'safe-local-variable #'stringp)))

;;; @doc Bridges projection with compile-multi: project-prefix RET picks
;;; from every named compile command available in this project.
(use-package projection-multi
  :after projection
  :bind (:map project-prefix-map
              ("RET" . projection-multi-compile)))

;;; @doc Embark menu for projection-multi entries — pin a command,
;;; preview output, etc.
(use-package projection-multi-embark
  :after (embark projection-multi)
  :functions (projection-multi-embark-setup-command-map)
  :demand t
  :config (projection-multi-embark-setup-command-map))

;;;; compile-multi — named compile commands per major mode

;;; @doc Per-major-mode picker for named compile commands ("go test",
;;; "pytest file", "nix flake check", …). Complement to projection,
;;; kept side by side because neither system fully covers the other.
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
     ;; `python-base-mode' is the shared parent of both `python-mode' and
     ;; `python-ts-mode', so this fires regardless of which one a .py buffer
     ;; lands in (the repo routes to python-ts-mode, but a missing grammar
     ;; falls back to python-mode).  "pytest file" is a function action so
     ;; the current file is substituted at run time — compile-multi does not
     ;; template `%…%' placeholders.
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
     ;; TypeScript / JavaScript — a list trigger is `eval'd by compile-multi,
     ;; so this one entry covers all three ts-modes.  Neutral `npm' scripts;
     ;; concrete commands are overridden per project via projection /
     ;; .dir-locals.el.
     ((apply #'derived-mode-p '(typescript-ts-mode tsx-ts-mode js-ts-mode))
      . (("npm test"  . "npm test")
         ("npm build" . "npm run build")
         ("npm lint"  . "npm run lint")))
     ;; C / C++ — CMake / CTest, mirroring the Meson entries above.
     ((apply #'derived-mode-p '(c-mode c++-mode c-ts-mode c++-ts-mode))
      . (("cmake configure" . "cmake -B build")
         ("cmake build"     . "cmake --build build")
         ("ctest"           . "ctest --test-dir build --output-on-failure"))))))

;;; @doc Renders compile-multi pickers through consult — gives you
;;; orderless filtering and preview on every "what command should I
;;; run?" prompt.
(use-package consult-compile-multi
  :after compile-multi
  :functions (consult-compile-multi-mode)
  :demand t
  :config (consult-compile-multi-mode))

;;; @doc Decorates compile-multi entries with nerd-font glyphs for the
;;; command type — purely visual, but a useful at-a-glance hint.
(use-package compile-multi-nerd-icons
  :after (compile-multi nerd-icons-completion)
  :demand t)

;;; @doc Embark actions on compile-multi entries (run, copy, edit
;;; command line). Mirrors the projection embark integration.
(use-package compile-multi-embark
  :after (embark compile-multi)
  :functions (compile-multi-embark-mode)
  :demand t
  :config (compile-multi-embark-mode 1))

(provide 'init-project)
;;; init-project.el ends here
