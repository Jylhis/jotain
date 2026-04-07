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

(use-package project
  :ensure nil
  :custom
  (project-buffers-viewer 'project-list-buffers-ibuffer)
  (project-vc-extra-root-markers
   '(".project" "package.json" "Cargo.toml" "pyproject.toml" "flake.nix")))

;;;; projection — per-project commands keyed off .dir-locals.el

(use-package projection
  :hook (after-init . global-projection-hook-mode)
  :bind-keymap ("C-x P" . projection-map)
  :config
  (with-eval-after-load 'project
    (require 'projection))
  ;; Mark all the projection-commands-* variables as safe local
  ;; variables so .dir-locals.el can set them without prompting.
  (dolist (sym '(projection-commands-configure-project
                 projection-commands-build-project
                 projection-commands-test-project
                 projection-commands-run-project
                 projection-commands-package-project
                 projection-commands-install-project))
    (put sym 'safe-local-variable #'stringp)))

(use-package projection-multi
  :after projection
  :bind (:map project-prefix-map
              ("RET" . projection-multi-compile)))

(use-package projection-multi-embark
  :after (embark projection-multi)
  :demand t
  :config (projection-multi-embark-setup-command-map))

;;;; compile-multi — named compile commands per major mode

(use-package compile-multi
  :defer t
  :commands (compile-multi)
  :custom
  (compile-multi-config
   '((go-mode      . (("go test"        . "go test ./...")
                      ("go test current" . "go test .")
                      ("go build"       . "go build .")))
     (python-mode  . (("pytest"         . "pytest")
                      ("pytest file"    . "pytest %file-name%")))
     (haskell-mode . (("stack test"     . "stack test")
                      ("cabal test"     . "cabal test")))
     (nix-ts-mode  . (("nix flake check" . "nix flake check")
                      ("nix fmt"        . "nix fmt")))
     (rust-ts-mode . (("cargo test"     . "cargo test")
                      ("cargo clippy"   . "cargo clippy --all-targets")
                      ("cargo build"    . "cargo build"))))))

(use-package consult-compile-multi
  :after compile-multi
  :demand t
  :config (consult-compile-multi-mode))

(use-package compile-multi-nerd-icons
  :after (compile-multi nerd-icons-completion)
  :demand t)

(use-package compile-multi-embark
  :after (embark compile-multi)
  :demand t
  :config (compile-multi-embark-mode 1))

(provide 'init-project)
;;; init-project.el ends here
