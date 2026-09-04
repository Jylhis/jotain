;;; init-lang-rust.el --- Rust language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust mode (built-in tree-sitter variant), eglot wired through
;; rust-analyzer (the hook lives in `init-prog'), and a couple of
;; conveniences for the typical edit-test-format loop.

;;; Code:

;; rust-analyzer workspace settings.  Buffer-local in a mode hook rather
;; than a global `setq-default' so no other language's workspace
;; configuration is clobbered, and a project .dir-locals.el
;; `eglot-workspace-configuration' entry still overrides it cleanly.  Same
;; pattern as `jotain-go--eglot-workspace-config' in init-lang-go.el.
(defvar eglot-workspace-configuration) ; defined in eglot.el

(defun jotain-rust--eglot-workspace-config ()
  "Set buffer-local rust-analyzer workspace configuration for Rust buffers.
`check.command \"clippy\"' runs clippy over the workspace on every save,
which is noticeably slow on a cold target directory; a project
.dir-locals.el can override this back to `check' if that matters.
`cargo (:features \"all\")' is deliberately left out — it is a real cost
on large workspaces, so it stays a per-project decision."
  (setq-local eglot-workspace-configuration
              '(:rust-analyzer
                (:check (:command "clippy")
                 :cargo (:buildScripts (:enable t))
                 :procMacro (:enable t)))))

;;; @doc Built-in tree-sitter Rust mode. Eglot wires rust-analyzer in
;;; init-prog, with buffer-local workspace configuration (clippy on save,
;;; build scripts, proc macros) set here in a mode hook; format-on-save
;;; runs rustfmt through apheleia.
(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :hook (rust-ts-mode . jotain-rust--eglot-workspace-config)
  :custom
  (rust-ts-mode-indent-offset 4))

(provide 'init-lang-rust)
;;; init-lang-rust.el ends here
