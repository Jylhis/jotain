;;; init-lang-rust.el --- Rust language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust mode (built-in tree-sitter variant), eglot wired through
;; rust-analyzer (the hook lives in `init-prog'), and a couple of
;; conveniences for the typical edit-test-format loop.

;;; Code:

(declare-function jotain-eglot-set-workspace-config "init-prog" (key settings))

;; rust-analyzer workspace settings (see `jotain-eglot-set-workspace-config'
;; in init-prog for how the section reaches the server).
;; `check.command "clippy"' runs clippy over the workspace on every save,
;; which is noticeably slow on a cold target directory; a project
;; .dir-locals.el can override this back to `check' if that matters.
;; `cargo (:features "all")' is deliberately left out — it is a real cost
;; on large workspaces, so it stays a per-project decision.
(jotain-eglot-set-workspace-config
 :rust-analyzer
 '(:check (:command "clippy")
   :cargo (:buildScripts (:enable t))
   :procMacro (:enable t)))

;;; @doc Built-in tree-sitter Rust mode. Eglot wires rust-analyzer in
;;; init-prog, with workspace configuration (clippy on save, build
;;; scripts, proc macros) contributed to the global
;;; `eglot-workspace-configuration' above; format-on-save runs rustfmt
;;; through apheleia.
(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :custom
  (rust-ts-mode-indent-offset 4))

(provide 'init-lang-rust)
;;; init-lang-rust.el ends here
