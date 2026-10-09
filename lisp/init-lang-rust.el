;;; init-lang-rust.el --- Rust language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Built-in `rust-ts-mode' and rust-analyzer workspace settings.  The
;; eglot hook lives in `init-prog'.

;;; Code:

(declare-function jotain-eglot-set-workspace-config "init-prog" (key settings))

;; `check.command "clippy"' runs clippy on every save, slow on a cold
;; target directory; a project .dir-locals.el can switch back to `check'.
;; `cargo (:features "all")' is left out: costly on large workspaces, so
;; it stays a per-project choice.
(jotain-eglot-set-workspace-config
 :rust-analyzer
 '(:check (:command "clippy")
   :cargo (:buildScripts (:enable t))
   :procMacro (:enable t)))

;;; @doc Built-in tree-sitter Rust mode. rust-analyzer runs clippy on
;;; save with build scripts and proc macros enabled; format-on-save runs
;;; rustfmt through apheleia.
(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :custom
  (rust-ts-mode-indent-offset 4))

(provide 'init-lang-rust)
;;; init-lang-rust.el ends here
