;;; init-lang-rust.el --- Rust language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust mode (built-in tree-sitter variant), eglot wired through
;; rust-analyzer (the hook lives in `init-prog'), and a couple of
;; conveniences for the typical edit-test-format loop.

;;; Code:

;;; @doc Built-in tree-sitter Rust mode. Eglot wires rust-analyzer in
;;; init-prog; format-on-save runs rustfmt through apheleia.
(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :custom
  (rust-ts-mode-indent-offset 4))

(provide 'init-lang-rust)
;;; init-lang-rust.el ends here
