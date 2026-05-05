;;; init-lang-python.el --- Python language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Python mode (built-in tree-sitter variant) plus the usual conveniences.
;; The eglot hook is registered in `init-prog'; the LSP server itself
;; (pyright / basedpyright / ruff-lsp) comes from the project's own
;; environment, not from this config.

;;; Code:

;;; @doc Built-in Python mode pinned to its tree-sitter variant so we
;;; @doc get the modern parser without any third-party package. The LSP
;;; @doc server (pyright/basedpyright/ruff-lsp) comes from the project's
;;; @doc own environment, not from this config.
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python3" . python-ts-mode)
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3"))

(provide 'init-lang-python)
;;; init-lang-python.el ends here
