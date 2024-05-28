(require 'use-package)


(use-package rust-mode
  :straight t
  :config
  (setq rust-mode-treesitter-derive t)
  (add-hook 'rust-mode-hook #'lsp))
