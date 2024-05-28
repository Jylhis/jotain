(require 'use-package)


(use-package go-mode
  :straight t
  :after
  (lsp-mode)
  :bind (:map go-mode-map
 	      ("C-c C-f" . 'gofmt))
  ;:hook (before-save . gofmt-before-save)
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'yas-minor-mode)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  )


(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
