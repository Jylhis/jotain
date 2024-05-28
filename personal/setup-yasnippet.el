
(use-package yasnippet
  :straight t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )

(use-package yasnippet-snippets
  :straight t
  :after (yasnippet))
