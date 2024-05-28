(require 'use-package)

;; EditorConfig
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1)
  )
(use-package editorconfig-generate
  :straight t
  )

(use-package editorconfig-domain-specific
  :straight t
  )

(use-package editorconfig-custom-majormode
  :straight t)
