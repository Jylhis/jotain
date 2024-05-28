(require 'use-package)

;;; JSON Support
(use-package json-snatcher
  :straight t
  )
(use-package json-mode
  :straight t
  :mode "\\.json\\'"
  :after
  (json-snatcher)
  )
