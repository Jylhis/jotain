;; An extremely feature-rich git client. Activate it with "C-c g".
(use-package magit
  :straight t
  :bind (("C-c g" . magit-status))
  :config
  ;; Show word-granularity differences within diff hunks
  (setq magit-diff-refine-hunk t)
  (setq magit-repository-directories
        '(;; Directory containing project root directories
          ("~/Developer"      . 2)
          ;; Specific project root directory
          ("~/.emacs.d" . 1)))
  )
