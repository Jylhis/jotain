(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

;; TODO: https://github.com/flycheck/flycheck-color-mode-line
(provide 'init-flycheck)
