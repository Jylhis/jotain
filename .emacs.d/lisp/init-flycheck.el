(when (maybe-require-package 'flycheck)
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++14")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14")))

;; TODO: https://github.com/flycheck/flycheck-color-mode-line
(provide 'init-flycheck)
