(linum-mode t)

(when (maybe-require-package 'function-args)
  (fa-config-default))

;;(require 'semantic)
(when (require-package 'semantic)
  (require 'semantic)

  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode)

  (global-semantic-idle-scheduler-mode t)
  (global-semantic-idle-completions-mode t)
  ;;(global-semantic-decoration-mode t)
  (global-semantic-highlight-func-mode t)
  ;;(global-semantic-show-unmatched-syntax-mode t)
  ;; (global-semantic-idle-completions-mode t)

  ;;(global-semantic-decoration-mode t)
  ;;(global-semantic-highlight-func-mode t)
  ;;(global-semantic-show-unmatched-syntax-mode t)
  (semantic-add-system-include "/usr/include/boost" 'c++-mode)
  (semantic-add-system-include "/usr/include/gtkmm-3.0/" 'c++-mode)
  ;;  (semantic-mode t)
  )

;;'(flycheck-clang-args (quote ("-std=c++14")))
;;'(flycheck-display-errors-function (quote flycheck-display-error-messages-unless-error-list))
;;'(flycheck-gcc-args (quote ("-std=c++14")))
(provide 'init-c)
