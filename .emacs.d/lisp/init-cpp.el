
(global-linum-mode t)




(when (maybe-require-package 'function-args)
  ;;  (fa-config-default)
  )

(require 'semantic)
(global-semanticdb-minor-mode)
(global-semantic-idle-scheduler-mode t)
(semantic-add-system-include "/usr/include/boost" 'c++-mode)
(semantic-add-system-include "/usr/include/gtkmm-3.0/" 'c++-mode)
(semantic-mode t)

(provide 'init-cpp)
