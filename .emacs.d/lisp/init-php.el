(when (maybe-require-package 'web-mode)
  ;;(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
  (maybe-require-package 'smarty-mode))

(provide 'init-php)
