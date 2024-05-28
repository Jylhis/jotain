(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(line-number-mode t)
 '(safe-local-variable-values
   '((eval google-set-c-style) (projectile-enable-cmake-presets t)
     (setq projectile-enable-caching t)
     (ros-workspaces (("tramp-prefix")))
     (format-all-formatters ("Nix" alejandra))
     (eval cmake-font-lock-activate)
     (lsp-enabled-clients pylsp makels bash-ls clangd)))
 '(show-paren-mode 1)
 '(warning-suppress-types '((comp) (use-package) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
