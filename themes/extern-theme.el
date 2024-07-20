;; Enable transparent title bar on macOS
(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . light)) ;; {light, dark}
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(set-frame-font "Source Code Pro 12")
(defalias 'yes-or-no #'y-or-n-p)
(setq confirm-kill-emacs #'yes-or-no-p)

(delete-selection-mode t)

(use-package all-the-icons
  :straight t)
(use-package all-the-icons-dired
  :straight t)

(use-package treemacs-all-the-icons
  :straight t)

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-solarized-light t)
  (load-theme 'doom-solarized-dark t)

  (setq doom-themes-treemacs-theme "doom-colors")
  (doom-themes-treemacs-config)
  (enable-theme 'doom-solarized-light)
  )

;;(if (display-graphic-p)
    ;;(enable-theme 'doom-solarized-light)
;;    (enable-theme 'doom-solarized-light)
;;  (enable-theme 'doom-solarized-dark))


(use-package solaire-mode
  :straight t
  :config
  (solaire-global-mode +1))
