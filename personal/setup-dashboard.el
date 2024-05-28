(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq ns-right-alternate-modifier 'none)

  (setq dashboard-items '((recents . 5)
 			  (projects . 20)
			  (bookmarks . 5)
			  ))
  (setq dashboard-navigation-cycle t)
  (setq dashboard-banner-logo-title "Teretulemast!")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-icon-type 'all-the-icons)  ; use `all-the-icons' package
  (setq dashboard-projects-backend 'projectile)
  )


;; Show dashboard with emacsclient
(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
