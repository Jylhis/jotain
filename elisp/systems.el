;;; systems.el --- sysadmin and stuff to work with systems -*- lexical-binding: t; -*-

;;; Commentary:
;; logview etc.
;;; Code:

(use-package logview
  :ensure
  :defer t
  :custom
  (logview-additional-submodes '(("ROS2" (format . "[LEVEL] [TIMESTAMP] [NAME]:") (levels . "SLF4J")
                                  (timestamp "ROS2"))))
  (logview-additional-timestamp-formats '(("ROS2" (java-pattern . "A.SSSSSSSSS"))))
  )

(use-package sops
  :ensure t
  :demand t
  :bind (("C-c C-c" . sops-save-file)
         ("C-c C-k" . sops-cancel)
         ("C-c C-d" . sops-edit-file))
  :config
  (global-sops-mode 1))

;; 1Password integration for auth-source
;; This allows Emacs to securely retrieve credentials from 1Password
;; for various applications like email, git, JIRA, etc.
(use-package auth-source-1password
  :ensure t
  :defer t
  :custom
  ;; Set the default vault name (customize as needed)
  ;; You can find your vault names with: op vault list
  (auth-source-1password-vault "Private")
  (auth-source-1password-op-executable "op")
  ;; (auth-source-1password-debug nil)
  (auth-source-1password-cache-ttl 3600)
  :config
  ;; Enable auth-source-1password as a backend
  (auth-source-1password-enable)
  ;; Add custom search behavior for better matching
  ;; This helps when searching for credentials by host/service
  (setq auth-source-1password-search-fields '("title" "website" "url")))

(provide 'systems)
;;; systems.el ends here
