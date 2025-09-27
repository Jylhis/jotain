;;; systems.el --- sysadmin and stuff to work with systems -*- lexical-binding: t; -*-

;;; Commentary:
;; logview etc.
;;; Code:

(use-package logview
  :ensure
  :custom
  (logview-additional-submodes '(("ROS2" (format . "[LEVEL] [TIMESTAMP] [NAME]:") (levels . "SLF4J")
                                  (timestamp "ROS2"))))
  (logview-additional-timestamp-formats '(("ROS2" (java-pattern . "A.SSSSSSSSS"))))
  )

(use-package sops
  :ensure t
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
  :custom
  ;; Set the default vault name (customize as needed)
  ;; You can find your vault names with: op vault list
  (auth-source-1password-vault "Private")
  
  ;; Configure 1Password CLI command (defaults to "op")
  (auth-source-1password-op-executable "op")
  
  ;; Enable debug mode for troubleshooting (disable in production)
  (auth-source-1password-debug nil)
  
  ;; Cache duration for 1Password sessions (in seconds)
  ;; Set to nil to disable caching
  (auth-source-1password-cache-ttl 3600)
  
  :config
  ;; Enable auth-source-1password as a backend
  (auth-source-1password-enable)
  
  ;; Add custom search behavior for better matching
  ;; This helps when searching for credentials by host/service
  (setq auth-source-1password-search-fields '("title" "website" "url"))
  
  ;; Optional: Add a custom function to validate 1Password CLI availability
  (defun my/check-1password-cli ()
    "Check if 1Password CLI is available and authenticated."
    (interactive)
    (if (executable-find "op")
        (if (zerop (call-process "op" nil nil nil "account" "list"))
            (message "1Password CLI is available and authenticated")
          (message "1Password CLI found but not authenticated. Run 'op signin'"))
      (message "1Password CLI not found. Please install it first.")))
  
  ;; Bind a key to check 1Password status (optional)
  :bind ("C-c a p" . my/check-1password-cli))

(provide 'systems)
;;; systems.el ends here
