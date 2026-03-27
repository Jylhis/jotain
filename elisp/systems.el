;;; systems.el --- sysadmin and stuff to work with systems -*- lexical-binding: t; -*-

;;; Commentary:
;; logview etc.
;;; Code:

(use-package logview
  :ensure t
  :defer t
  :custom
  (logview-additional-submodes '(("ROS2" (format . "[LEVEL] [TIMESTAMP] [NAME]:") (levels . "SLF4J")
                                  (timestamp "ROS2"))))
  (logview-additional-timestamp-formats '(("ROS2" (java-pattern . "A.SSSSSSSSS"))))
  )

;; sops-mode has no built-in keymap; declare one so bindings are mode-scoped.
(defvar sops-mode-map (make-sparse-keymap) "Keymap for `sops-mode'.")

(use-package sops
  :demand t
  :config
  (global-sops-mode 1)
  (define-key sops-mode-map (kbd "C-c C-c") #'sops-save-file)
  (define-key sops-mode-map (kbd "C-c C-k") #'sops-cancel)
  (define-key sops-mode-map (kbd "C-c C-d") #'sops-edit-file)
  ;; Register keymap — update existing entry or add a new one
  (let ((entry (assq 'sops-mode minor-mode-map-alist)))
    (if entry
        (setcdr entry sops-mode-map)
      (add-to-list 'minor-mode-map-alist (cons 'sops-mode sops-mode-map)))))

;; 1Password integration for auth-source
;; This allows Emacs to securely retrieve credentials from 1Password
;; for various applications like email, git, JIRA, etc.
(use-package auth-source-1password
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
  (setopt auth-source-1password-search-fields '("title" "website" "url")))

(provide 'systems)
;;; systems.el ends here
