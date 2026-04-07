;;; init-systems.el --- Sysadmin: secrets, log files, auth -*- lexical-binding: t; -*-

;;; Commentary:

;; Tools you reach for when working *on* systems rather than *in*
;; them: SOPS-encrypted file editing, log file viewing, and the
;; 1Password auth-source backend that gptel/forge/etc. consume
;; transparently.

;;; Code:

;;;; auth-source-1password

;; Pull credentials from the 1Password CLI (`op'). Once enabled, any
;; package that uses auth-source — magit/forge, gptel, smtpmail,
;; circe, etc. — can ask for credentials by host and they'll be
;; resolved against your 1Password vault.
(use-package auth-source-1password
  :defer t
  :custom
  (auth-source-1password-vault "Private")
  (auth-source-1password-op-executable "op")
  (auth-source-1password-cache-ttl 3600)
  :config
  (auth-source-1password-enable)
  (setopt auth-source-1password-search-fields '("title" "website" "url")))

;;;; sops — transparent encryption for YAML/JSON/env files

;; sops-mode has no built-in keymap, so we declare one ourselves
;; before registering it on `minor-mode-map-alist'.
(defvar sops-mode-map (make-sparse-keymap)
  "Keymap for `sops-mode'.")

(use-package sops
  :demand t
  :config
  (global-sops-mode 1)
  (define-key sops-mode-map (kbd "C-c C-c") #'sops-save-file)
  (define-key sops-mode-map (kbd "C-c C-k") #'sops-cancel)
  (define-key sops-mode-map (kbd "C-c C-d") #'sops-edit-file)
  (let ((entry (assq 'sops-mode minor-mode-map-alist)))
    (if entry
        (setcdr entry sops-mode-map)
      (add-to-list 'minor-mode-map-alist (cons 'sops-mode sops-mode-map)))))

;;;; logview — major mode for log files

(use-package logview
  :defer t
  :custom
  (logview-additional-submodes
   '(("ROS2" (format . "[LEVEL] [TIMESTAMP] [NAME]:")
             (levels . "SLF4J")
             (timestamp "ROS2"))))
  (logview-additional-timestamp-formats
   '(("ROS2" (java-pattern . "A.SSSSSSSSS")))))

(provide 'init-systems)
;;; init-systems.el ends here
