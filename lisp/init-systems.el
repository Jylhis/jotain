;;; init-systems.el --- Sysadmin: secrets, log files, auth -*- lexical-binding: t; -*-

;;; Commentary:

;; Sysadmin tools: the 1Password auth-source backend, SOPS-encrypted
;; file editing, and log file viewing.

;;; Code:

(defvar auth-sources)

;;;; auth-source-1password

;;; @doc Pulls credentials from the 1Password CLI (`op`), so every
;;; auth-source consumer (forge, gptel, smtpmail) can resolve them from
;;; the vault by host.
(use-package auth-source-1password
  :defer t
  :functions (auth-source-1password-enable)
  :custom
  (auth-source-1password-vault "Private")
  (auth-source-1password-op-executable "op")
  (auth-source-1password-cache-ttl 3600)
  :init
  ;; Register lazily, when a consumer first loads auth-source.
  (with-eval-after-load 'auth-source
    ;; Module-declared authinfo files (colon-separated JOTAIN_AUTH_SOURCES)
    ;; take priority over ~/.authinfo(.gpg).
    (when-let* ((paths (getenv "JOTAIN_AUTH_SOURCES")))
      (setopt auth-sources (append (split-string paths ":" t) auth-sources)))
    (require 'auth-source-1password)
    (auth-source-1password-enable))
  :config
  (setopt auth-source-1password-search-fields '("title" "website" "url")))

;;;; sops — transparent encryption for YAML/JSON/env files

;; sops-mode ships no keymap; define one and register it below.
(defvar sops-mode-map (make-sparse-keymap)
  "Keymap for `sops-mode'.")

;;; @doc Transparent SOPS encrypt/decrypt for YAML/JSON/env files.
;;; C-c C-d opens the decrypted edit view, C-c C-c saves it encrypted,
;;; C-c C-k cancels.
(use-package sops
  :commands (global-sops-mode)
  :functions (sops-save-file sops-cancel sops-edit-file)
  :init
  ;; after-init runs before command-line files are visited, so none are
  ;; missed.  The `sops' CLI is opt-in (`services.jotain.sops.enable');
  ;; without it the mode logs "executable not found: sops" on every
  ;; candidate file.
  (when (executable-find "sops")
    (add-hook 'after-init-hook #'global-sops-mode))
  :config
  (define-key sops-mode-map (kbd "C-c C-c") #'sops-save-file)
  (define-key sops-mode-map (kbd "C-c C-k") #'sops-cancel)
  (define-key sops-mode-map (kbd "C-c C-d") #'sops-edit-file)
  (let ((entry (assq 'sops-mode minor-mode-map-alist)))
    (if entry
        (setcdr entry sops-mode-map)
      (add-to-list 'minor-mode-map-alist (cons 'sops-mode sops-mode-map)))))

;;;; logview — major mode for log files

;;; @doc Major mode for log files: level filtering, timestamp parsing,
;;; thread highlighting. Adds a ROS2 submode on top of the built-in ones.
(use-package logview
  :defer t
  :custom
  (logview-cache-filename (jotain-var-file "logview-cache"))
  (logview-additional-submodes
   '(("ROS2" (format . "[LEVEL] [TIMESTAMP] [NAME]:")
             (levels . "SLF4J")
             (timestamp "ROS2"))))
  (logview-additional-timestamp-formats
   '(("ROS2" (java-pattern . "A.SSSSSSSSS")))))

(provide 'init-systems)
;;; init-systems.el ends here
