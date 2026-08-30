;;; init-systems.el --- Sysadmin: secrets, log files, auth -*- lexical-binding: t; -*-

;;; Commentary:

;; Tools you reach for when working *on* systems rather than *in*
;; them: SOPS-encrypted file editing, log file viewing, and the
;; 1Password auth-source backend that gptel/forge/etc. consume
;; transparently.

;;; Code:

;; Forward declaration: `auth-sources' is a defcustom in the built-in
;; auth-source library, referenced below inside `with-eval-after-load'.
(defvar auth-sources)

;;;; auth-source-1password

;;; @doc Pulls credentials from the 1Password CLI (`op`). Once enabled,
;;; every package that uses auth-source — magit/forge, gptel,
;;; smtpmail, circe — resolves credentials by host against the
;;; 1Password vault transparently.
;;
;; Additional authinfo files (e.g. a sops-nix/agenix secret decrypted to a
;; runtime path) are handed in through the JOTAIN_AUTH_SOURCES env var, set
;; by the module option `services.jotain.authSources', and searched ahead of
;; the defaults.
(use-package auth-source-1password
  :defer t
  :functions (auth-source-1password-enable)
  :custom
  (auth-source-1password-vault "Private")
  (auth-source-1password-op-executable "op")
  (auth-source-1password-cache-ttl 3600)
  :init
  ;; Credentials are only looked up when a consumer (gptel, forge, …)
  ;; loads auth-source, so register everything lazily at that point.
  (with-eval-after-load 'auth-source
    ;; Prepend any module-declared authinfo files (colon-separated paths in
    ;; JOTAIN_AUTH_SOURCES) so they take priority over ~/.authinfo(.gpg).
    (when-let* ((paths (getenv "JOTAIN_AUTH_SOURCES")))
      (setopt auth-sources (append (split-string paths ":" t) auth-sources)))
    (require 'auth-source-1password)
    (auth-source-1password-enable))
  :config
  (setopt auth-source-1password-search-fields '("title" "website" "url")))

;;;; sops — transparent encryption for YAML/JSON/env files

;; sops-mode has no built-in keymap, so we declare one ourselves
;; before registering it on `minor-mode-map-alist'.
(defvar sops-mode-map (make-sparse-keymap)
  "Keymap for `sops-mode'.")

;;; @doc Transparent SOPS encrypt/decrypt for YAML/JSON/env files. C-c
;;; C-c saves an encrypted edit; C-c C-d toggles into the editing
;;; view; C-c C-k cancels.
(use-package sops
  ;; The global mode hooks find-file; after-init runs before
  ;; command-line file arguments are visited, so nothing is missed.
  :hook (after-init . global-sops-mode)
  :functions (sops-save-file sops-cancel sops-edit-file)
  :config
  (define-key sops-mode-map (kbd "C-c C-c") #'sops-save-file)
  (define-key sops-mode-map (kbd "C-c C-k") #'sops-cancel)
  (define-key sops-mode-map (kbd "C-c C-d") #'sops-edit-file)
  (let ((entry (assq 'sops-mode minor-mode-map-alist)))
    (if entry
        (setcdr entry sops-mode-map)
      (add-to-list 'minor-mode-map-alist (cons 'sops-mode sops-mode-map)))))

;;;; logview — major mode for log files

;;; @doc Major mode for log files — level filtering, timestamp parsing,
;;; thread highlighting. Configured for SLF4J (Java/Kotlin) and a
;;; custom ROS2 submode.
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
