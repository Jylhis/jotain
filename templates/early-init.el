;;; early-init.el --- Jotain Emacs early initialization -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Early initialization for Emacs 30+.  This file is loaded before the GUI
;; is initialized and before package.el runs.  Used for performance
;; optimization and UI tweaks that need to happen before frame creation.

;;; Code:

;; Defer garbage collection during startup for faster load times
;; Will be reset by jotain-gc.el after initialization
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Disable package.el - we use Nix for package management
(setq package-enable-at-startup nil)

;; Native compilation settings (Emacs 30+)
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  ;; Silence compiler warnings during init
  (setq native-comp-async-report-warnings-errors nil)
  ;; Don't store eln-cache in user-emacs-directory
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name "var/eln-cache/" user-emacs-directory))))
  ;; Increase native compilation speed
  (setq native-comp-speed 2))

;; UI optimizations - disable UI elements before frame creation
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;; Disable startup screen
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name)

;; Faster to disable these here before GUI init
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; Prevent unwanted runtime compilation for performance
(setq byte-compile-warnings '(not obsolete))

(provide 'early-init)
;;; early-init.el ends here
