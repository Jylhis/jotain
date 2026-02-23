;;; android.el --- Enhanced Android/Termux configuration -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Enhanced Android-specific configuration based on Termux environment.
;; This module provides Android-optimized keybindings, UI adaptations,
;; and hardware-specific features like volume key bindings and modifier bar.

;;; Code:

;;; Package Customizations for Android

(use-package gnu-elpa-keyring-update
  :ensure t)

;; Disable packages that don't work well on Android
;; TODO: Move to package specific configs
(defvar android-disabled-packages
  '(vterm pdf-tools exwm magit-delta all-the-icons-dired
	  lsp-mode eglot company-mode flycheck)
  "Packages to disable on Android for performance/compatibility.")

;; Configure remaining packages for Android
(with-eval-after-load 'org
  ;; Simpler org setup for mobile
  (setq org-startup-folded 'content)
  (setq org-image-actual-width '(400)) ; Smaller images
  (setq org-hide-emphasis-markers t))

(with-eval-after-load 'magit
  ;; Simpler magit interface
  (setq magit-diff-refine-hunk nil) ; Disable for performance
  (setq magit-revision-show-gravatars nil))

;;; Hardware Integration
;; Enable modifier bar for easier key input


(when (fboundp 'modifier-bar-mode)
  (modifier-bar-mode t))

;; Smooth scrolling for touch screens
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))


  ;;; Performance Optimizations
;; Reduce GC pressure on mobile devices
(setq gc-cons-threshold (* 20 1024 1024)) ; 20MB
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Simplify some UI elements for performance
(setq-default mode-line-format
	      '("%e" mode-line-front-space
		(:propertize mode-line-buffer-identification
			     face mode-line-buffer-id)
		" " mode-line-position
		(:eval (when (> (buffer-size) 1000)
			 (format " %dk" (/ (buffer-size) 1000))))
		" " (:propertize "%m" face mode-line-buffer-id)
		mode-line-end-spaces))
