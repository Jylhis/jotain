;;; platform-examples.el --- Examples of platform-aware configurations -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; This file contains examples showing how to use the platform detection
;; system to create platform-aware configurations. These examples demonstrate
;; the patterns that can be applied throughout the configuration.

;;; Code:

(require 'platform)

;;; Example 1: Platform-conditional package loading
;; Only load packages that work on the current platform

;; Icons - only useful on GUI systems
(platform-when platform-gui-p
  (use-package nerd-icons :ensure))

;; Terminal emulator - not available on Android/Windows
(platform-unless (or platform-android-p platform-windows-p)
  (use-package vterm :ensure))

;; PDF tools - doesn't work well on Android  
(platform-unless platform-android-p
  (use-package pdf-tools
    :ensure
    :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)))

;;; Example 2: Platform-specific package configuration
;; Configure the same package differently based on platform

(use-package magit
  :ensure
  :bind ("C-c g" . magit-status)
  :config
  ;; Disable expensive features on Android for performance
  (platform-when platform-android-p
    (setq magit-diff-refine-hunk nil)
    (setq magit-revision-show-gravatars nil))
  
  ;; Enable delta on platforms that support it
  (platform-unless platform-android-p
    (when (executable-find "delta")
      (use-package magit-delta
        :ensure
        :hook (magit-mode . magit-delta-mode)))))

;;; Example 3: Platform-specific keybindings
;; Different key bindings for different platforms

(platform-cond
 ;; macOS: Use Command key for system-like shortcuts
 (platform-macos-p
  (global-set-key (kbd "s-c") 'kill-ring-save)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-x") 'kill-region))
 
 ;; Android: Touch-optimized shortcuts
 (platform-android-p
  (global-set-key (kbd "C-<tab>") 'other-window)
  (global-set-key (kbd "C-c s") 'consult-line))
 
 ;; Other platforms: Standard Emacs shortcuts
 (t
  (global-set-key (kbd "C-c C-s") 'consult-line)))

;;; Example 4: Platform-aware UI configuration
;; Adjust UI elements based on platform capabilities

(platform-cond
 ;; Android: Optimize for touch and small screens
 (platform-android-p
  (setq-default line-spacing 0.2) ; More breathing room
  (global-visual-line-mode 1)     ; Better for touch scrolling
  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1)))
 
 ;; macOS: Native look and feel
 (platform-macos-p
  (setq ns-use-native-fullscreen t)
  (setq mac-allow-anti-aliasing t))
 
 ;; Linux: X11 optimizations
 (platform-linux-p
  (setq x-select-enable-clipboard t)
  (setq x-select-enable-primary t)))

;;; Example 5: Conditional feature loading based on platform capabilities
;; Enable features only if the platform supports them

(when (platform-has-feature-p 'notifications)
  (use-package alert
    :ensure
    :config
    (setq alert-default-style 'notifications)))

(when (platform-has-feature-p 'treesitter)
  (use-package treesit-auto
    :ensure
    :config
    (global-treesit-auto-mode)))

;;; Example 6: Platform-specific directory and file settings
;; Use appropriate paths for each platform

(platform-cond
 (platform-android-p
  (setq org-directory "/storage/emulated/0/Documents/org")
  (setq default-directory "/storage/emulated/0/"))
 
 (platform-macos-p
  (setq org-directory "~/Documents/org")
  (setq default-directory "~/"))
 
 (t
  (setq org-directory "~/Documents/org")))

;;; Example 7: Platform-aware performance tuning
;; Different performance settings for different platforms

(platform-cond
 ;; Android: Aggressive optimization for mobile devices
 (platform-android-p
  (setq gc-cons-threshold (* 20 1024 1024)) ; 20MB
  (setq read-process-output-max (* 512 1024)) ; 512KB
  (setq auto-save-default nil) ; Disable for performance
  (setq make-backup-files nil))
 
 ;; Desktop platforms: Balanced settings  
 (t
  (setq gc-cons-threshold (* 2 1024 1024)) ; 2MB
  (setq read-process-output-max (* 1024 1024)))) ; 1MB

;;; Example 8: Platform-specific package alternatives
;; Use different packages for the same functionality on different platforms

(platform-cond
 ;; Android: Use simpler alternatives
 (platform-android-p
  (use-package simple-mpc :ensure)) ; Instead of full EMMS
 
 ;; Desktop: Full-featured packages
 ((or platform-linux-p platform-macos-p)
  (use-package emms :ensure)))

;;; Example 9: Platform detection for external tools
;; Configure based on available external tools

(when (and platform-linux-p (executable-find "notify-send"))
  (setq alert-default-style 'libnotify))

(when (and platform-macos-p (executable-find "terminal-notifier"))
  (use-package terminal-notifier :ensure))

;;; Example 10: Using the platform-use-package macro
;; Convenient macro for platform-aware package loading

(defmacro platform-use-package (name &rest args)
  "Use package NAME with platform-specific conditions.
This is a helper macro that extends use-package with platform awareness."
  (declare (indent 1))
  `(when (not (memq ',name (bound-and-true-p platform-disabled-packages)))
     (use-package ,name ,@args)))

;; Example usage:
(platform-use-package pdf-tools  ; Will be disabled on Android
  :ensure
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode))

;;; Example 11: Platform-specific hooks
;; Add different behaviors based on platform

(platform-when platform-android-p
  (add-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'prog-mode-hook #'visual-line-mode))

(platform-when platform-gui-p
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (when (display-graphic-p frame)
                (with-selected-frame frame
                  ;; GUI-specific initialization
                  (when (fboundp 'tool-bar-mode) (tool-bar-mode -1)))))))

;;; Example 12: Platform-aware theme configuration
;; Different themes or theme settings for different platforms

(platform-cond
 (platform-android-p
  ;; High contrast themes work better on mobile screens
  (load-theme 'modus-vivendi t))
 
 (platform-gui-p
  ;; Use auto-dark on GUI systems
  (use-package auto-dark
    :ensure
    :config
    (auto-dark-mode 1)))
 
 (t
  ;; Terminal: Simple themes
  (load-theme 'leuven t)))

(provide 'platform-examples)
;;; platform-examples.el ends here