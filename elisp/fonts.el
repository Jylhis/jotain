;;; fonts.el --- Font configuration and management -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Comprehensive font configuration system that handles:
;; - Font availability detection with fallback chains
;; - Daemon/client consistency across sessions
;; - Platform-specific optimizations
;; - Variable vs fixed-pitch management
;; - Integration with themes and UI elements
;; - Performance optimizations

;;; Code:

(require 'platform)

;;; Font Configuration Variables

(defcustom jylhis-fonts-default-family
  '(("JetBrainsMono Nerd Font" . 110)
    ("FiraCode Nerd Font" . 110)
    ("Iosevka Nerd Font" . 110)
    ("CascadiaCode Nerd Font" . 110)
    ("Hack Nerd Font" . 110)
    ("DejaVu Sans Mono" . 100))
  "Programming font preferences with fallbacks.
Each entry is (font-name . height-in-points*10)."
  :type '(alist :key-type string :value-type integer)
  :group 'jylhis-fonts)

(defcustom jylhis-fonts-variable-family  
  '(("Inter" . 120)
    ("SF Pro Text" . 120)
    ("Segoe UI" . 120)
    ("Ubuntu" . 120)
    ("DejaVu Sans" . 110))
  "Variable pitch fonts for prose and UI elements."
  :type '(alist :key-type string :value-type integer)
  :group 'jylhis-fonts)

(defcustom jylhis-fonts-serif-family
  '(("Source Serif Pro" . 120)
    ("Liberation Serif" . 120)
    ("DejaVu Serif" . 110))
  "Serif fonts for formal documents and reading."
  :type '(alist :key-type string :value-type integer)
  :group 'jylhis-fonts)

;;; Font Detection and Utilities

(defvar jylhis-fonts--available-cache nil
  "Cache of available font families to avoid repeated system calls.")

(defun jylhis-fonts--get-available-families ()
  "Get list of available font families, cached for performance."
  (unless jylhis-fonts--available-cache
    (setq jylhis-fonts--available-cache (font-family-list)))
  jylhis-fonts--available-cache)

(defun jylhis-fonts--find-first-available (font-list)
  "Find first available font from FONT-LIST preference list.
Returns (font-name . height) or nil if none found."
  (let ((available-fonts (jylhis-fonts--get-available-families)))
    (seq-find (lambda (font-spec)
                (member (car font-spec) available-fonts))
              font-list)))

(defun jylhis-fonts--set-face-font (face font-spec)
  "Set FACE to use FONT-SPEC (font-name . height)."
  (when font-spec
    (let ((font-name (car font-spec))
          (font-height (cdr font-spec)))
      (set-face-attribute face nil
                          :family font-name
                          :height font-height)
      (message "jylhis-fonts: Set %s to %s (height %d)" 
               face font-name font-height)
      font-spec)))

;;; Core Font Setup Functions

(defun jylhis-fonts-setup-default ()
  "Setup default monospace programming font."
  (let ((font (jylhis-fonts--find-first-available jylhis-fonts-default-family)))
    (when font
      (jylhis-fonts--set-face-font 'default font)
      (jylhis-fonts--set-face-font 'fixed-pitch font))))

(defun jylhis-fonts-setup-variable-pitch ()
  "Setup variable pitch font for prose and UI."
  (let ((font (jylhis-fonts--find-first-available jylhis-fonts-variable-family)))
    (when font
      (jylhis-fonts--set-face-font 'variable-pitch font))))

(defun jylhis-fonts-setup-serif ()
  "Setup serif font for formal reading."
  (let ((font (jylhis-fonts--find-first-available jylhis-fonts-serif-family)))
    (when font
      ;; Create a custom serif face for org-mode and similar
      (unless (facep 'jylhis-fonts-serif)
        (defface jylhis-fonts-serif
          '((t :inherit variable-pitch))
          "Serif font face for formal text."))
      (jylhis-fonts--set-face-font 'jylhis-fonts-serif font))))

;;; Performance Optimizations

(defun jylhis-fonts-setup-performance ()
  "Apply font-related performance optimizations."
  ;; Prevent font cache compaction during GC
  (setq inhibit-compacting-font-caches t)
  
  ;; Enable font scaling
  (setq scalable-fonts-allowed t)
  
  ;; Reduce font rendering overhead
  (setq font-use-system-font t)
  
  ;; Better Unicode handling
  (set-fontset-font t 'unicode (font-spec :name "Noto Color Emoji") nil 'prepend))

;;; Ligature Support

(use-package ligature
  :config
  ;; Enable ligatures for programming modes
  (ligature-set-ligatures
   '(prog-mode nix-mode)
   '("--" "---" "==" "===" "!=" "!==" "=!=" "=:=" "=/=" "<="
     ">=" "&&" "&&&" "&=" "+=" "-=" "*=" "/=" "%=" "|=" "||"
     "||=" "|>" "^=" "=>" "->>" "<<-" "<<=" "<=>" "<->" "<--"
     "<-<" "<<" ">>" "<<<" ">>>" "<=" "<=<" "=<<" ">>=" ">=>"
     ">>-" ">->" "<-" "-<" "-<<" ">-" "<~>" "-~" "~@" "^?" "//"
     "///" "&&&" "|||" "???" "***" "+++" ":::" "::"))
  
  ;; Enable globally
  (global-ligature-mode t))

;;; Theme Integration

(defun jylhis-fonts-setup-theme-faces ()
  "Setup additional font faces that work well with themes."
  ;; Comments with subtle styling
  (set-face-attribute 'font-lock-comment-face nil
                      :slant 'italic
                      :weight 'normal)
  
  ;; Documentation strings
  (set-face-attribute 'font-lock-doc-face nil
                      :slant 'italic)
  
  ;; Mode line with appropriate sizing
  (set-face-attribute 'mode-line nil
                      :height 0.9)
  (set-face-attribute 'mode-line-inactive nil
                      :height 0.9))

;;; Daemon/Client Compatibility

(defun jylhis-fonts--setup-frame (&optional frame)
  "Setup fonts for FRAME (or current frame if nil).
This ensures consistent fonts across daemon and client sessions."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (jylhis-fonts-setup-default)
      (jylhis-fonts-setup-variable-pitch)
      (jylhis-fonts-setup-serif)
      (jylhis-fonts-setup-theme-faces)
      (message "jylhis-fonts: Frame font setup completed"))))

(defun jylhis-fonts-setup ()
  "Main font setup function."
  ;; Performance optimizations first
  (jylhis-fonts-setup-performance)
  
  ;; Setup fonts for current frame
  (jylhis-fonts--setup-frame)
  
  ;; Ensure new frames get font setup (critical for daemon/client)
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'jylhis-fonts--setup-frame)
    ;; For non-daemon, ensure theme changes reapply fonts
    (add-hook 'after-load-theme-hook #'jylhis-fonts-setup-theme-faces)))

;;; Interactive Commands

(defun jylhis-fonts-increase-size ()
  "Increase font size for current buffer."
  (interactive)
  (text-scale-increase 1))

(defun jylhis-fonts-decrease-size ()
  "Decrease font size for current buffer."
  (interactive)
  (text-scale-decrease 1))

(defun jylhis-fonts-reset-size ()
  "Reset font size for current buffer."
  (interactive)
  (text-scale-set 0))

(defun jylhis-fonts-info ()
  "Display information about current font configuration."
  (interactive)
  (let ((default-font (face-attribute 'default :family))
        (default-height (face-attribute 'default :height))
        (variable-font (face-attribute 'variable-pitch :family))
        (available-count (length (jylhis-fonts--get-available-families))))
    (message "Font: %s (height %d), Variable: %s, Available fonts: %d"
             default-font default-height variable-font available-count)))

;;; Global Keybindings

(global-set-key (kbd "C-+") #'jylhis-fonts-increase-size)
(global-set-key (kbd "C--") #'jylhis-fonts-decrease-size)
(global-set-key (kbd "C-0") #'jylhis-fonts-reset-size)
(global-set-key (kbd "C-c f i") #'jylhis-fonts-info)

;;; Initialize Font System

;; Setup fonts when this module loads
(jylhis-fonts-setup)

(provide 'fonts)
;;; fonts.el ends here
