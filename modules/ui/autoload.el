;;; ui/autoload.el --- UI autoloaded functions -*- lexical-binding: t; -*-
;;; Commentary:
;; Autoloaded utility functions for UI module.
;;; Code:

;;;###autoload
(defun +ui/pulse-line (&rest _)
  "Pulse the current line for visual feedback."
  (pulse-momentary-highlight-one-line (point)))

;;;###autoload
(defun +ui/setup-org-fonts ()
  "Configure mixed fonts for org-mode - variable pitch for text, fixed for code."
  (interactive)
  ;; Enable variable pitch mode for better prose readability
  (variable-pitch-mode 1)

  ;; Keep code elements in fixed-pitch font
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block-end-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-special-keyword nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-property-value nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-tag nil :inherit 'fixed-pitch)

  ;; Better visual hierarchy with font sizes
  (set-face-attribute 'org-document-title nil :height 1.3 :weight 'bold)
  (set-face-attribute 'org-level-1 nil :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.1 :weight 'semi-bold)
  (set-face-attribute 'org-level-3 nil :height 1.05)

  ;; Subtle styling for meta elements
  (set-face-attribute 'org-meta-line nil :height 0.9 :slant 'italic)
  (set-face-attribute 'org-drawer nil :height 0.9 :slant 'italic))

(provide 'ui/autoload)
;;; ui/autoload.el ends here
