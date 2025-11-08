;;; jotain-ui-themes.el --- Theme configuration for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Theme configuration for Jotain.  Uses modus-themes by default (built-in
;; to Emacs 30) with fallback to doom-themes if preferred.

;;; Code:

;; Use modus-themes (built-in to Emacs 30)
(use-package modus-themes
  :ensure nil
  :config
  ;; Configure modus-themes appearance
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-custom-auto-reload t
        modus-themes-disable-other-themes t
        
        ;; Org mode specific
        modus-themes-org-blocks 'gray-background
        modus-themes-headings '((1 . (1.3))
                                (2 . (1.2))
                                (3 . (1.1))
                                (t . (1.0))))
  
  ;; Load modus-vivendi (dark) theme by default
  (load-theme 'modus-vivendi t))

;; Optional: Configure doom-themes if available
;; Uncomment to use doom-themes instead
;; (use-package doom-themes
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   (load-theme 'doom-one t)
;;   (doom-themes-org-config))

(provide 'jotain-ui-themes)
;;; jotain-ui-themes.el ends here
