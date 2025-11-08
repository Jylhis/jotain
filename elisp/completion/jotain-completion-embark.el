;;; jotain-completion-embark.el --- Embark actions for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Embark provides contextual actions on completion candidates.
;; Think of it as a right-click context menu for the minibuffer.

;;; Code:

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Show the Embark target at point via Eldoc
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult integration with Embark
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :bind (:map minibuffer-mode-map
              ("C-c C-o" . embark-export)))

(provide 'jotain-completion-embark)
;;; jotain-completion-embark.el ends here
