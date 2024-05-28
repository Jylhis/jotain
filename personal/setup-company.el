(use-package diminish
  :straight (:host github :repo "emacsmirror/diminish" :files ("*.el"))
  :straight t)


(use-package company
  :straight t
  :config
  ;; No delay in showing suggestions.
(setq company-idle-delay 0.1
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-tooltip-align-annotations t
      company-tooltip-annotation-padding 1
      company-tooltip-flip-when-above t
      company-text-face-extra-attributes
      '(:weight bold :slant italic)
      )
      (add-hook 'after-init-hook 'global-company-mode)
      )

(diminish 'company-mode)
