;; Packages
;; irony-mode
;; company-irony
;; flycheck-irony
;; TODO: company-irony-c-headers

(when (maybe-require-package 'irony)

  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)

  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  ;; Company-irony
  (when (maybe-require-package 'company-irony)
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony)))

  ;; flycheck-irony
  (when (maybe-require-package 'flycheck-irony)
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

    )
  )

(provide 'init-irony)
