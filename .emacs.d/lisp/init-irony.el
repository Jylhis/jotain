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
  (require-package 'company-irony-c-headers) ; TODO
  (when (maybe-require-package 'company-irony)
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony)))

  ;; flycheck-irony
  (when (maybe-require-package 'flycheck-irony)
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

    )
  )

;; FIXME
(defun my-company-irony ()
  (add-to-list 'company-backends 'company-irony))

(add-hook 'c-mode-hook #'my-company-irony)
(add-hook 'c++-mode-hook #'my-company-irony)

;; Company irony c headers
(defun company-c-head ()
  (add-to-list 'company-backends 'company-irony))

(add-hook 'c-mode-hook #'company-c-head)
(add-hook 'c++-mode-hook #'company-c-head)

(provide 'init-irony)
