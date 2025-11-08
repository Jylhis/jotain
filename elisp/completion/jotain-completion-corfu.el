;;; jotain-completion-corfu.el --- In-buffer completion for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Corfu provides a modern in-buffer completion popup.
;; Works seamlessly with LSP, dabbrev, and other completion backends.

;;; Code:

(use-package corfu
  :ensure
  :init
  (global-corfu-mode)
  (corfu-history-mode))

;; Add extensions
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :config
  (corfu-popupinfo-mode))

;; Cape - additional completion backends
(use-package cape
  :after corfu
  :config
  ;; Add useful completion-at-point functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :custom
  (cape-file-directory-must-exist t)
  (cape-file-prefix '("~" "/" "./" "../"))
  (cape-dabbrev-min-length 3)
  )

(provide 'jotain-completion-corfu)
;;; jotain-completion-corfu.el ends here
