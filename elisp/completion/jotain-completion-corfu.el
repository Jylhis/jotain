;;; jotain-completion-corfu.el --- In-buffer completion for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Corfu provides a modern in-buffer completion popup.
;; Works seamlessly with LSP, dabbrev, and other completion backends.

;;; Code:

(use-package corfu
  :ensure
  :demand t  ; Load immediately to ensure functions are available
  :custom
  ;; Enable cycling for next/previous completion candidate
  (corfu-cycle t)
  ;; Preselect the first candidate
  (corfu-preselect 'prompt)
  ;; Preview current candidate
  (corfu-preview-current 'insert)

  :config
  ;; Enable Corfu globally for all modes
  ;; Use `global-corfu-modes' to exclude certain modes if needed
  (global-corfu-mode)

  ;; Enable optional extension modes (these are global, require extensions first)
  (require 'corfu-history)
  (corfu-history-mode)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode))

;; Configure corfu-popupinfo settings
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2)))

;; Cape - additional completion backends
(use-package cape
  :ensure
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
  (cape-dabbrev-min-length 3))

(provide 'jotain-completion-corfu)
;;; jotain-completion-corfu.el ends here
