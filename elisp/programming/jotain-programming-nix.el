;;; jotain-programming-nix.el --- Nix language support for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Nix language support with LSP integration using nil.

;;; Code:

;; Nix mode with LSP and formatting
(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure)
  :config
  ;; Add nil LSP server for Nix
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(nix-mode . ("nil"))))
  ;; Nix formatting
  (setq nix-nixfmt-bin "nixfmt"))

(provide 'jotain-programming-nix)
;;; jotain-programming-nix.el ends here
