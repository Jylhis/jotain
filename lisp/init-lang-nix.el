;;; init-lang-nix.el --- Nix language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Nix is the package manager and dev-shell tool this whole config is
;; built around, so it earns its own language file even though we don't
;; write much of it day to day. The eglot hook is registered in
;; `init-prog' so all language servers are visible from one place.

;;; Code:

(use-package nix-ts-mode
  :ensure nil ; Provided by Nix
  :mode "\\.nix\\'")

;; Format-on-save is handled by apheleia (configured in init-prog.el).
;; apheleia ships a default Nix entry that uses nixfmt.

(provide 'init-lang-nix)
;;; init-lang-nix.el ends here
