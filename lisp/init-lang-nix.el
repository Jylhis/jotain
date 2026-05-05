;;; init-lang-nix.el --- Nix language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Nix is the package manager and dev-shell tool this whole config is
;; built around, so it earns its own language file even though we don't
;; write much of it day to day. The eglot hook is registered in
;; `init-prog' so all language servers are visible from one place.

;;; Code:

;;; @doc Tree-sitter Nix major mode. Nix is the package manager and
;;; dev-shell tool the whole config is built around, so it gets a
;;; dedicated module even though we don't write much of it daily.
;;; Provided by Nix; format-on-save flows through apheleia →
;;; nixfmt (configured in init-prog).
(use-package nix-ts-mode
  :ensure nil ; Provided by Nix
  :mode "\\.nix\\'")

(provide 'init-lang-nix)
;;; init-lang-nix.el ends here
