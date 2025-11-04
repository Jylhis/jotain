;;; lang-nix.el --- Nix language configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Nix development configuration with nil/nixd LSP, nixfmt formatting,
;; and comprehensive keybindings following Doom/Spacemacs patterns.

;;; Code:

(use-package nix-mode
  :ensure
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    ;; Prefer nil LSP server, fallback to nixd or rnix-lsp
    (add-to-list 'eglot-server-programs
                 '(nix-mode . ("nil"))))

  ;; Helper functions
  (defun my/nix-build ()
    "Build current Nix expression."
    (interactive)
    (compile "nix build"))

  (defun my/nix-build-flake ()
    "Build current flake."
    (interactive)
    (compile "nix build ."))

  (defun my/nix-develop ()
    "Enter nix develop shell."
    (interactive)
    (compile "nix develop"))

  (defun my/nix-flake-check ()
    "Run nix flake check."
    (interactive)
    (compile "nix flake check"))

  (defun my/nix-flake-update ()
    "Update flake lock file."
    (interactive)
    (compile "nix flake update"))

  (defun my/nix-format-buffer ()
    "Format buffer with nixfmt."
    (interactive)
    (if (executable-find "nixfmt")
        (eglot-format-buffer)
      (message "nixfmt not found")))

  ;; Major-mode keybindings following Doom/Spacemacs patterns
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps '(nix-mode-map nix-ts-mode-map)
      "" '(:ignore t :which-key "nix")

      ;; Goto operations
      "g" '(:ignore t :which-key "goto")
      "gg" '(xref-find-definitions :which-key "definition")
      "gG" '(xref-find-definitions-other-window :which-key "definition other")
      "gr" '(xref-find-references :which-key "references")
      "gb" '(xref-go-back :which-key "go back")
      "gf" '(xref-go-forward :which-key "go forward")

      ;; Help/Documentation
      "h" '(:ignore t :which-key "help")
      "hh" '(eldoc-doc-buffer :which-key "doc at point")

      ;; Build operations
      "b" '(:ignore t :which-key "build")
      "bb" '(my/nix-build :which-key "build")
      "bf" '(my/nix-build-flake :which-key "build flake")
      "bd" '(my/nix-develop :which-key "develop shell")

      ;; Flake operations
      "f" '(:ignore t :which-key "flake")
      "fc" '(my/nix-flake-check :which-key "check")
      "fu" '(my/nix-flake-update :which-key "update")
      "fs" '((lambda () (interactive) (compile "nix flake show")) :which-key "show")

      ;; Eval operations
      "e" '(:ignore t :which-key "eval")
      "ee" '((lambda () (interactive)
               (compile (format "nix eval -f %s" (buffer-file-name))))
             :which-key "eval file")

      ;; Format
      "=" '(my/nix-format-buffer :which-key "format buffer")
      "==" '(my/nix-format-buffer :which-key "format buffer")

      ;; Refactor
      "r" '(:ignore t :which-key "refactor")
      "rr" '(eglot-rename :which-key "rename")

      ;; Actions
      "a" '(:ignore t :which-key "actions")
      "aa" '(eglot-code-actions :which-key "code actions")
      "aq" '(eglot-code-action-quickfix :which-key "quickfix"))))

;; Tree-sitter mode if available
(use-package nix-ts-mode
  :ensure
  :mode "\\.nix\\'"
  :when (treesit-available-p))

(provide 'lang-nix)
;;; lang-nix.el ends here
