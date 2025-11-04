;;; lang-haskell.el --- Haskell language configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Haskell development configuration with haskell-language-server (HLS),
;; interactive REPL support, and comprehensive keybindings following
;; Doom/Spacemacs patterns.

;;; Code:

(defcustom my/haskell-backend 'eglot
  "Backend for Haskell IDE features."
  :type '(choice (const :tag "Eglot + HLS" eglot)
                 (const :tag "None" none))
  :group 'my-config)

(use-package haskell-mode
  :ensure
  :mode "\\.hs\\'"
  :hook ((haskell-mode . (lambda ()
                          (when (eq my/haskell-backend 'eglot)
                            (eglot-ensure))))
         (haskell-mode . interactive-haskell-mode))
  :custom
  ;; REPL configuration
  (haskell-process-type 'auto)  ; Auto-detect stack/cabal
  (haskell-interactive-popup-errors nil)
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)

  ;; Indentation
  (haskell-indentation-layout-offset 4)
  (haskell-indentation-left-offset 4)
  (haskell-indentation-starter-offset 4)

  :config
  ;; Configure HLS with Eglot
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((haskell-mode haskell-ts-mode) . ("haskell-language-server-wrapper" "--lsp"))))

  ;; Helper functions for common tasks
  (defun my/haskell-repl-load ()
    "Load current file in REPL."
    (interactive)
    (haskell-process-load-file))

  (defun my/haskell-repl-reload ()
    "Reload current file in REPL."
    (interactive)
    (haskell-process-reload))

  (defun my/haskell-type-at-point ()
    "Show type at point."
    (interactive)
    (haskell-process-do-type))

  (defun my/haskell-info-at-point ()
    "Show info at point."
    (interactive)
    (haskell-process-do-info))

  ;; Major-mode keybindings following Doom/Spacemacs patterns
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps 'haskell-mode-map
      "" '(:ignore t :which-key "haskell")

      ;; Goto operations
      "g" '(:ignore t :which-key "goto")
      "gg" '(xref-find-definitions :which-key "definition")
      "gG" '(xref-find-definitions-other-window :which-key "definition other")
      "gr" '(xref-find-references :which-key "references")
      "gi" '(haskell-navigate-imports :which-key "imports")
      "gb" '(xref-go-back :which-key "go back")
      "gf" '(xref-go-forward :which-key "go forward")

      ;; REPL/Send operations
      "s" '(:ignore t :which-key "repl")
      "ss" '(haskell-interactive-switch :which-key "switch to repl")
      "sb" '(my/haskell-repl-load :which-key "load file")
      "sr" '(my/haskell-repl-reload :which-key "reload")
      "st" '(my/haskell-type-at-point :which-key "type at point")
      "si" '(my/haskell-info-at-point :which-key "info at point")
      "se" '(haskell-interactive-mode-clear :which-key "clear repl")

      ;; Cabal/Stack operations
      "c" '(:ignore t :which-key "cabal/stack")
      "cc" '(haskell-process-cabal-build :which-key "build")
      "ct" '(haskell-process-cabal :which-key "test")
      "cx" '(haskell-process-cabal :which-key "run")

      ;; Help/Documentation
      "h" '(:ignore t :which-key "help")
      "hh" '(eldoc-doc-buffer :which-key "doc at point")
      "ht" '(my/haskell-type-at-point :which-key "type at point")
      "hi" '(my/haskell-info-at-point :which-key "info at point")
      "hd" '(haskell-mode-jump-to-def-or-tag :which-key "jump to definition")

      ;; Imports
      "i" '(:ignore t :which-key "imports")
      "ii" '(haskell-navigate-imports :which-key "navigate imports")
      "is" '(haskell-sort-imports :which-key "sort imports")
      "ia" '(haskell-import-add :which-key "add import")

      ;; Format
      "=" '(eglot-format-buffer :which-key "format buffer")
      "==" '(eglot-format-buffer :which-key "format buffer")

      ;; Refactor
      "r" '(:ignore t :which-key "refactor")
      "rr" '(eglot-rename :which-key "rename")

      ;; Actions
      "a" '(:ignore t :which-key "actions")
      "aa" '(eglot-code-actions :which-key "code actions")
      "aq" '(eglot-code-action-quickfix :which-key "quickfix"))))

;; Tree-sitter mode if available
(use-package haskell-ts-mode
  :ensure
  :mode "\\.hs\\'"
  :when (treesit-available-p))

(provide 'lang-haskell)
;;; lang-haskell.el ends here
