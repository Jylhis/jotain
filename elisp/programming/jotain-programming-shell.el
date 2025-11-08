;;; jotain-programming-shell.el --- Shell scripting support for Jotain -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Markus Jylhänkangas

;;; Commentary:
;; Shell scripting support for Bash, Zsh, and other shells with
;; LSP integration and linting.

;;; Code:

;; Enhanced shell scripting mode
(use-package sh-script
  :ensure nil
  :mode (("\\.sh\\'" . sh-mode)
         ("\\.bash\\'" . sh-mode)
         ("\\.zsh\\'" . sh-mode)
         ("\\.fish\\'" . fish-mode)
         ("bashrc\\'" . sh-mode)
         ("bash_profile\\'" . sh-mode)
         ("bash_aliases\\'" . sh-mode)
         ("zshrc\\'" . sh-mode)
         ("zshenv\\'" . sh-mode))
  :hook (sh-mode . eglot-ensure)
  :config
  (setq sh-basic-offset 2
        sh-indentation 2)
  
  ;; Add bash-language-server for LSP
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(sh-mode . ("bash-language-server" "start")))))

;; Fish shell mode
(use-package fish-mode
  :mode "\\.fish\\'")

;; Shell formatting
(use-package shfmt
  :defer t
  :hook (sh-mode . shfmt-on-save-mode))

;; Shellcheck integration via flymake
(use-package flymake-shellcheck
  :hook (sh-mode . flymake-shellcheck-load))

(provide 'jotain-programming-shell)
;;; jotain-programming-shell.el ends here
