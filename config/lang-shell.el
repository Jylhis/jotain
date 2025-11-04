;;; lang-shell.el --- Shell script configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Shell script development configuration for Bash and PowerShell with
;; bash-language-server and comprehensive keybindings following
;; Doom/Spacemacs patterns.

;;; Code:

(use-package sh-script
  :mode (("\\.sh\\'" . bash-ts-mode)
         ("\\.bash\\'" . bash-ts-mode)
         (".bashrc" . bash-ts-mode)
         (".bash_profile" . bash-ts-mode)
         (".bash_aliases" . bash-ts-mode)
         ("\\.zsh\\'" . sh-mode)
         (".zshrc" . sh-mode))
  :hook (sh-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(sh-mode . ("bash-language-server" "start"))))

  ;; Helper functions
  (defun my/shell-run-file ()
    "Run current shell script."
    (interactive)
    (compile (format "bash %s" (buffer-file-name))))

  (defun my/shell-check-syntax ()
    "Check shell script syntax with shellcheck."
    (interactive)
    (compile (format "shellcheck %s" (buffer-file-name))))

  ;; Major-mode keybindings following Doom/Spacemacs patterns
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps '(sh-mode-map bash-ts-mode-map)
      "" '(:ignore t :which-key "shell")

      ;; Help/Documentation
      "h" '(:ignore t :which-key "help")
      "hh" '(eldoc-doc-buffer :which-key "doc at point")
      "hm" '(man :which-key "man page")

      ;; Run
      "r" '(:ignore t :which-key "run")
      "rr" '(my/shell-run-file :which-key "run file")

      ;; Check
      "c" '(:ignore t :which-key "check")
      "cc" '(my/shell-check-syntax :which-key "shellcheck")

      ;; Format
      "=" '(eglot-format-buffer :which-key "format buffer")
      "==" '(eglot-format-buffer :which-key "format buffer")

      ;; Actions
      "a" '(:ignore t :which-key "actions")
      "aa" '(eglot-code-actions :which-key "code actions")
      "aq" '(eglot-code-action-quickfix :which-key "quickfix"))))

;; PowerShell support
(use-package powershell
  :if (locate-library "powershell")
  :mode (("\\.ps1\\'" . powershell-mode)
         ("\\.psm1\\'" . powershell-mode))
  :config
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps 'powershell-mode-map
      "" '(:ignore t :which-key "powershell")

      ;; Run
      "r" '(:ignore t :which-key "run")
      "rr" '((lambda () (interactive)
               (compile (format "powershell -File %s" (buffer-file-name))))
             :which-key "run file")

      ;; REPL
      "s" '(:ignore t :which-key "repl")
      "ss" '(powershell :which-key "start repl"))))

(provide 'lang-shell)
;;; lang-shell.el ends here
