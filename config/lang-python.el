;;; lang-python.el --- Python language configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Python development configuration with pyright/pylsp LSP, pytest integration,
;; virtual environment support, and comprehensive keybindings following
;; Doom/Spacemacs patterns.

;;; Code:

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :hook (python-mode . eglot-ensure)
  :custom
  ;; Python indentation
  (python-indent-offset 4)
  (python-indent-guess-indent-offset-verbose nil)

  ;; Use IPython if available
  (python-shell-interpreter "python3")
  (python-shell-interpreter-args "-i")

  :config
  ;; Configure LSP server (pyright or pylsp)
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(python-mode . ("pyright-langserver" "--stdio"))))

  ;; Test runner functions
  (defun my/python-test-file ()
    "Run pytest on current file."
    (interactive)
    (compile (format "pytest -v %s" (buffer-file-name))))

  (defun my/python-test-function ()
    "Run pytest on current function/class."
    (interactive)
    (let ((func-name (which-function)))
      (if func-name
          (compile (format "pytest -v %s::%s" (buffer-file-name) func-name))
        (message "No function found at point"))))

  (defun my/python-test-project ()
    "Run all pytest tests."
    (interactive)
    (compile "pytest -v"))

  (defun my/python-test-coverage ()
    "Run pytest with coverage."
    (interactive)
    (compile "pytest --cov=. --cov-report=html"))

  (defun my/python-run-file ()
    "Run current Python file."
    (interactive)
    (compile (format "python3 %s" (buffer-file-name))))

  (defun my/python-send-buffer-to-repl ()
    "Send buffer to Python REPL."
    (interactive)
    (python-shell-send-buffer))

  (defun my/python-send-region-to-repl ()
    "Send region to Python REPL."
    (interactive)
    (python-shell-send-region (region-beginning) (region-end)))

  ;; Major-mode keybindings following Doom/Spacemacs patterns
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps 'python-mode-map
      "" '(:ignore t :which-key "python")

      ;; Goto operations
      "g" '(:ignore t :which-key "goto")
      "gg" '(xref-find-definitions :which-key "definition")
      "gG" '(xref-find-definitions-other-window :which-key "definition other")
      "gr" '(xref-find-references :which-key "references")
      "gi" '(eglot-find-implementation :which-key "implementation")
      "gb" '(xref-go-back :which-key "go back")
      "gf" '(xref-go-forward :which-key "go forward")

      ;; Help/Documentation
      "h" '(:ignore t :which-key "help")
      "hh" '(eldoc-doc-buffer :which-key "doc at point")

      ;; REPL/Send operations
      "s" '(:ignore t :which-key "repl/send")
      "ss" '(run-python :which-key "start repl")
      "sb" '(my/python-send-buffer-to-repl :which-key "send buffer")
      "sr" '(my/python-send-region-to-repl :which-key "send region")
      "sf" '(python-shell-send-defun :which-key "send function")

      ;; Testing
      "t" '(:ignore t :which-key "test")
      "tt" '(my/python-test-function :which-key "test function")
      "tf" '(my/python-test-file :which-key "test file")
      "ta" '(my/python-test-project :which-key "test all")
      "tc" '(my/python-test-coverage :which-key "test with coverage")

      ;; Run
      "r" '(:ignore t :which-key "run")
      "rr" '(my/python-run-file :which-key "run file")

      ;; Virtual environment
      "v" '(:ignore t :which-key "venv")
      "va" '(pyvenv-activate :which-key "activate")
      "vd" '(pyvenv-deactivate :which-key "deactivate")
      "vw" '(pyvenv-workon :which-key "workon")

      ;; Imports
      "i" '(:ignore t :which-key "imports")
      "is" '(eglot-code-action-organize-imports :which-key "sort imports")
      "io" '(eglot-code-action-organize-imports :which-key "organize imports")

      ;; Format
      "=" '(eglot-format-buffer :which-key "format buffer")
      "==" '(eglot-format-buffer :which-key "format buffer")
      "=r" '(eglot-format :which-key "format region")

      ;; Refactor
      "r" '(:ignore t :which-key "refactor")
      "rr" '(eglot-rename :which-key "rename")

      ;; Actions
      "a" '(:ignore t :which-key "actions")
      "aa" '(eglot-code-actions :which-key "code actions")
      "aq" '(eglot-code-action-quickfix :which-key "quickfix"))))

;; Optional: pyvenv for virtual environment management
(use-package pyvenv
  :if (executable-find "python3")
  :config
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] "))))

(provide 'lang-python)
;;; lang-python.el ends here
