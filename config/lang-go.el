;;; lang-go.el --- Go language configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Optimized Go development configuration with gopls LSP, goimports formatting,
;; and comprehensive test integration following Doom/Spacemacs patterns.

;;; Code:

(use-package go-mode
  :ensure
  :mode "\\.go\\'"
  :hook (go-mode . eglot-ensure)
  :init
  ;; Use goimports instead of gofmt for automatic import management
  (setq gofmt-command "goimports")
  :config
  ;; Format on save
  (add-hook 'before-save-hook
            (lambda ()
              (when (eq major-mode 'go-mode)
                (gofmt-before-save)))
            nil t)

  ;; gopls configuration for optimal hints and features
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((go-mode go-ts-mode) . ("gopls")))

    ;; Configure gopls with inlay hints
    (setq-default eglot-workspace-configuration
                  '(:gopls (:hints (:assignVariableTypes t
                                   :compositeLiteralFields t
                                   :constantValues t
                                   :functionTypeParameters t
                                   :parameterNames t
                                   :rangeVariableTypes t)
                           :staticcheck t
                           :usePlaceholders t))))

  ;; Test runner functions
  (defun my/go-test-current-test ()
    "Run the test function at point."
    (interactive)
    (let* ((test-method (if (string-match "_test\\.go" buffer-file-name)
                           (which-function)
                         nil)))
      (if test-method
          (compile (format "go test -v -run '^%s$'" test-method))
        (message "No test found at point"))))

  (defun my/go-test-current-file ()
    "Run all tests in the current file."
    (interactive)
    (if (string-match "_test\\.go" buffer-file-name)
        (compile (format "go test -v %s" (file-name-nondirectory buffer-file-name)))
      (message "Not in a test file")))

  (defun my/go-test-project ()
    "Run all tests in the project."
    (interactive)
    (compile "go test -v ./..."))

  (defun my/go-test-project-with-coverage ()
    "Run all tests with coverage report."
    (interactive)
    (compile "go test -v -coverprofile=coverage.out ./... && go tool cover -html=coverage.out"))

  (defun my/go-run-main ()
    "Run the main package."
    (interactive)
    (compile "go run ."))

  ;; Major-mode keybindings following Doom/Spacemacs patterns
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps 'go-mode-map
      "" '(:ignore t :which-key "go")

      ;; Goto operations
      "g" '(:ignore t :which-key "goto")
      "gg" '(xref-find-definitions :which-key "definition")
      "gG" '(xref-find-definitions-other-window :which-key "definition other")
      "gr" '(xref-find-references :which-key "references")
      "gi" '(eglot-find-implementation :which-key "implementation")
      "gd" '(xref-find-definitions :which-key "definition")
      "gb" '(xref-go-back :which-key "go back")
      "gf" '(xref-go-forward :which-key "go forward")

      ;; Help/Documentation
      "h" '(:ignore t :which-key "help")
      "hh" '(eldoc-doc-buffer :which-key "doc at point")

      ;; Imports
      "i" '(:ignore t :which-key "imports")
      "ia" '(go-import-add :which-key "add import")
      "ir" '(go-remove-unused-imports :which-key "remove unused")
      "ig" '(go-goto-imports :which-key "goto imports")

      ;; Testing
      "t" '(:ignore t :which-key "test")
      "tt" '(my/go-test-current-test :which-key "test at point")
      "tf" '(my/go-test-current-file :which-key "test file")
      "ta" '(my/go-test-project :which-key "test all")
      "tc" '(my/go-test-project-with-coverage :which-key "test with coverage")

      ;; Run
      "r" '(:ignore t :which-key "run")
      "rr" '(my/go-run-main :which-key "run main")

      ;; Format
      "=" '(gofmt :which-key "format buffer")
      "==" '(gofmt :which-key "format buffer")

      ;; Refactor
      "r" '(:ignore t :which-key "refactor")
      "rr" '(eglot-rename :which-key "rename")

      ;; Actions
      "a" '(:ignore t :which-key "actions")
      "aa" '(eglot-code-actions :which-key "code actions")
      "ao" '(eglot-code-action-organize-imports :which-key "organize imports")
      "aq" '(eglot-code-action-quickfix :which-key "quickfix"))))

(provide 'lang-go)
;;; lang-go.el ends here
