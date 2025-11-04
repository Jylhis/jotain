;;; lang-ruby.el --- Ruby language configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Ruby development configuration with solargraph LSP, RSpec integration,
;; interactive REPL support, and comprehensive keybindings following
;; Doom/Spacemacs patterns.

;;; Code:

(use-package ruby-mode
  :mode (("\\.rb\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("Gemfile\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.gemspec\\'" . ruby-mode))
  :hook (ruby-mode . eglot-ensure)
  :custom
  (ruby-insert-encoding-magic-comment nil)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(ruby-mode . ("solargraph" "stdio"))))

  ;; Test runner functions
  (defun my/ruby-test-file ()
    "Run RSpec on current file."
    (interactive)
    (compile (format "rspec %s" (buffer-file-name))))

  (defun my/ruby-test-function ()
    "Run RSpec on current example/context."
    (interactive)
    (let ((line (line-number-at-pos)))
      (compile (format "rspec %s:%d" (buffer-file-name) line))))

  (defun my/ruby-test-project ()
    "Run all RSpec tests."
    (interactive)
    (compile "rspec"))

  (defun my/ruby-run-file ()
    "Run current Ruby file."
    (interactive)
    (compile (format "ruby %s" (buffer-file-name))))

  ;; Major-mode keybindings following Doom/Spacemacs patterns
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps 'ruby-mode-map
      "" '(:ignore t :which-key "ruby")

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

      ;; REPL/Send operations
      "s" '(:ignore t :which-key "repl/send")
      "ss" '(inf-ruby :which-key "start repl")
      "sb" '(ruby-send-buffer :which-key "send buffer")
      "sr" '(ruby-send-region :which-key "send region")
      "sd" '(ruby-send-definition :which-key "send definition")

      ;; Testing (RSpec)
      "t" '(:ignore t :which-key "test")
      "tt" '(my/ruby-test-function :which-key "test at point")
      "tf" '(my/ruby-test-file :which-key "test file")
      "ta" '(my/ruby-test-project :which-key "test all")

      ;; Run
      "r" '(:ignore t :which-key "run")
      "rr" '(my/ruby-run-file :which-key "run file")

      ;; Bundle operations
      "b" '(:ignore t :which-key "bundle")
      "bi" '((lambda () (interactive) (compile "bundle install")) :which-key "install")
      "bu" '((lambda () (interactive) (compile "bundle update")) :which-key "update")
      "be" '((lambda () (interactive) (compile "bundle exec ")) :which-key "exec")

      ;; Format
      "=" '(eglot-format-buffer :which-key "format buffer")
      "==" '(eglot-format-buffer :which-key "format buffer")

      ;; Refactor
      "R" '(:ignore t :which-key "refactor")
      "Rr" '(eglot-rename :which-key "rename")

      ;; Actions
      "a" '(:ignore t :which-key "actions")
      "aa" '(eglot-code-actions :which-key "code actions")
      "aq" '(eglot-code-action-quickfix :which-key "quickfix"))))

(provide 'lang-ruby)
;;; lang-ruby.el ends here
