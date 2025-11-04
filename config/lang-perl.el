;;; lang-perl.el --- Perl language configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Perl development configuration with Perl Navigator LSP and comprehensive
;; keybindings following Doom/Spacemacs patterns.

;;; Code:

(use-package perl-mode
  :mode (("\\.pl\\'" . perl-mode)
         ("\\.pm\\'" . perl-mode)
         ("\\.t\\'" . perl-mode))
  :hook (perl-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(perl-mode . ("perlnavigator" "--stdio"))))

  ;; Helper functions
  (defun my/perl-run-file ()
    "Run current Perl file."
    (interactive)
    (compile (format "perl %s" (buffer-file-name))))

  (defun my/perl-test-file ()
    "Run prove on current test file."
    (interactive)
    (compile (format "prove -v %s" (buffer-file-name))))

  (defun my/perl-test-project ()
    "Run all tests with prove."
    (interactive)
    (compile "prove -r t/"))

  (defun my/perl-syntax-check ()
    "Check Perl syntax."
    (interactive)
    (compile (format "perl -c %s" (buffer-file-name))))

  ;; Major-mode keybindings following Doom/Spacemacs patterns
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps 'perl-mode-map
      "" '(:ignore t :which-key "perl")

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
      "hp" '(cperl-perldoc :which-key "perldoc")

      ;; Testing
      "t" '(:ignore t :which-key "test")
      "tt" '(my/perl-test-file :which-key "test file")
      "ta" '(my/perl-test-project :which-key "test all")

      ;; Run
      "r" '(:ignore t :which-key "run")
      "rr" '(my/perl-run-file :which-key "run file")

      ;; Check
      "c" '(:ignore t :which-key "check")
      "cc" '(my/perl-syntax-check :which-key "syntax check")

      ;; Format
      "=" '(eglot-format-buffer :which-key "format buffer")
      "==" '(eglot-format-buffer :which-key "format buffer")

      ;; Actions
      "a" '(:ignore t :which-key "actions")
      "aa" '(eglot-code-actions :which-key "code actions")
      "aq" '(eglot-code-action-quickfix :which-key "quickfix"))))

(provide 'lang-perl)
;;; lang-perl.el ends here
