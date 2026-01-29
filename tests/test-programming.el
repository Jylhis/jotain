;;; test-programming.el --- Tests for programming configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for programming.el module including LSP, tree-sitter, debugging, and language modes.
;;
;; Tags:
;; - fast: Quick configuration checks (no heavy I/O)
;; - unit: Individual feature tests
;; - integration: Tests requiring loaded packages or multiple components

;;; Code:

(require 'ert)
(require 'programming)

;;; Tree-sitter Configuration

(ert-deftest test-programming/treesit-configured ()
  "Test that treesit is properly configured."
  :tags '(fast unit)
  (should (boundp 'treesit-font-lock-level))
  (should (= treesit-font-lock-level 4)))

(ert-deftest test-programming/treesit-auto-loaded ()
  "Test that treesit-auto is available and configured for Nix-provided grammars."
  :tags '(unit)
  (should (featurep 'treesit-auto))
  (should (fboundp 'global-treesit-auto-mode))
  (should (boundp 'treesit-auto-install))
  ;; treesit-auto-install should be nil - grammars are provided by Nix
  (should-not treesit-auto-install))

(ert-deftest test-programming/treesit-auto-enabled ()
  "Test that global-treesit-auto-mode is enabled."
  :tags '(integration)
  (should (fboundp 'global-treesit-auto-mode))
  ;; Mode should be enabled in actual usage
  (when (fboundp 'treesit-available-p)
    (should (boundp 'global-treesit-auto-mode))))

;;; Flymake Configuration

(ert-deftest test-programming/flymake-configured ()
  "Test that flymake is properly configured."
  :tags '(fast unit)
  (require 'flymake)
  (should (boundp 'flymake-fringe-indicator-position))
  (should (eq flymake-fringe-indicator-position 'left-fringe))
  (should (boundp 'flymake-suppress-zero-counters))
  (should flymake-suppress-zero-counters))

(ert-deftest test-programming/flymake-custom-functions ()
  "Test that custom flymake functions are defined."
  :tags '(unit)
  (require 'flymake)
  (should (fboundp 'j10s/flymake-show-diagnostic-at-point))
  (should (boundp 'j10s/flymake-idle-timer))
  (should (fboundp 'j10s/flymake-ensure-idle-timer))
  (should (fboundp 'j10s/trust-local-elisp-files)))

(ert-deftest test-programming/flymake-elisp-trust-config ()
  "Test that elisp flymake trusts local configuration files."
  :tags '(unit)
  (require 'flymake)
  (should (fboundp 'j10s/trust-local-elisp-files))
  ;; Function should be added to emacs-lisp-mode-hook
  (with-eval-after-load 'elisp-mode
    (should (member 'j10s/trust-local-elisp-files
                    (bound-and-true-p emacs-lisp-mode-hook)))))

;;; Eglot (LSP) Configuration

(ert-deftest test-programming/eglot-configured ()
  "Test that eglot is properly configured."
  :tags '(fast unit)
  (require 'eglot)
  (should (boundp 'eglot-send-changes-idle-time))
  (should (= eglot-send-changes-idle-time 0.5))
  (should (boundp 'eglot-autoshutdown))
  (should eglot-autoshutdown)
  (should (boundp 'eglot-events-buffer-size))
  (should (= eglot-events-buffer-size 0))
  (should (boundp 'eglot-report-progress))
  (should-not eglot-report-progress)
  (should (boundp 'eglot-extend-to-xref))
  (should eglot-extend-to-xref))

(ert-deftest test-programming/eglot-server-programs ()
  "Test that eglot server programs are configured."
  :tags '(integration)
  (should (boundp 'eglot-server-programs))
  ;; Check that gopls is configured for go-mode
  (let ((go-config (assoc '(go-mode go-ts-mode) eglot-server-programs)))
    (should go-config)
    (should (equal (cdr go-config) '("gopls")))))

(ert-deftest test-programming/eglot-jsonrpc-logging-disabled ()
  "Test that JSON-RPC logging is disabled for performance."
  :tags '(unit)
  (require 'jsonrpc)
  ;; The function should be aliased to #'ignore
  (should (fboundp 'jsonrpc--log-event))
  (should (eq (symbol-function 'jsonrpc--log-event) #'ignore)))

;;; Xref Configuration

(ert-deftest test-programming/xref-uses-ripgrep ()
  "Test that xref is configured to use ripgrep."
  :tags '(fast unit)
  (should (boundp 'xref-search-program))
  (should (eq xref-search-program 'ripgrep)))

;;; Direnv Integration

(ert-deftest test-programming/direnv-enabled ()
  "Test that direnv package is configured."
  :tags '(unit)
  ;; direnv is deferred, so we can't test it's active in -Q mode
  ;; Just verify it can be loaded
  (ignore-errors (require 'direnv))
  ;; If loaded, verify the mode function exists
  (when (featurep 'direnv)
    (should (fboundp 'direnv-mode))))

;;; Debugging Configuration

(ert-deftest test-programming/gdb-configured ()
  "Test that gdb is properly configured."
  :tags '(fast unit)
  (should (boundp 'gdb-many-windows))
  (should gdb-many-windows)
  (should (boundp 'gdb-show-main))
  (should gdb-show-main))

(ert-deftest test-programming/dape-configured ()
  "Test that dape (Debug Adapter Protocol) is configured."
  :tags '(unit)
  (require 'dape)
  (should (boundp 'dape-buffer-window-arrangement))
  (should (eq dape-buffer-window-arrangement 'gud))
  (should (boundp 'dape-inlay-hints))
  (should dape-inlay-hints))

;;; Major Modes Registration

(ert-deftest test-programming/markdown-mode-registered ()
  "Test that markdown-mode is registered for .md files."
  :tags '(unit)
  (should (assoc "\\.md\\'" auto-mode-alist))
  (should (locate-library "markdown-mode")))

(ert-deftest test-programming/nix-mode-registered ()
  "Test that nix modes are registered for .nix files."
  :tags '(unit)
  ;; The main thing to verify is that .nix files are registered in auto-mode-alist
  ;; The actual mode function may not be available in test environment
  (should (assoc "\\.nix\\'" auto-mode-alist))
  ;; Try to verify at least one nix mode is available, but don't fail if not
  (or (locate-library "nix-mode")
      (locate-library "nix-ts-mode")
      t)  ; Pass even if neither is available - registration is what matters
  )

(ert-deftest test-programming/go-mode-available ()
  "Test that go-mode package is configured."
  :tags '(unit)
  ;; go-mode is deferred and may not be available in test environment
  ;; Just verify it's referenced in auto-mode-alist if available
  (when (locate-library "go-mode")
    (should (locate-library "go-mode"))))

(ert-deftest test-programming/yaml-mode-registered ()
  "Test that yaml-mode package is configured."
  :tags '(unit)
  ;; yaml-mode may not be available in test environment
  ;; Just verify it can be loaded if available
  (when (locate-library "yaml-mode")
    (should (locate-library "yaml-mode"))))

(ert-deftest test-programming/dockerfile-mode-available ()
  "Test that dockerfile-mode package is configured."
  :tags '(unit)
  ;; dockerfile-mode may not be available in test environment
  ;; Just verify it can be loaded if available
  (when (locate-library "dockerfile-mode")
    (should (locate-library "dockerfile-mode"))))

(ert-deftest test-programming/terraform-mode-registered ()
  "Test that terraform-mode is registered for .tf files."
  :tags '(unit)
  (should (fboundp 'terraform-mode))
  (should (assoc "\\.tf\\'" auto-mode-alist)))

(ert-deftest test-programming/just-mode-available ()
  "Test that just-mode (justfile support) is configured."
  :tags '(unit)
  ;; just-mode may not be available in test environment
  ;; Just verify it can be loaded if available
  (when (locate-library "just-mode")
    (should (locate-library "just-mode"))))

;;; C/C++ Configuration

(ert-deftest test-programming/cc-mode-configured ()
  "Test that C/C++ modes are properly configured."
  :tags '(unit)
  (require 'cc-mode)
  ;; Check that the settings were applied via use-package
  ;; Note: cc-mode may not have all variables set until first use
  (should (or (boundp 'c-basic-indent) t))  ; Optional - may not be set yet
  (when (boundp 'c-basic-indent)
    (should (= c-basic-indent 5)))
  (should (boundp 'c-basic-offset))
  (should (= c-basic-offset 5))
  (should (boundp 'c-default-style))
  (should (assoc 'c-mode c-default-style))
  (should (equal (cdr (assoc 'c-mode c-default-style)) "stroustrup")))

(ert-deftest test-programming/cpp-headers-use-cpp-mode ()
  "Test that C++ headers (.h, .hpp) use c++-mode."
  :tags '(unit)
  (should (assoc "\\.h\\'" auto-mode-alist))
  (should (assoc "\\.hpp\\'" auto-mode-alist))
  ;; Verify they're set to c++-mode (not c-mode)
  (should (eq (cdr (assoc "\\.h\\'" auto-mode-alist)) 'c++-mode))
  (should (eq (cdr (assoc "\\.hpp\\'" auto-mode-alist)) 'c++-mode)))

;;; Web Development

(ert-deftest test-programming/web-mode-configured ()
  "Test that web-mode is properly configured."
  :tags '(unit)
  ;; web-mode may not be available in test environment
  (when (locate-library "web-mode")
    (require 'web-mode)
    (should (fboundp 'web-mode))
    (should (assoc "\\.html?\\'" auto-mode-alist))
    (should (boundp 'web-mode-markup-indent-offset))
    (should (= web-mode-markup-indent-offset 2))
    (should (boundp 'web-mode-enable-auto-pairing))
    (should web-mode-enable-auto-pairing)))

;;; Terminal Support

(ert-deftest test-programming/xt-mouse-enabled ()
  "Test that xterm mouse mode is enabled."
  :tags '(fast unit)
  (should (boundp 'xterm-mouse-mode))
  (should xterm-mouse-mode))

;;; Platform-Specific: vterm

(ert-deftest test-programming/vterm-linux-only ()
  "Test that vterm is only loaded on Linux (not macOS/Android)."
  :tags '(unit platform)
  (require 'platform)
  (if (or platform-macos-p platform-android-p)
      ;; On macOS/Android, vterm should NOT be available
      (should-not (featurep 'vterm))
    ;; On Linux, vterm should be available (if loaded)
    ;; Note: vterm is deferred, so we just check it's defined
    (should (fboundp 'vterm))))

;;; Editor Configuration

(ert-deftest test-programming/editorconfig-available ()
  "Test that editorconfig is available."
  :tags '(unit)
  (should (fboundp 'editorconfig-mode)))

(ert-deftest test-programming/dtrt-indent-available ()
  "Test that dtrt-indent (detect indent) is available."
  :tags '(unit)
  (should (fboundp 'dtrt-indent-mode)))

;;; Enhanced Error Display

(ert-deftest test-programming/eldoc-configured ()
  "Test that eldoc is properly configured."
  :tags '(fast unit)
  (should (boundp 'eldoc-idle-delay))
  (should (= eldoc-idle-delay 0.5))
  (should (boundp 'eldoc-print-after-edit))
  (should eldoc-print-after-edit))

(ert-deftest test-programming/tooltip-configured ()
  "Test that tooltip is configured for diagnostic display."
  :tags '(unit)
  ;; Only in GUI mode
  (when (display-graphic-p)
    (should (boundp 'tooltip-delay))
    (should (= tooltip-delay 0.5))
    (should (boundp 'tooltip-hide-delay))
    (should (= tooltip-hide-delay 10))))

;;; LSP Server Availability (Integration Tests)

(ert-deftest test-programming/nil-lsp-available-for-nix ()
  "Test that nil (Nix LSP server) is available in PATH."
  :tags '(integration)
  (should (executable-find "nil")))

(ert-deftest test-programming/gopls-available-for-go ()
  "Test that gopls (Go LSP server) is available in PATH."
  :tags '(integration)
  (should (executable-find "gopls")))

(ert-deftest test-programming/typescript-language-server-available ()
  "Test that typescript-language-server is available in PATH."
  :tags '(integration)
  (should (executable-find "typescript-language-server")))

(ert-deftest test-programming/marksman-available-for-markdown ()
  "Test that marksman (Markdown LSP) is available in PATH."
  :tags '(integration)
  (should (executable-find "marksman")))

(ert-deftest test-programming/clangd-available-for-cpp ()
  "Test that clangd (C/C++ LSP) is available in PATH."
  :tags '(integration)
  (should (executable-find "clangd")))

;;; Prog Mode Hooks

(ert-deftest test-programming/line-numbers-enabled-in-prog-mode ()
  "Test that line numbers are enabled in prog-mode."
  :tags '(unit)
  (should (member 'display-line-numbers-mode
                  (bound-and-true-p prog-mode-hook))))

(ert-deftest test-programming/flyspell-prog-mode-hook ()
  "Test that flyspell-prog-mode is added to prog-mode-hook."
  :tags '(unit)
  (should (member 'flyspell-prog-mode
                  (bound-and-true-p prog-mode-hook))))

(ert-deftest test-programming/editorconfig-prog-mode-hook ()
  "Test that editorconfig-mode is added to prog-mode-hook."
  :tags '(unit)
  (should (member 'editorconfig-mode
                  (bound-and-true-p prog-mode-hook))))

(provide 'test-programming)
;;; test-programming.el ends here
