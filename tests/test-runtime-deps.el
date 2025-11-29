;;; test-runtime-deps.el --- Tests for runtime dependencies -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for runtime dependencies (tree-sitter grammars, LSP servers, CLI tools).
;; These tests verify that external dependencies are properly configured and available.
;;
;; Tags:
;; - smoke: Critical runtime dependency checks (ultra-fast)
;; - fast: PATH and environment checks (no heavy I/O)
;; - unit: Individual tool availability tests
;; - integration: Grammar loading and LSP server availability

;;; Code:

(require 'ert)

;;; Tree-Sitter Environment

(ert-deftest test-runtime-deps/tree-sitter-dir-set ()
  "Test that TREE_SITTER_DIR environment variable is set."
  :tags '(smoke critical fast)
  (let ((ts-dir (getenv "TREE_SITTER_DIR")))
    (should ts-dir)
    (should (stringp ts-dir))
    (should (not (string-empty-p ts-dir)))))

(ert-deftest test-runtime-deps/tree-sitter-dir-exists ()
  "Test that TREE_SITTER_DIR points to an existing directory."
  :tags '(fast unit)
  (let ((ts-dir (getenv "TREE_SITTER_DIR")))
    (when ts-dir
      (should (file-directory-p ts-dir))
      (should (file-accessible-directory-p ts-dir)))))

(ert-deftest test-runtime-deps/tree-sitter-grammars-present ()
  "Test that tree-sitter grammar files exist in TREE_SITTER_DIR."
  :tags '(unit)
  (let ((ts-dir (getenv "TREE_SITTER_DIR")))
    ;; Changed from skip-unless to should: TREE_SITTER_DIR must be set
    (should ts-dir)
    (should (file-directory-p ts-dir))

    ;; Check for at least some common grammars
    (let ((grammar-files (directory-files ts-dir t "\\.so$")))
      (should (> (length grammar-files) 0))

      ;; Look for specific critical grammars
      (let ((files-string (mapconcat #'identity grammar-files " ")))
        ;; At least one of these should exist
        (should (or (string-match-p "libtree-sitter-nix\\.so" files-string)
                    (string-match-p "libtree-sitter-bash\\.so" files-string)
                    (string-match-p "libtree-sitter-python\\.so" files-string)))))))

(ert-deftest test-runtime-deps/tree-sitter-common-grammars ()
  "Test that commonly used tree-sitter grammars are available."
  :tags '(integration)
  (let ((ts-dir (getenv "TREE_SITTER_DIR")))
    ;; Changed from skip-unless to should: TREE_SITTER_DIR must be set
    (should ts-dir)
    (should (file-directory-p ts-dir))

    ;; List of critical grammars we expect to have
    (let ((expected-grammars '("nix" "bash" "python" "json" "yaml")))
      (dolist (lang expected-grammars)
        (let ((grammar-file (expand-file-name
                             (concat "libtree-sitter-" lang ".so")
                             ts-dir)))
          ;; Changed from skip to should: grammars must exist
          (should (file-exists-p grammar-file)))))))

;;; CLI Tools in PATH

(ert-deftest test-runtime-deps/ripgrep-available ()
  "Test that ripgrep (rg) is available in PATH."
  :tags '(smoke critical fast)
  (should (executable-find "rg")))

(ert-deftest test-runtime-deps/fd-available ()
  "Test that fd is available in PATH."
  :tags '(fast unit)
  (should (executable-find "fd")))

;; NOTE: git is NOT tested here as it's a system prerequisite, not a bundled
;; runtime dependency. Users should manage git separately via programs.git or
;; home.packages. See CLAUDE.md "System Prerequisites" section.

(ert-deftest test-runtime-deps/direnv-available ()
  "Test that direnv is available in PATH."
  :tags '(fast unit)
  (should (executable-find "direnv")))

(ert-deftest test-runtime-deps/cli-tools-functional ()
  "Test that CLI tools can actually execute."
  :tags '(unit)
  ;; Test ripgrep
  (when (executable-find "rg")
    (should (= 0 (call-process "rg" nil nil nil "--version"))))

  ;; Test fd
  (when (executable-find "fd")
    (should (= 0 (call-process "fd" nil nil nil "--version"))))

  ;; NOTE: git is NOT tested here - it's a system prerequisite, not bundled
  )

;;; LSP Servers

(ert-deftest test-runtime-deps/nil-lsp-available ()
  "Test that nil (Nix LSP) is available in PATH."
  :tags '(fast unit)
  (should (executable-find "nil")))

(ert-deftest test-runtime-deps/gopls-available ()
  "Test that gopls (Go LSP) is available in PATH."
  :tags '(unit)
  (should (executable-find "gopls")))

(ert-deftest test-runtime-deps/bash-language-server-available ()
  "Test that bash-language-server is available in PATH."
  :tags '(unit)
  (should (executable-find "bash-language-server")))

(ert-deftest test-runtime-deps/typescript-language-server-available ()
  "Test that typescript-language-server is available in PATH."
  :tags '(unit)
  (should (executable-find "typescript-language-server")))

(ert-deftest test-runtime-deps/clangd-available ()
  "Test that clangd (C/C++ LSP) is available in PATH."
  :tags '(unit)
  (should (executable-find "clangd")))

(ert-deftest test-runtime-deps/marksman-available ()
  "Test that marksman (Markdown LSP) is available in PATH."
  :tags '(unit)
  (should (executable-find "marksman")))

(ert-deftest test-runtime-deps/yaml-language-server-available ()
  "Test that yaml-language-server is available in PATH."
  :tags '(unit)
  (should (executable-find "yaml-language-server")))

(ert-deftest test-runtime-deps/lsp-servers-functional ()
  "Test that LSP servers can execute and respond to version queries."
  :tags '(integration)
  ;; Test nil
  (when (executable-find "nil")
    (should (= 0 (call-process "nil" nil nil nil "--version"))))
  
  ;; Test gopls
  (when (executable-find "gopls")
    (should (= 0 (call-process "gopls" nil nil nil "version")))))

;;; Integration Tests - Tree-sitter with Emacs

(ert-deftest test-runtime-deps/treesit-available ()
  "Test that Emacs has tree-sitter support compiled in."
  :tags '(integration)
  ;; Changed from skip-unless to should: treesit must be available
  (should (fboundp 'treesit-available-p))
  (should (treesit-available-p)))

(ert-deftest test-runtime-deps/treesit-can-load-grammar ()
  "Test that tree-sitter can actually load a grammar."
  :tags '(integration slow)
  ;; Changed from skip-unless to should: treesit must be available
  (should (fboundp 'treesit-available-p))
  (should (treesit-available-p))
  (should (getenv "TREE_SITTER_DIR"))

  ;; Try to load a common grammar (nix or bash)
  (let ((loaded nil)
        (grammars-to-try '(nix bash python)))
    (dolist (lang grammars-to-try)
      (condition-case err
          (when (treesit-language-available-p lang)
            (setq loaded t))
        (error nil)))

    ;; Changed: at least one grammar MUST load (not just should)
    (should loaded)))

;;; Comprehensive Runtime Environment Check

(ert-deftest test-runtime-deps/complete-environment ()
  "Comprehensive test that all critical runtime dependencies are available.
NOTE: git is NOT checked here as it's a system prerequisite, not bundled."
  :tags '(integration)
  (let ((missing '())
        (critical-tools '(("rg" . "ripgrep")
                          ("fd" . "fd")
                          ("nil" . "Nix LSP"))))
    
    ;; Check critical tools
    (dolist (tool critical-tools)
      (unless (executable-find (car tool))
        (push (cdr tool) missing)))
    
    ;; Check TREE_SITTER_DIR
    (unless (and (getenv "TREE_SITTER_DIR")
                 (file-directory-p (getenv "TREE_SITTER_DIR")))
      (push "TREE_SITTER_DIR" missing))
    
    ;; Report findings
    (when missing
      (message "Missing runtime dependencies: %s" 
               (mapconcat #'identity (nreverse missing) ", ")))
    
    (should (null missing))))

(provide 'test-runtime-deps)
;;; test-runtime-deps.el ends here
