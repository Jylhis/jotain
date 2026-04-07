;;; init-lang-rust.el --- Rust language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust mode (built-in tree-sitter variant), eglot wired through
;; rust-analyzer (the hook lives in `init-prog'), and a couple of
;; conveniences for the typical edit-test-format loop.

;;; Code:

(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :custom
  (rust-ts-mode-indent-offset 4))

;; Cargo commands as named compilation invocations — easier to remember
;; than typing them into M-x compile every time.
(defun jotain-rust-cargo-test ()
  "Run cargo test in the current project root."
  (interactive)
  (let ((default-directory (or (when-let* ((p (project-current)))
                                 (project-root p))
                               default-directory)))
    (compile "cargo test")))

(defun jotain-rust-cargo-clippy ()
  "Run cargo clippy in the current project root."
  (interactive)
  (let ((default-directory (or (when-let* ((p (project-current)))
                                 (project-root p))
                               default-directory)))
    (compile "cargo clippy --all-targets")))

(provide 'init-lang-rust)
;;; init-lang-rust.el ends here
