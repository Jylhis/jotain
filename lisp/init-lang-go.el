;;; init-lang-go.el --- Go language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Go modes (built-in tree-sitter variants), eglot wired through gopls
;; (the hook and its workspace configuration live in `init-prog'), and a
;; couple of conveniences for the typical edit-test loop. The external
;; tools (gopls, dlv, gotests, gomodifytags) come from the project's own
;; environment, not from this config — same philosophy as Rust/Python.

;;; Code:

;;; @doc Built-in tree-sitter Go modes. `go-mod-ts-mode' lives in the
;;; same `go-ts-mode' file, so both are registered from this one block.
;;; Eglot wires gopls in init-prog (for both modes); format-on-save runs
;;; gofmt through apheleia. No indent-offset is set: gofmt uses tabs and
;;; the built-in default already matches.
(use-package go-ts-mode
  :ensure nil
  :mode (("\\.go\\'"    . go-ts-mode)
         ("/go\\.mod\\'" . go-mod-ts-mode)))

;;; @doc Struct-tag editing (`json:"..."', `db:"..."', …). gopls has no
;;; equivalent; the underlying `gomodifytags' binary comes from the
;;; project environment. Bound under the buffer-local `C-c C-t' Go
;;; tooling prefix so it never shadows the global `C-c t' theme toggle.
(use-package go-tag
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-c C-t a" . go-tag-add-tags)
              ("C-c C-t d" . go-tag-remove-tags)))

;;; @doc Point-aware test runner. Complements the project-wide
;;; compile-multi "go test" commands with single-test / single-file runs.
;;; Shares the `C-c C-t' Go tooling prefix with go-tag.
(use-package gotest
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-c C-t t" . go-test-current-test)
              ("C-c C-t f" . go-test-current-file)
              ("C-c C-t p" . go-test-current-project)))

(provide 'init-lang-go)
;;; init-lang-go.el ends here
