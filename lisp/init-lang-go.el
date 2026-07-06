;;; init-lang-go.el --- Go language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Go modes (built-in tree-sitter variants) plus test-at-point via
;; gotest.  The eglot hook is registered in `init-prog'; format-on-save
;; flows through apheleia -> goimports and debugging through dape ->
;; dlv, both configured in `init-prog'.
;;
;; Per the convention for language tooling, no Go binaries ship with
;; this config.  Expected on the project/host PATH: go, gopls (LSP),
;; goimports (formatting), dlv (debugging), and optionally
;; golangci-lint (compile-multi entry).

;;; Code:

;; gopls workspace settings.  Buffer-local in a mode hook instead of
;; `setq-default' so no other language's workspace configuration is
;; clobbered and a project .dir-locals.el
;; `eglot-workspace-configuration' entry still overrides it cleanly.
(defvar eglot-workspace-configuration) ; defined in eglot.el

(defun jotain-go--eglot-workspace-config ()
  "Conservative gopls defaults: staticcheck plus inlay-hint sources.
gopls only emits inlay hints when the hint kinds are enabled in
workspace configuration; `eglot-inlay-hints-mode' (armed for Go in
init-prog) then displays them."
  (setq-local eglot-workspace-configuration
              '(:gopls (:staticcheck t
                        :usePlaceholders t
                        :hints (:parameterNames t
                                :assignVariableTypes t
                                :compositeLiteralFields t
                                :compositeLiteralTypes t
                                :constantValues t
                                :functionTypeParameters t
                                :rangeVariableTypes t)))))

;;; @doc Built-in tree-sitter Go modes: `go-ts-mode` for source files,
;;; `go-mod-ts-mode` for go.mod. Eglot wires gopls in init-prog;
;;; format-on-save runs goimports through apheleia; dape drives dlv
;;; for debugging. All Go tooling (go, gopls, goimports, dlv) comes
;;; from the project/host PATH, not from this config.
(use-package go-ts-mode
  :ensure nil
  :mode (("\\.go\\'" . go-ts-mode)
         ("/go\\.mod\\'" . go-mod-ts-mode))
  :hook ((go-ts-mode . jotain-go--eglot-workspace-config)
         (go-mod-ts-mode . jotain-go--eglot-workspace-config))
  :custom
  ;; gofmt indents with tabs; a step of 8 (the default) matches the
  ;; default `tab-width' so one indent level renders as exactly one tab.
  (go-ts-mode-indent-offset 8))

;; go.work: Emacs 31 adds `go-work-ts-mode'; on Emacs 30 fall back to
;; `go-mod-ts-mode', whose gomod grammar handles the near-identical
;; syntax.  This runs after treesit-auto (loaded in init-prog), so the
;; entry prepends ahead of the go-work-ts-mode entry treesit-auto
;; registers even on Emacs 30.
(add-to-list 'auto-mode-alist
             (cons "/go\\.work\\'"
                   (if (fboundp 'go-work-ts-mode)
                       'go-work-ts-mode
                     'go-mod-ts-mode)))

;;; @doc Run the Go test or benchmark at point, the current file's
;;; tests, or the whole project, with compilation-mode error jumping.
;;; Bound under the mode-local `C-c C-t` prefix in Go buffers.
(use-package gotest
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-c C-t t" . go-test-current-test)
              ("C-c C-t f" . go-test-current-file)
              ("C-c C-t p" . go-test-current-project)
              ("C-c C-t b" . go-test-current-benchmark)
              ("C-c C-t c" . go-test-current-coverage)
              ("C-c C-t r" . go-run)))

(provide 'init-lang-go)
;;; init-lang-go.el ends here
