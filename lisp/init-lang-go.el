;;; init-lang-go.el --- Go language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Go modes (built-in tree-sitter variants), buffer-local gopls
;; workspace configuration, struct-tag editing, and a point-aware test
;; runner.  The eglot hook that starts gopls lives in `init-prog';
;; format-on-save flows through apheleia -> goimports and debugging
;; through dape -> dlv, both configured in `init-prog'.
;;
;; Per the convention for language tooling, no Go binaries ship with
;; this config.  Expected on the project/host PATH: go, gopls (LSP),
;; goimports (formatting), dlv (debugging), and optionally
;; gomodifytags (struct tags) and golangci-lint (compile-multi entry).

;;; Code:

;; gopls workspace settings.  Buffer-local in a mode hook rather than a
;; global `setq-default' so no other language's workspace configuration
;; is clobbered, and a project .dir-locals.el `eglot-workspace-configuration'
;; entry still overrides it cleanly.
(defvar eglot-workspace-configuration) ; defined in eglot.el

(defun jotain-go--eglot-workspace-config ()
  "Set buffer-local gopls workspace configuration for Go buffers.
gopls only emits inlay hints when the hint kinds are enabled here;
`eglot-inlay-hints-mode' (armed for Go in init-prog) then displays
them.  `gofumpt' is left at its default (nil) so gopls agrees with the
goimports/gofmt formatter apheleia runs on save."
  (setq-local eglot-workspace-configuration
              '(:gopls (:usePlaceholders t
                        :completeUnimported t
                        :staticcheck t
                        :hints (:parameterNames t
                                :assignVariableTypes t
                                :constantValues t
                                :functionTypeParameters t
                                :rangeVariableTypes t
                                :compositeLiteralTypes t
                                :compositeLiteralFields t)
                        :analyses (:unusedparams t
                                   :shadow t
                                   :nilness t
                                   :unusedwrite t)))))

;;; @doc Built-in tree-sitter Go modes: `go-ts-mode` for source files,
;;; `go-mod-ts-mode` for go.mod, `go-work-ts-mode` (Emacs 31) for
;;; go.work. Eglot wires gopls in init-prog; format-on-save runs
;;; goimports through apheleia; dape drives dlv for debugging. All Go
;;; tooling (go, gopls, goimports, dlv) comes from the project/host
;;; PATH, not from this config.
(use-package go-ts-mode
  :ensure nil
  :mode (("\\.go\\'"     . go-ts-mode)
         ("/go\\.mod\\'"  . go-mod-ts-mode))
  :hook ((go-ts-mode      . jotain-go--eglot-workspace-config)
         (go-mod-ts-mode  . jotain-go--eglot-workspace-config)
         ;; Emacs 31 mode; on 30 the hook variable is created but never
         ;; runs (go.work falls back to go-mod-ts-mode below).
         (go-work-ts-mode . jotain-go--eglot-workspace-config))
  :custom
  ;; gofmt indents with tabs; a step of 8 (the default) matches the
  ;; default `tab-width' so one indent level renders as exactly one tab.
  (go-ts-mode-indent-offset 8))

;; go.work: Emacs 31 adds `go-work-ts-mode'; on Emacs 30 fall back to
;; `go-mod-ts-mode', whose gomod grammar handles the near-identical
;; syntax.  Runs after treesit-auto (loaded in init-prog), so the entry
;; prepends ahead of the go-work-ts-mode entry treesit-auto registers
;; even on Emacs 30.
(add-to-list 'auto-mode-alist
             (cons "/go\\.work\\'"
                   (if (fboundp 'go-work-ts-mode)
                       'go-work-ts-mode
                     'go-mod-ts-mode)))

;; Route any *classic* Go major mode to its built-in tree-sitter
;; equivalent.  The classic `go-mode' package is only ever pulled in as
;; a transitive dependency of gotest/go-tag below; nothing in this
;; config selects it deliberately.  Without this remap, a stale
;; `package-quickstart' autoload, a treesit-auto grammar-missing
;; fallback, or an old `auto-mode-alist' entry could route a .go file to
;; `go-mode' and — when the classic package is not on load-path — raise
;; "File mode specification error: Cannot open load file ... go-mode".
;; The remap is consulted by `set-auto-mode' *before* the mode function
;; loads, so `go-mode' is never required and every Go buffer lands in
;; `go-ts-mode'.
(when (fboundp 'go-ts-mode)
  (dolist (remap '((go-mode         . go-ts-mode)
                   (go-dot-mod-mode . go-mod-ts-mode)
                   (go-mod-mode     . go-mod-ts-mode)))
    (add-to-list 'major-mode-remap-alist remap)))

;;; @doc Struct-tag editing (`json:"..."', `db:"..."', …). gopls has no
;;; equivalent; the underlying `gomodifytags' binary comes from the
;;; project environment. Bound under the buffer-local `C-c C-t' Go
;;; tooling prefix so it never shadows the global `C-c t' theme toggle.
(use-package go-tag
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-c C-t a" . go-tag-add-tags)
              ("C-c C-t d" . go-tag-remove-tags)))

;;; @doc Run the Go test or benchmark at point, the current file's
;;; tests, or the whole project, with compilation-mode error jumping.
;;; Complements the project-wide compile-multi "go test" commands with
;;; single-test / single-file runs. Shares the mode-local `C-c C-t' Go
;;; tooling prefix with go-tag.
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
