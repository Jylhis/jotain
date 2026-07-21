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
  ;; Renamed to `go-ts-indent-offset' in Emacs 31 (obsolete alias keeps
  ;; this working); rename atomically when the floor moves to 31.
  (go-ts-mode-indent-offset 8))

;; go.work: Emacs 31 adds `go-work-ts-mode'; on Emacs 30 fall back to
;; `go-mod-ts-mode', whose gomod grammar handles the near-identical
;; syntax.  This prepends to `auto-mode-alist' so it wins over any
;; go-work-ts-mode entry Emacs registers on its own.
(add-to-list 'auto-mode-alist
             (cons "/go\\.work\\'"
                   (if (fboundp 'go-work-ts-mode)
                       'go-work-ts-mode
                     'go-mod-ts-mode)))

;; go-tag/gotest (below) drag in the *classic* `go-mode' package as a
;; transitive dependency; nothing in this config selects it deliberately
;; — every Go buffer uses the built-in tree-sitter modes.  go-mode's
;; autoloads install two pieces of *global* state that break unrelated
;; buffers, so strip them:
;;
;;   - A `magic-mode-alist' entry `(go--is-go-asm . go-asm-mode)'.
;;     `magic-mode-alist' is consulted for EVERY file `set-auto-mode'
;;     visits, and `go--is-go-asm' is autoloaded from `go-mode', so
;;     opening any file at all (e.g. a .nix file) force-loads classic
;;     go-mode.  When go-mode is not on `load-path' this raises "File
;;     mode specification error: Cannot open load file ... go-mode" and
;;     aborts mode setup, so the buffer never reaches its real mode.
;;   - `auto-mode-alist' entries routing .go / go.mod / go.work to the
;;     classic modes (this module's own `:mode' and the go.work entry
;;     above already route them to the tree-sitter modes).
;;
;; Stripping the `magic-mode-alist' entry is what actually prevents the
;; load error: the `major-mode-remap-alist' remap below does NOT (it
;; only rewrites the *chosen* mode; the magic predicate fires earlier
;; and loads go-mode directly).  The removals are no-ops when go-mode's
;; autoloads were never loaded, so this stays safe everywhere.
(setq magic-mode-alist (assq-delete-all 'go--is-go-asm magic-mode-alist))
(dolist (classic '(go-mode go-dot-mod-mode go-dot-work-mode))
  (setq auto-mode-alist (rassq-delete-all classic auto-mode-alist)))

;; Belt-and-suspenders: if some other path still selects a classic Go
;; major mode (a stale `package-quickstart' autoload, an old
;; `auto-mode-alist' entry), remap it to the tree-sitter equivalent so
;; `go-mode' is never required and every Go buffer lands in `go-ts-mode'.
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
