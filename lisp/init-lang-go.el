;;; init-lang-go.el --- Go language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Go tree-sitter modes, gopls workspace settings, struct-tag editing,
;; and a test runner.  gopls (eglot), goimports (apheleia), and dlv
;; (dape) are wired in `init-prog'.
;;
;; No Go binaries ship with this config.  Expected on the project PATH:
;; go, gopls, goimports, dlv, and optionally gomodifytags and
;; golangci-lint (a compile-multi entry in `init-project').

;;; Code:

(declare-function jotain-eglot-set-workspace-config "init-prog" (key settings))

;; gopls only emits inlay hints for the kinds enabled here; init-prog
;; turns on `eglot-inlay-hints-mode' to show them.  `gofumpt' stays nil
;; so gopls agrees with the goimports formatter apheleia runs on save.
(jotain-eglot-set-workspace-config
 :gopls
 '(:usePlaceholders t
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
              :unusedwrite t)))

;;; @doc Built-in tree-sitter Go modes: `go-ts-mode` for source,
;;; `go-mod-ts-mode` for go.mod, `go-work-ts-mode` (Emacs 31) for
;;; go.work. gopls (eglot), goimports (apheleia, on save), and dlv (dape)
;;; are wired in init-prog; all Go tooling comes from the project PATH.
(use-package go-ts-mode
  :ensure nil
  :mode (("\\.go\\'"     . go-ts-mode)
         ("/go\\.mod\\'"  . go-mod-ts-mode))
  :custom
  ;; 8 matches the default `tab-width', so one level is one gofmt tab.
  ;; Emacs 31 renames this `go-ts-indent-offset' (obsolete alias kept);
  ;; switch when the floor moves to 31.
  (go-ts-mode-indent-offset 8))

;; go.work: `go-work-ts-mode' on Emacs 31, else `go-mod-ts-mode' (the
;; gomod grammar handles the near-identical syntax).
(add-to-list 'auto-mode-alist
             (cons "/go\\.work\\'"
                   (if (fboundp 'go-work-ts-mode)
                       'go-work-ts-mode
                     'go-mod-ts-mode)))

;; go-tag/gotest pull in classic `go-mode' as a dependency, and its
;; autoloads add global state that breaks unrelated buffers:
;;
;;   - `magic-mode-alist' entry `(go--is-go-asm . go-asm-mode)'.  It runs
;;     for EVERY visited file and autoloads go-mode; when go-mode is not
;;     on `load-path' that signals "Cannot open load file ... go-mode"
;;     and the buffer never reaches its real mode.
;;   - `auto-mode-alist' entries routing Go files to the classic modes.
;;
;; Only stripping the magic entry prevents the load error; the remap
;; below acts after the predicate has already fired.  Both removals are
;; no-ops when go-mode's autoloads were never loaded.
(setq magic-mode-alist (assq-delete-all 'go--is-go-asm magic-mode-alist))
(dolist (classic '(go-mode go-dot-mod-mode go-dot-work-mode))
  (setq auto-mode-alist (rassq-delete-all classic auto-mode-alist)))

;; If anything else still selects a classic Go mode (e.g. a stale
;; `package-quickstart' autoload), remap it to the tree-sitter mode.
(when (fboundp 'go-ts-mode)
  (dolist (remap '((go-mode         . go-ts-mode)
                   (go-dot-mod-mode . go-mod-ts-mode)
                   (go-mod-mode     . go-mod-ts-mode)))
    (add-to-list 'major-mode-remap-alist remap)))

;;; @doc Struct-tag editing (`json:"..."`, `db:"..."`, ...) via the
;;; project's `gomodifytags`. Bound under the mode-local `C-c C-t` Go
;;; prefix, clear of the global `C-c t` theme toggle.
(use-package go-tag
  :after go-ts-mode
  :bind (:map go-ts-mode-map
              ("C-c C-t a" . go-tag-add-tags)
              ("C-c C-t d" . go-tag-remove-tags)))

;;; @doc Run the Go test or benchmark at point, the current file's
;;; tests, or the whole project, with compilation-mode error jumping.
;;; Shares the `C-c C-t` prefix with go-tag.
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
