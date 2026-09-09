;;; init-lang-rust.el --- Rust language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust mode (built-in tree-sitter variant), eglot wired through
;; rust-analyzer (the hook lives in `init-prog'), and a couple of
;; conveniences for the typical edit-test-format loop.

;;; Code:

(defvar eglot-workspace-configuration) ; defined in eglot.el

;; rust-analyzer workspace settings.  Eglot only ever reads the GLOBAL
;; value of `eglot-workspace-configuration' (it evaluates the variable in
;; a fresh temp buffer, so a buffer-local mode-hook binding never reaches
;; the server), so contribute our section to the default value keyed under
;; `:rust-analyzer'.  Other languages own their own sections, so nothing
;; is clobbered, and a project .dir-locals.el `eglot-workspace-configuration'
;; entry still overrides it cleanly (a buffer-local value shadows the
;; global default eglot reads).  Wrapped in `with-eval-after-load' so the
;; variable eglot defines is present (and the section is set before the
;; first server connects).  `check.command "clippy"' runs clippy over the
;; workspace on every save, which is noticeably slow on a cold target
;; directory; a project .dir-locals.el can override this back to `check'
;; if that matters.  `cargo (:features "all")' is deliberately left out —
;; it is a real cost on large workspaces, so it stays a per-project decision.
(with-eval-after-load 'eglot
  (setq-default eglot-workspace-configuration
                (plist-put (copy-sequence
                            (default-value 'eglot-workspace-configuration))
                           :rust-analyzer
                           '(:check (:command "clippy")
                             :cargo (:buildScripts (:enable t))
                             :procMacro (:enable t)))))

;;; @doc Built-in tree-sitter Rust mode. Eglot wires rust-analyzer in
;;; init-prog, with workspace configuration (clippy on save, build
;;; scripts, proc macros) contributed to the global
;;; `eglot-workspace-configuration' above; format-on-save runs rustfmt
;;; through apheleia.
(use-package rust-ts-mode
  :ensure nil
  :mode "\\.rs\\'"
  :custom
  (rust-ts-mode-indent-offset 4))

(provide 'init-lang-rust)
;;; init-lang-rust.el ends here
