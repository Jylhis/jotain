;;; init-lang-python.el --- Python language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Python mode (built-in tree-sitter variant) plus the usual conveniences.
;; The eglot hook is registered in `init-prog'; the LSP server itself
;; (pyright / basedpyright / ruff-lsp) comes from the project's own
;; environment, not from this config.
;;
;; Everything about *which* Python runs — the REPL interpreter and the
;; one Org Babel shells out to — is decided here rather than in
;; `init-org', so there is a single place to look when a `#+begin_src
;; python' block picks up the wrong binary.

;;; Code:

;;; @doc Built-in Python mode pinned to its tree-sitter variant so we
;;; get the modern parser without any third-party package. The LSP
;;; server (pyright/basedpyright/ruff-lsp) comes from the project's
;;; own environment, not from this config. When IPython is on PATH it
;;; becomes the REPL — better completion, tracebacks and `%magic' for
;;; `run-python' and for Org Babel `:session' blocks alike — while
;;; one-shot (sessionless) Babel blocks stay on plain `python3'.
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python3" . python-ts-mode)
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  :config
  ;; Org 9.7's default for `org-babel-python-command' is `auto', which
  ;; derives the non-session command from `python-shell-interpreter'
  ;; *and its args*. Pinning it explicitly keeps the REPL swap below
  ;; from handing a one-shot block an interactive `ipython -i', which
  ;; would sit waiting for input instead of returning a result.
  (setopt org-babel-python-command "python3")
  ;; The IPython REPL is strictly nicer than the stock one, but it is
  ;; not guaranteed to be installed — this config never assumes an
  ;; interpreter it does not ship. `--simple-prompt' is what makes
  ;; IPython usable from comint at all: it turns off the prompt_toolkit
  ;; UI that python.el cannot drive.
  (when (executable-find "ipython")
    (setopt python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i --simple-prompt")))

(provide 'init-lang-python)
;;; init-lang-python.el ends here
