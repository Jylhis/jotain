;;; init-lang-python.el --- Python language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Built-in `python-ts-mode'.  The LSP server is resolved in `init-prog'
;; from the project's environment.
;;
;; Which Python runs, for the REPL and for Org Babel, is decided here
;; rather than in `init-org'.

;;; Code:

;;; @doc Built-in Python mode pinned to its tree-sitter variant. The LSP
;;; server (basedpyright, with ruff via `rass` when present; else pyright
;;; or pylsp) comes from the project's environment. When IPython is on
;;; PATH it becomes the `run-python` REPL; Org Babel blocks, session or
;;; not, run plain `python3`.
(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python3" . python-ts-mode)
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  :config
  ;; Since Org 9.7 a non-`auto' value overrides both
  ;; `org-babel-python-command-session' (whose `auto' would inherit the
  ;; IPython interpreter and args below) and
  ;; `org-babel-python-command-nonsession' (default "python").
  (setopt org-babel-python-command "python3")
  ;; IPython is not shipped, so only use it when present.
  ;; `--simple-prompt' disables the prompt_toolkit UI that comint cannot
  ;; drive.
  (when (executable-find "ipython")
    (setopt python-shell-interpreter "ipython"
            python-shell-interpreter-args "-i --simple-prompt")))

(provide 'init-lang-python)
;;; init-lang-python.el ends here
