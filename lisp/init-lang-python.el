;;; init-lang-python.el --- Python language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Built-in `python-ts-mode'.  The LSP server is resolved in `init-prog'
;; from the project's environment.
;;
;; Which Python runs, for the REPL and for Org Babel, is decided here
;; rather than in `init-org'.

;;; Code:

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python3" . python-ts-mode)
  :custom
  (python-shell-interpreter "python3")
  :config
  ;; A non-`auto' value overrides both
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
