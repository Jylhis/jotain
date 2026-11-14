;;; init-lang-python.el --- Python language support -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
