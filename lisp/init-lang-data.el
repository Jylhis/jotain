;;; init-lang-data.el --- Data, templating, and query language modes -*- lexical-binding: t; -*-

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

;; Modes for data-shaped files: YAML, KDL, CSV, jq, SQL, Jinja2 templates,
;; and gnuplot scripts.

;;; Code:

(declare-function mixed-pitch-mode "mixed-pitch" (&optional arg))
(declare-function my-eglot-set-workspace-config "init-prog" (key settings))

;; yaml-language-server settings.  Declaring GitLab's `!reference' as a
;; sequence tag silences "Unresolved tag: !reference" in `.gitlab-ci.yml'.
(my-eglot-set-workspace-config
 :yaml '(:customTags ["!reference sequence"]))

(defun my-lang-data--enable-prog-mode-features ()
  "Run `prog-mode-hook' in a `text-mode'-derived config buffer.
Both `yaml-mode' and `yaml-ts-mode' derive from `text-mode', so
prog-mode features (line numbers, flymake, hl-todo,
indent-bars, ...) never fire in YAML.  The `derived-mode-p' guard
avoids running the hook twice if the mode ever reparents onto
`prog-mode'.  Then turn off the prose features `text-mode-hook'
enabled: mixed-pitch, visual-line wrapping, and jinx."
  (unless (derived-mode-p 'prog-mode)
    (run-hooks 'prog-mode-hook))
  (mixed-pitch-mode -1)
  (visual-line-mode -1)
  (when (bound-and-true-p visual-wrap-prefix-mode)
    (visual-wrap-prefix-mode -1))
  (when (bound-and-true-p jinx-mode)
    (jinx-mode -1)))

;;; @doc YAML major mode (MELPA), loaded on demand. YAML derives from
;;; `text-mode' upstream, so we re-run `prog-mode-hook' to get line
;;; numbers, flymake, indent guides and the rest of the code setup.
(use-package yaml-mode
  :defer t
  :hook (yaml-mode . my-lang-data--enable-prog-mode-features))

;;; @doc Built-in tree-sitter YAML mode. Same prog-mode hook tweak as
;;; `yaml-mode'; a separate block so the built-in mode works without
;;; `use-package-always-ensure' pulling in MELPA `yaml-mode'.
(use-package yaml-ts-mode
  :ensure nil
  :defer t
  :hook (yaml-ts-mode . my-lang-data--enable-prog-mode-features))

;;; @doc jq filter major mode (`.jq` files): syntax highlighting and
;;; indentation for jq programs. The mode derives from `prog-mode', so
;;; the usual programming features apply.
(use-package jq-mode
  :mode "\\.jq\\'")

;;; @doc KDL document language major mode (`.kdl` files, e.g. the
;;; nixtamal `manifest.kdl`): syntax highlighting and indentation.
(use-package kdl-mode
  :mode "\\.kdl\\'")

;;; @doc CSV major mode. csv-align-mode aligns columns visually without
;;; changing the file.
(use-package csv-mode
  :mode "\\.csv\\'"
  :hook (csv-mode . csv-align-mode)
  :custom (csv-separators '("," ";" "|" "\t")))

;;; @doc Syntax-aware indentation for SQL: SELECT lists, JOINs, and
;;; CTEs.
(use-package sql-indent
  :defer t)

;;; @doc Jinja2 / Ansible / Saltstack templating syntax. Mode regex
;;; covers `.j2`, `.jinja`, and `.jinja2`.
(use-package jinja2-mode
  :mode (("\\.j2\\'"      . jinja2-mode)
         ("\\.jinja2?\\'" . jinja2-mode)))

;;; @doc Major mode for gnuplot script files (`.plt`).
(use-package gnuplot
  :mode ("\\.plt\\'" . gnuplot-mode))

(provide 'init-lang-data)
;;; init-lang-data.el ends here
