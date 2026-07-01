;;; init-lang-data.el --- Data, templating, and query language modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Modes for data-shaped files: YAML, CSV, SQL, Jinja2 templates, and
;; gnuplot scripts.

;;; Code:

(declare-function mixed-pitch-mode "mixed-pitch" (&optional arg))

(defun jotain-lang-data--enable-prog-mode-features ()
  "Run `prog-mode-hook' in a `text-mode'-derived config buffer.
Both `yaml-mode' (MELPA) and the built-in `yaml-ts-mode' derive
from `text-mode', so none of the prog-mode niceties — line
numbers, flymake, editorconfig, dtrt-indent, hl-todo,
breadcrumb, indent-bars, fill-column indicator — ever fire in
YAML buffers.  Re-running the hook reaches them without having
to enumerate every minor mode separately, and without changing
the mode's upstream parent.  The `derived-mode-p' guard makes
this a no-op if the buffer's mode ever (re)parents onto
`prog-mode', preventing the hook from firing twice.

`text-mode-hook' also runs first and enables prose niceties
that are wrong for code-shaped YAML: mixed-pitch (proportional)
fonts, visual-line wrapping, and on-the-fly spell check.  Disable
those after the parent hook ran so the buffer ends up looking
like code, not prose."
  (unless (derived-mode-p 'prog-mode)
    (run-hooks 'prog-mode-hook))
  (mixed-pitch-mode -1)
  (visual-line-mode -1)
  (when (bound-and-true-p visual-wrap-prefix-mode)
    (visual-wrap-prefix-mode -1))
  (when (bound-and-true-p jinx-mode)
    (jinx-mode -1)))

;;; @doc YAML major mode (MELPA). Loaded on demand for the dozens of
;;; YAML-shaped files in any modern repo (CI, k8s, helm). YAML derives
;;; from `text-mode' upstream, so we re-fire `prog-mode-hook' to get
;;; the full editor surface (line numbers, flymake, indent guides, …).
(use-package yaml-mode
  :defer t
  :hook (yaml-mode . jotain-lang-data--enable-prog-mode-features))

;;; @doc Built-in tree-sitter YAML mode (Emacs 29+). Same prog-mode
;;; hook tweak as `yaml-mode'; kept in its own use-package block so
;;; users running the built-in mode aren't forced to install the
;;; MELPA `yaml-mode' package by `use-package-always-ensure'.
(use-package yaml-ts-mode
  :ensure nil
  :defer t
  :hook (yaml-ts-mode . jotain-lang-data--enable-prog-mode-features))

;;; @doc CSV major mode with column alignment. csv-align-mode renders
;;; separators visually so wide files become readable without
;;; reflowing the actual bytes.
(use-package csv-mode
  :mode "\\.csv\\'"
  :hook (csv-mode . csv-align-mode)
  :custom (csv-separators '("," ";" "|" "\t")))

;;; @doc Smart indentation for SQL files — keeps SELECT lists, JOINs,
;;; and CTEs aligned without manual whitespace fiddling.
(use-package sql-indent
  :defer t)

;;; @doc Jinja2 / Ansible / Saltstack templating syntax. Mode regex
;;; covers `.j2`, `.jinja`, and `.jinja2`.
(use-package jinja2-mode
  :mode (("\\.j2\\'"      . jinja2-mode)
         ("\\.jinja2?\\'" . jinja2-mode)))

;;; @doc Major mode for gnuplot script files (`.plt`). Useful when an
;;; analysis pipeline emits its own plotting scripts.
(use-package gnuplot
  :mode ("\\.plt\\'" . gnuplot-mode))

(provide 'init-lang-data)
;;; init-lang-data.el ends here
