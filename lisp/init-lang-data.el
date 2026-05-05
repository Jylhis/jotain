;;; init-lang-data.el --- Data, templating, and query language modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Modes for data-shaped files: YAML, CSV, SQL, Jinja2 templates, and
;; gnuplot scripts.

;;; Code:

;;; @doc YAML major mode. Loaded on demand for the dozens of YAML-shaped
;;; files in any modern repo (CI, k8s, helm).
(use-package yaml-mode
  :defer t)

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
