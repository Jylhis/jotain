;;; init-lang-data.el --- Data, templating, and query language modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Modes for data-shaped files: YAML, CSV, SQL, Jinja2 templates, and
;; gnuplot scripts.

;;; Code:

(use-package yaml-mode
  :defer t)

(use-package csv-mode
  :mode "\\.csv\\'"
  :hook (csv-mode . csv-align-mode)
  :custom (csv-separators '("," ";" "|" "\t")))

(use-package sql-indent
  :defer t)

(use-package jinja2-mode
  :mode (("\\.j2\\'"      . jinja2-mode)
         ("\\.jinja2?\\'" . jinja2-mode)))

(use-package gnuplot
  :mode ("\\.plt\\'" . gnuplot-mode))

(provide 'init-lang-data)
;;; init-lang-data.el ends here
