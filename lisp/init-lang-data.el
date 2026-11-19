;;; init-lang-data.el --- Data, templating, and query language modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Modes for data-shaped files: YAML, KDL, CSV, jq, SQL, Jinja2 templates,
;; and gnuplot scripts.

;;; Code:

(declare-function mixed-pitch-mode "mixed-pitch" (&optional arg))
(declare-function jotain-eglot-set-workspace-config "init-prog" (key settings))

;; yaml-language-server settings.  Declaring GitLab's `!reference' as a
;; sequence tag silences "Unresolved tag: !reference" in `.gitlab-ci.yml'.
(jotain-eglot-set-workspace-config
 :yaml '(:customTags ["!reference sequence"]))

(defun jotain-lang-data--enable-prog-mode-features ()
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

(use-package yaml-mode
  :defer t
  :hook (yaml-mode . jotain-lang-data--enable-prog-mode-features))

;; A separate block so the built-in mode works without
;; `use-package-always-ensure' pulling in MELPA `yaml-mode'.
(use-package yaml-ts-mode
  :ensure nil
  :defer t
  :hook (yaml-ts-mode . jotain-lang-data--enable-prog-mode-features))

(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . js-json-mode))

(use-package jq-mode
  :mode "\\.jq\\'")

(use-package kdl-mode
  :mode "\\.kdl\\'")

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
