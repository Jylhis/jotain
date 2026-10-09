;;; test-ui.el --- Theme wiring regression tests -*- lexical-binding: t; -*-

;;; Commentary:

;; The Jylhis themes come from an out-of-tree pin (nix/design-pin.nix),
;; so the theme symbols `init-ui.el' names can drift from the theme files
;; the pin ships (as upstream renames have done).  On drift `load-theme'
;; signals and init-ui falls back to Modus.  Byte compilation can't
;; catch it: the `load-theme' calls are skipped in batch.
;;
;; The tests read `init-ui.el' as data instead of loading it, which would
;; pull in doom-modeline, auto-dark, and friends.

;;; Code:

(require 'ert)

(defconst test-ui--init-ui-file
  (expand-file-name "lisp/init-ui.el"
                    (locate-dominating-file
                     (or load-file-name buffer-file-name default-directory)
                     "init.el"))
  "Absolute path to the init-ui module under test.")

(defun test-ui--defcustom-default (symbol)
  "Return the default-value form of the defcustom named SYMBOL.
Reads `test-ui--init-ui-file' as data, so no module is loaded.  Returns
nil when SYMBOL has no defcustom in that file."
  (with-temp-buffer
    (insert-file-contents test-ui--init-ui-file)
    (goto-char (point-min))
    (catch 'found
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (when (and (consp form)
                         (eq (car form) 'defcustom)
                         (eq (nth 1 form) symbol))
                (throw 'found (nth 2 form)))))
        (end-of-file nil)))))

(ert-deftest test-ui-theme-defaults-are-quoted-symbols ()
  "`init-ui.el' names a theme symbol for each system appearance."
  (dolist (var '(jotain-theme-light jotain-theme-dark))
    (let ((default (test-ui--defcustom-default var)))
      (should (eq (car-safe default) 'quote))
      (should (symbolp (cadr default))))))

(ert-deftest test-ui-named-themes-are-loadable ()
  "Every theme `init-ui.el' names is one Emacs can actually load.
Skipped when the Jylhis theme package is absent — `init-ui.el' falls
back to Modus in that case, which is a supported configuration."
  (skip-unless (require 'jylhis-themes nil t))
  (let ((available (custom-available-themes)))
    (dolist (var '(jotain-theme-light jotain-theme-dark))
      (should (memq (cadr (test-ui--defcustom-default var)) available)))))

(provide 'test-ui)
;;; test-ui.el ends here
