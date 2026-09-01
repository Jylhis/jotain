;;; test-ui.el --- Theme wiring regression tests -*- lexical-binding: t; -*-

;;; Commentary:

;; The Jylhis themes come from an out-of-tree pin (nix/design-pin.nix,
;; built as jylhis-emacs-themes), so the theme symbols `init-ui.el'
;; names and the theme files that pin actually ships can drift apart.
;; When they do, `load-theme' signals — and because `init.el' requires
;; init-ui unguarded, every module after it would silently never load.
;;
;; v2.0.0 of the design system (the theming framework) renamed
;; `jylhis-sheet'/`jylhis-field' to the theme×mode symbols
;; `jylhis-survey-light'/`jylhis-survey-dark', which is exactly that
;; failure.  Byte
;; compilation cannot catch it: the `load-theme' calls sit behind
;; `(unless noninteractive ...)' and are never evaluated in batch.
;;
;; These tests read `init-ui.el' as data rather than loading it — the
;; module pulls in doom-modeline, auto-dark and friends, none of which
;; are worth booting in batch just to read two defcustom defaults.

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
