;;; init-snippets.el --- Template/snippet expansion via Tempel -*- lexical-binding: t; -*-

;;; Commentary:

;; Snippet tooling for basic language constructs (for/while/if/function,
;; etc.).  We use Tempel rather than the built-in `skeleton'/`abbrev' or
;; the heavier `yasnippet': Tempel is by the same author as the
;; corfu/cape/vertico stack this config already runs, so it plugs
;; straight into `completion-at-point-functions' (snippet names surface
;; in the corfu popup) and adds TextMate-style tab-stop field navigation
;; that the built-ins lack.
;;
;; The curated templates themselves live in `templates/jotain.eld', keyed
;; by major mode -- not in this file -- so adding a snippet never means
;; touching Elisp.
;;
;; `eglot-tempel' lives here too: although LSP server config is otherwise
;; centralised in `init-prog.el', this is purely a snippet-expansion
;; adapter, so it belongs with the snippet feature it enables.

;;; Code:

;; Buffer-local guard for the merged tempel+eglot capf (see
;; `jotain-tempel-eglot-capf' below).  Declared at top level so the
;; byte-compiler sees the `make-variable-buffer-local' call `defvar-local'
;; emits as a toplevel form; inside `use-package' `:init' it is not.
(defvar-local jotain-tempel--eglot-merged nil
  "Non-nil once the merged tempel+eglot capf is installed in this buffer.")

;;; @doc Lightweight template/snippet engine from the corfu/cape author.
;;; Templates are read from `templates/*.eld' (keyed by major mode);
;;; `M-+' completes a snippet by name, `M-*' inserts one interactively,
;;; and once fields are active `TAB'/`S-TAB' move between them.
(use-package tempel
  :functions (tempel-complete cape-capf-super eglot-completion-at-point eglot-managed-p)
  :custom
  (tempel-path (expand-file-name "templates/*.eld" user-emacs-directory))
  :bind
  (("M-+" . tempel-complete)
   ("M-*" . tempel-insert)
   :map tempel-map
   ("TAB" . tempel-next)
   ("<backtab>" . tempel-previous))
  :init
  ;; Prepend `tempel-complete' to the buffer-local capf list so snippet
  ;; names appear in the corfu popup ahead of dabbrev/keyword candidates
  ;; (cf. the cape capfs in `init-completion.el').  Outside LSP buffers
  ;; this only ever shadows the low-value dabbrev/keyword fallbacks.
  (defun jotain-tempel-setup-capf ()
    "Add `tempel-complete' to the front of the buffer-local capfs."
    (add-hook 'completion-at-point-functions #'tempel-complete -90 t))
  (add-hook 'prog-mode-hook #'jotain-tempel-setup-capf)
  (add-hook 'text-mode-hook #'jotain-tempel-setup-capf)
  ;; In eglot-managed buffers, prepending `tempel-complete' would hide LSP
  ;; candidates for prefixes that match a snippet name (f, if, class...).
  ;; Merge the two into a single capf with `cape-capf-super' so snippet and
  ;; server candidates share one corfu popup instead of shadowing.  The
  ;; `jotain-tempel--eglot-merged' guard is declared at top level above.
  (defun jotain-tempel-eglot-capf ()
    "Merge `tempel-complete' with eglot's capf in managed buffers.
Idempotent: the buffer-local guard stops `eglot-reconnect' from
stacking duplicate super-capfs on repeated hook runs."
    (when (and (eglot-managed-p) (not jotain-tempel--eglot-merged))
      (setq jotain-tempel--eglot-merged t)
      (remove-hook 'completion-at-point-functions #'tempel-complete t)
      (setq-local completion-at-point-functions
                  (cons (cape-capf-super #'tempel-complete #'eglot-completion-at-point)
                        (remq #'eglot-completion-at-point
                              completion-at-point-functions)))))
  (add-hook 'eglot-managed-mode-hook #'jotain-tempel-eglot-capf))

;;; @doc Lets Tempel expand the snippets language servers send back
;;; (e.g. function-argument placeholders from rust-analyzer / pyright).
;;; `eglot-tempel-mode' must be enabled before eglot connects, so it is
;;; armed the moment eglot loads rather than after the first session.
(use-package eglot-tempel
  :after eglot
  :functions (eglot-tempel-mode)
  :config (eglot-tempel-mode 1))

(provide 'init-snippets)
;;; init-snippets.el ends here
