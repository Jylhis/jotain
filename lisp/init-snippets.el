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

;;; @doc Lightweight template/snippet engine from the corfu/cape author.
;;; Templates are read from `templates/*.eld' (keyed by major mode);
;;; `M-+' completes a snippet by name, `M-*' inserts one interactively,
;;; and once fields are active `TAB'/`S-TAB' move between them.
(use-package tempel
  :functions (tempel-complete)
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
  ;; (cf. the cape capfs in `init-completion.el').
  (defun jotain-tempel-setup-capf ()
    "Add `tempel-complete' to the front of the buffer-local capfs."
    (add-hook 'completion-at-point-functions #'tempel-complete -90 t))
  (add-hook 'prog-mode-hook #'jotain-tempel-setup-capf)
  (add-hook 'text-mode-hook #'jotain-tempel-setup-capf))

;;; @doc Lets Tempel expand the snippets language servers send back
;;; (e.g. function-argument placeholders from rust-analyzer / pyright).
;;; `eglot-tempel-mode' must be enabled before eglot connects, so it is
;;; armed the moment eglot loads rather than after the first session.
(use-package eglot-tempel
  :after eglot
  :functions (eglot-tempel-mode)
  :init (with-eval-after-load 'eglot (eglot-tempel-mode 1)))

(provide 'init-snippets)
;;; init-snippets.el ends here
