;;; init-snippets.el --- Template/snippet expansion via Tempel -*- lexical-binding: t; -*-

;;; Commentary:

;; Snippet tooling for basic language constructs (for/while/if/function,
;; etc.).  We use Tempel rather than the built-in `skeleton'/`abbrev' or
;; the heavier `yasnippet': Tempel is by the same author as the
;; corfu/cape/vertico stack this config already runs, so it plugs
;; straight into `completion-at-point-functions' (snippet names surface
;; in the corfu popup) and adds tab-stop field navigation that the
;; built-ins lack.  Those fields move on `M-}'/`M-{' (tempel's own keys)
;; or `C-M-n'/`C-M-p'; TAB is not involved anywhere, because in this
;; configuration TAB indents and nothing else.
;;
;; The curated templates themselves live in `templates/jotain.eld', keyed
;; by major mode -- not in this file -- so adding a snippet never means
;; touching Elisp.
;;
;; `eglot-tempel' lives here too: although LSP server config is otherwise
;; centralised in `init-prog.el', this is purely a snippet-expansion
;; adapter, so it belongs with the snippet feature it enables.

;;; Code:

;; Defined inside `use-package' :config blocks below; declared for the
;; clean-session per-file compile (nix/config-compiled-split.nix).
(declare-function jotain-tempel-setup-capf nil)
(declare-function jotain-tempel-eglot-capf nil)

;; Buffer-local guard for the merged tempel+eglot capf (see
;; `jotain-tempel-eglot-capf' below).  Declared at top level so the
;; byte-compiler sees the `make-variable-buffer-local' call `defvar-local'
;; emits as a toplevel form; inside `use-package' `:init' it is not.
(defvar-local jotain-tempel--eglot-merged nil
  "Merged tempel+eglot capf installed in this buffer, or nil.")

;; Both are defined in `init-completion.el', which `init.el' loads first.
(defvar jotain-completion-snippets)
(defvar jotain-completion-eglot-nonexclusive)

;;; @doc Lightweight template/snippet engine from the corfu/cape author.
;;; Templates are read from `templates/*.eld' (keyed by major mode);
;;; `M-+' completes a snippet by name, `M-*' inserts one interactively,
;;; and once fields are active `M-}'/`M-{' or `C-M-n'/`C-M-p' move
;;; between them, with `M-RET' to finish. TAB is deliberately not
;;; involved: in this configuration TAB indents and nothing else. Set
;;; `jotain-completion-snippets' to nil to keep snippet names out of the
;;; completion popup; `M-+' and `M-*' still work.
(use-package tempel
  :functions (tempel-complete cape-capf-super cape-capf-buster
              cape-capf-nonexclusive eglot-completion-at-point eglot-managed-p)
  :custom
  (tempel-path (expand-file-name "templates/*.eld" user-emacs-directory))
  :bind
  (("M-+" . tempel-complete)
   ("M-*" . tempel-insert)
   :map tempel-map
   ;; TAB is deliberately absent: it indents, always.  Upstream `tempel-map'
   ;; never bound it either -- the TAB/S-TAB pair that used to live here was
   ;; this config's own addition.  Removing it restores tempel's own
   ;; `M-}'/`M-{' (plus `M-RET' to finish and `M-<up>'/`M-<down>'), and the
   ;; two below are added as a mnemonic alias.  Neither collides with
   ;; corfu's `M-n'/`M-p', which matters because `tempel-map' rides on an
   ;; overlay `keymap' property and so outranks corfu's minor-mode map while
   ;; a snippet is live.
   ("C-M-n" . tempel-next)
   ("C-M-p" . tempel-previous))
  :init
  ;; Prepend `tempel-complete' to the buffer-local capf list so snippet
  ;; names appear in the corfu popup ahead of dabbrev/keyword candidates
  ;; (cf. the cape capfs in `init-completion.el').
  (defun jotain-tempel-setup-capf ()
    "Add `tempel-complete' to the front of the buffer-local capfs."
    (add-hook 'completion-at-point-functions #'tempel-complete -90 t))
  (when jotain-completion-snippets
    (add-hook 'prog-mode-hook #'jotain-tempel-setup-capf)
    (add-hook 'text-mode-hook #'jotain-tempel-setup-capf))
  ;; Merge the snippet capf with eglot's so both sets of candidates share
  ;; one popup, rather than the server's list only appearing when no
  ;; template name matches.
  ;;
  ;; Note this is NOT about tempel shadowing the LSP -- it cannot.  Both
  ;; tempel capfs return `:exclusive no' and fall through when nothing
  ;; matches.  Eglot is the exclusive one (eglot.el declares no
  ;; `:exclusive' at all), so it suppresses whatever follows it, and
  ;; `cape-capf-super' propagates non-exclusivity only when *every* input
  ;; is non-exclusive -- so the merge inherits eglot's exclusivity and
  ;; would silently kill the global cape fallbacks.  Hence the
  ;; `cape-capf-nonexclusive' wrapper; `test/completion-test.el' measures
  ;; both halves.  `cape-capf-buster' invalidates the candidate cache that
  ;; `cape-capf-super' otherwise holds for the whole lifetime of the capf.
  (defun jotain-tempel-eglot-capf ()
    "Merge `tempel-complete' with eglot's capf in managed buffers.
`eglot-managed-mode-hook' also runs on server shutdown; in that
teardown branch, drop the merged capf -- its eglot half would
signal with no live connection -- restore the plain tempel capf,
and re-arm the merge for a later reconnect."
    (if (eglot-managed-p)
        (unless jotain-tempel--eglot-merged
          (remove-hook 'completion-at-point-functions #'tempel-complete t)
          (setq jotain-tempel--eglot-merged
                (let ((merged (cape-capf-buster
                               (cape-capf-super #'tempel-complete
                                                #'eglot-completion-at-point))))
                  (if jotain-completion-eglot-nonexclusive
                      (cape-capf-nonexclusive merged)
                    merged)))
          (setq-local completion-at-point-functions
                      (cons jotain-tempel--eglot-merged
                            (remq #'eglot-completion-at-point
                                  completion-at-point-functions))))
      ;; Teardown: drop the merged capf (its eglot half now signals) and
      ;; restore the plain tempel capf; also re-arms the merge for reconnect.
      (when jotain-tempel--eglot-merged
        (setq-local completion-at-point-functions
                    (remq jotain-tempel--eglot-merged
                          completion-at-point-functions))
        (setq jotain-tempel--eglot-merged nil)
        (add-hook 'completion-at-point-functions #'tempel-complete -90 t))))
  (when jotain-completion-snippets
    (add-hook 'eglot-managed-mode-hook #'jotain-tempel-eglot-capf)))

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
