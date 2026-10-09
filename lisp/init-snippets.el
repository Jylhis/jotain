;;; init-snippets.el --- Template/snippet expansion via Tempel -*- lexical-binding: t; -*-

;;; Commentary:

;; Snippets via Tempel (same author as corfu/cape/vertico): snippet names
;; surface through `completion-at-point-functions' in the corfu popup, and
;; templates get tab-stop fields.  TAB is not a field key; it indents and
;; completes (see `jotain-completion-free-tab').
;;
;; Templates live in `templates/jotain.eld', keyed by major mode.
;;
;; `eglot-tempel' is a snippet-expansion adapter, so it lives here rather
;; than with the LSP wiring in `init-prog.el'.

;;; Code:

;; Top level, not in `:init', so the byte-compiler sees the
;; `make-variable-buffer-local' that `defvar-local' expands to.
(defvar-local jotain-tempel--eglot-merged nil
  "Merged tempel+eglot capf installed in this buffer, or nil.")

;; Defined in `init-completion.el', which `init.el' loads first.
(defvar jotain-completion-snippets)
(defvar jotain-completion-eglot-nonexclusive)

;;; @doc Lightweight template/snippet engine from the corfu/cape author.
;;; Templates are read from `templates/*.eld' (keyed by major mode).
;;; `M-+' completes a snippet by name, `M-*' inserts one interactively;
;;; `M-}'/`M-{' or `C-M-n'/`C-M-p' move between fields, `M-RET' finishes.
;;; TAB indents and completes, never moves fields. Set
;;; `jotain-completion-snippets' to nil to keep snippet names out of the
;;; popup; `M-+' and `M-*' still work.
(use-package tempel
  :functions (tempel-complete cape-capf-super cape-capf-buster
              cape-capf-nonexclusive eglot-completion-at-point eglot-managed-p)
  :custom
  (tempel-path (expand-file-name "templates/*.eld" user-emacs-directory))
  :bind
  (("M-+" . tempel-complete)
   ("M-*" . tempel-insert)
   :map tempel-map
   ;; Aliases for tempel's `M-}'/`M-{'.  `tempel-map' is an overlay
   ;; `keymap' property, so it outranks corfu's map while a snippet is
   ;; live.
   ("C-M-n" . tempel-next)
   ("C-M-p" . tempel-previous))
  :init
  ;; Depth -90 puts snippet names ahead of the cape capfs
  ;; (`init-completion.el').
  (defun jotain-tempel-setup-capf ()
    "Add `tempel-complete' to the front of the buffer-local capfs."
    (add-hook 'completion-at-point-functions #'tempel-complete -90 t))
  (when jotain-completion-snippets
    (add-hook 'prog-mode-hook #'jotain-tempel-setup-capf)
    (add-hook 'text-mode-hook #'jotain-tempel-setup-capf))
  ;; Merge the snippet capf with eglot's so both share one popup instead
  ;; of server candidates only showing when no template matches.  The
  ;; merge inherits eglot's exclusivity, hence the optional
  ;; `cape-capf-nonexclusive' wrapper (rationale in
  ;; `jotain-completion-eglot-nonexclusive'; tested in
  ;; test/completion-test.el).  `cape-capf-buster' drops the candidate
  ;; cache `cape-capf-super' would otherwise keep for the capf's lifetime.
  (defun jotain-tempel-eglot-capf ()
    "Merge `tempel-complete' with eglot's capf in managed buffers.
`eglot-managed-mode-hook' also runs on shutdown: then drop the merged
capf (its eglot half would signal without a connection), restore the
plain tempel capf, and re-arm the merge for a reconnect."
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
      (when jotain-tempel--eglot-merged
        (setq-local completion-at-point-functions
                    (remq jotain-tempel--eglot-merged
                          completion-at-point-functions))
        (setq jotain-tempel--eglot-merged nil)
        (add-hook 'completion-at-point-functions #'tempel-complete -90 t))))
  (when jotain-completion-snippets
    (add-hook 'eglot-managed-mode-hook #'jotain-tempel-eglot-capf)))

;;; @doc Lets Tempel expand snippets sent by language servers (e.g.
;;; argument placeholders). Enabled as soon as eglot loads, because it
;;; must be on before eglot connects.
(use-package eglot-tempel
  :after eglot
  :functions (eglot-tempel-mode)
  :config (eglot-tempel-mode 1))

(provide 'init-snippets)
;;; init-snippets.el ends here
