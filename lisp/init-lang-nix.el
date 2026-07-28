;;; init-lang-nix.el --- Nix language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Nix is the package manager and dev-shell tool this whole config is
;; built around, so it earns its own language file even though we don't
;; write much of it day to day. The eglot hook is registered in
;; `init-prog' so all language servers are visible from one place.

;;; Code:

;; Blank-line indentation for `nix-ts-mode'.
;;
;; On a blank line treesit finds no node starting at BOL, so
;; `treesit-simple-indent' matches on PARENT alone.  Most containers are
;; handled by the mode's own `parent-is' rules; three parents fall
;; through to column 0: `binding_set' (its prev-sibling anchor returns
;; nil when NODE is nil), and `source_code'/`ERROR' (a not-yet-balanced
;; buffer).  Indent those relative to the previous non-blank line.

(defvar nix-ts-mode-indent-offset)        ; defined in nix-ts-mode.el
(defvar treesit-simple-indent-rules)      ; defined in treesit.el
(declare-function treesit-node-type "treesit.c")

(defun jotain-nix-ts--line-opens-p ()
  "Non-nil if the current line's last non-blank token opens a block.
Openers are the tokens after which Nix nests one level: one of the
brackets ({, [, (), an =, a lambda colon, or the let/in keywords."
  (save-excursion
    (end-of-line)
    (skip-chars-backward " \t")
    (let ((end (point)))
      (or (memq (char-before) '(?{ ?\[ ?\( ?= ?:))
          (progn
            (skip-chars-backward "a-z")
            (member (buffer-substring-no-properties (point) end)
                    '("let" "in")))))))

(defun jotain-nix-ts--blank-line-indent (node parent _bol &rest _)
  "Indent a blank line in `nix-ts-mode' from the previous non-blank line.
A treesit single-function indent rule: return (ANCHOR . OFFSET), or nil
to decline.  Fires only when NODE is nil (a blank line) and PARENT is one
of the containers nix-ts-mode otherwise indents to column 0.  Returns nil
at the buffer start and inside multi-line strings (whose PARENT is never
in the set), letting the mode's own rules run."
  (when (and (null node)
             parent
             (member (treesit-node-type parent)
                     '("source_code" "binding_set" "ERROR")))
    (save-excursion
      (forward-line 0)
      (let (found)
        (while (and (not found) (not (bobp)))
          (forward-line -1)
          (unless (looking-at-p "[ \t]*$")
            (setq found t)))
        (when found
          (let ((opener (jotain-nix-ts--line-opens-p)))
            (back-to-indentation)
            (cons (point) (if opener nix-ts-mode-indent-offset 0))))))))

(defun jotain-nix-ts--install-blank-line-rule ()
  "Prepend `jotain-nix-ts--blank-line-indent' to the buffer-local rules.
`treesit-major-mode-setup' has already rebuilt (and made buffer-local)
`treesit-simple-indent-rules' by the time this hook runs, so consing a
new head onto the `nix' entry never mutates the shared default."
  (when (assq 'nix treesit-simple-indent-rules)
    (setq-local treesit-simple-indent-rules
                (cons (cons 'nix
                            (cons #'jotain-nix-ts--blank-line-indent
                                  (alist-get 'nix treesit-simple-indent-rules)))
                      (assq-delete-all 'nix
                                       (copy-sequence treesit-simple-indent-rules))))))

;;; @doc Tree-sitter Nix major mode. Nix is the package manager and
;;; dev-shell tool the whole config is built around, so it gets a
;;; dedicated module even though we don't write much of it daily.
;;; Provided by Nix; format-on-save flows through apheleia →
;;; nixfmt (configured in init-prog). A blank-line indent rule fills the
;;; gap where tree-sitter leaves an empty line at column 0.
(use-package nix-ts-mode
  :ensure nil ; Provided by Nix
  :mode "\\.nix\\'"
  :hook (nix-ts-mode . jotain-nix-ts--install-blank-line-rule))

(provide 'init-lang-nix)
;;; init-lang-nix.el ends here
