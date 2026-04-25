;;; init-navigation.el --- File, project, and window navigation -*- lexical-binding: t; -*-

;;; Commentary:

;; Dired (built-in) and dirvish (third-party enhancement) live together
;; here, as do `project' and `winner'. The rule from the top of init.el:
;; if package A only exists to enhance built-in B, they share a file.

;;; Code:

(use-package dired
  :ensure nil
  :custom
  ;; macOS ships BSD `ls', which rejects `--dired' and `--group-directories-first'.
  ;; Prefer GNU `gls' from coreutils when available; otherwise fall back to BSD
  ;; ls and disable the `--dired' handshake so Emacs doesn't error on startup.
  (insert-directory-program (or (and (eq system-type 'darwin)
                                     (executable-find "gls"))
                                "ls"))
  (dired-use-ls-dired (or (not (eq system-type 'darwin))
                          (and (executable-find "gls") t)))
  (dired-listing-switches (if (and (eq system-type 'darwin)
                                   (not (executable-find "gls")))
                              "-alh"
                            "-alh --group-directories-first"))
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-create-destination-dirs 'ask)
  (dired-free-space nil)
  (dired-vc-rename-file t)
  :hook (dired-mode . dired-hide-details-mode))

(use-package dired-x
  :ensure nil
  :after dired
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil)
  (dired-omit-files
   (concat "\\`[.]?#\\|\\`[.][.]?\\'"
           "\\|^[a-zA-Z0-9]\\.syncthing-enc\\'"
           "\\|^\\.git\\'"
           "\\|^\\.stfolder\\'"
           "\\|^\\.stversions\\'"
           "\\|^__pycache__\\'")))

;; When BSD `ls' is all we have (macOS without coreutils), fall back to
;; Emacs's built-in `ls-lisp' emulation so we still get folders-first
;; sorting — BSD ls lacks `--group-directories-first'.
(use-package ls-lisp
  :ensure nil
  :if (and (eq system-type 'darwin) (not (executable-find "gls")))
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-use-string-collate t)
  (ls-lisp-UCA-like-collation t)
  (ls-lisp-verbosity '(links uid gid)))

(use-package wdired
  :ensure nil
  :after dired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t)
  :bind (:map dired-mode-map ("C-c C-e" . wdired-change-to-wdired-mode)))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow)))

(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)))

(use-package trashed
  :commands trashed
  :custom
  (trashed-action-confirmer 'y-or-n-p)
  (trashed-use-header-line t)
  (trashed-sort-key '("Date deleted" . t)))

;; Shorten /nix/store/abc123-foo-1.0 display to …foo-1.0 in dired/shell
;; buffers. Pure cosmetic, no behavioral change.
(use-package pretty-sha-path
  :hook ((dired-mode shell-mode) . pretty-sha-path-mode))

(use-package dirvish
  :demand t
  :after dired
  :functions (dirvish-override-dired-mode)
  :custom
  (dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state))
  (dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (dirvish-default-layout '(0 0.4 0.6))
  (dirvish-preview-dispatchers '(image gif video audio epub archive pdf))
  (dirvish-side-width 30)
  :bind (("C-c d" . dirvish)
         ("C-c D" . dirvish-side))
  :config
  (dirvish-override-dired-mode 1))

;; Resize all sibling windows proportionally when splitting, instead
;; of always halving the current window.
(setopt window-combination-resize t)

(use-package winner
  :ensure nil
  :config (winner-mode 1))

;; Reversible `C-x 1': first press collapses to a single window,
;; second press restores the previous layout via `winner-mode'.
(declare-function winner-undo "winner")
(defun jotain-nav-toggle-delete-other-windows ()
  "Delete other windows, or restore the previous layout.
If only one window is visible and `winner-mode' has a previous
configuration, undo the deletion instead."
  (interactive)
  (if (and winner-mode (one-window-p))
      (winner-undo)
    (delete-other-windows)))

(keymap-global-set "C-x 1" #'jotain-nav-toggle-delete-other-windows)

(provide 'init-navigation)
;;; init-navigation.el ends here
