;;; init-navigation.el --- File, project, and window navigation -*- lexical-binding: t; -*-

;;; Commentary:

;; Dired (built-in) and dirvish (third-party enhancement) live together
;; here, as do `project' and `winner'. The rule from the top of init.el:
;; if package A only exists to enhance built-in B, they share a file.

;;; Code:

;;; @doc Built-in directory editor — Jotain's primary file manager.
;;; The custom block below tames cross-platform ls quirks (BSD on
;;; macOS lacks `--group-directories-first` and `--dired`). `M-s R`
;;; previews a regex replacement across the contents of all marked
;;; files as a unified diff (Emacs 30's
;;; `dired-do-replace-regexp-as-diff').
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
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-create-destination-dirs 'ask)
  (dired-free-space nil)
  (dired-vc-rename-file t)
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ("M-s R" . dired-do-replace-regexp-as-diff))
  :config
  ;; Emacs 31+: also hide the absolute directory path in the header line
  ;; under `dired-hide-details-mode'. Guarded for Emacs 30.
  (when (boundp 'dired-hide-details-hide-absolute-location)
    (setopt dired-hide-details-hide-absolute-location t)))

;;; @doc Built-in dired extras — `dired-omit-mode` hides dotfiles and
;;; cache directories so dired listings show only the things you
;;; actually want to see.
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

;;; @doc Async file ops for dired — wraps `dired-do-copy`, `dired-do-rename`,
;;; `dired-do-symlink`, `dired-do-hardlink` so they fork into a subprocess
;;; instead of blocking the main Emacs. Multi-GB copies no longer freeze
;;; the UI; the mode-line shows progress and a message fires on completion.
(use-package dired-async
  :ensure async
  :after dired
  :config (dired-async-mode 1))

;;; @doc rsync from dired — bound to `C-c C-r` in `dired-mode-map`. Best
;;; for very large transfers or TRAMP sources/destinations: hands marked
;;; files to `rsync` in an async shell buffer with live progress. Uses
;;; `--progress` (not `--info=progress2`) so stock macOS rsync 2.6.9 still
;;; works; noisier output, but portable.
(use-package dired-rsync
  :after dired
  :bind (:map dired-mode-map ("C-c C-r" . dired-rsync))
  :custom
  (dired-rsync-options "-az --progress --human-readable"))

;;; @doc Pure-Lisp ls emulation. Fallback for macOS without GNU coreutils
;;; — gives us folders-first sorting that BSD ls cannot produce.
(use-package ls-lisp
  :ensure nil
  :if (and (eq system-type 'darwin) (not (executable-find "gls")))
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-use-string-collate t)
  (ls-lisp-UCA-like-collation t)
  (ls-lisp-verbosity '(links uid gid)))

;;; @doc Built-in writable dired — C-c C-e turns the dired buffer into
;;; a regular text buffer where you can rename/chmod files with the
;;; usual editing commands. Save to commit changes.
(use-package wdired
  :ensure nil
  :after dired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t)
  :bind (:map dired-mode-map ("C-c C-e" . wdired-change-to-wdired-mode)))

;;; @doc Pretty colours for dired (font-locks files by type, age,
;;; executability). Pure cosmetic, big readability win.
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;;; @doc Live filter dired buffers by typing a fragment after `/`.
;;; Faster than re-running ls with a glob.
(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow)))

;;; @doc Inline tree expansion in dired — TAB on a directory expands its
;;; contents below it instead of opening a new buffer.
(use-package dired-subtree
  :after dired
  :custom
  (dired-subtree-use-backgrounds nil)
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)))

;;; @doc Browse the system trash bin from inside Emacs. With
;;; `delete-by-moving-to-trash` set in init-core, every dired
;;; deletion is recoverable through `M-x trashed`.
(use-package trashed
  :commands trashed
  :custom
  (trashed-action-confirmer 'y-or-n-p)
  (trashed-use-header-line t)
  (trashed-sort-key '("Date deleted" . t)))

;;; @doc Shortens `/nix/store/abc123-foo-1.0` to `…foo-1.0` in dired and
;;; shell buffers — purely cosmetic, but transformative on a system
;;; that's mostly Nix store paths.
(use-package pretty-sha-path
  :hook ((dired-mode shell-mode) . pretty-sha-path-mode))

;;; @doc Modern dired front-end with previews, side panels, and miller
;;; columns. Overrides plain dired so every `C-x d` benefits.
;;; `dirvish-side' (C-c D) already provides the docked side-tree that
;;; Emacs 31's new `speedbar-window' offers, so speedbar is intentionally
;;; not wired up here.
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
  :bind (("C-c f" . dirvish)
         ("C-c D" . dirvish-side))
  :config
  (dirvish-override-dired-mode 1))

;; Resize all sibling windows proportionally when splitting, instead
;; of always halving the current window.
(setopt window-combination-resize t)

;;; @doc Built-in window-layout undo/redo — pairs with the toggle helper
;;; below so `C-x 1` becomes a reversible "expand this window"
;;; command.
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
