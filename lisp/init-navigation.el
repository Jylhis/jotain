;;; init-navigation.el --- File, project, and window navigation -*- lexical-binding: t; -*-

;;; Commentary:

;; Dired (built-in) and dirvish (third-party enhancement) live together
;; here, as do `project' and `winner'. The rule from the top of init.el:
;; if package A only exists to enhance built-in B, they share a file.

;;; Code:

(use-package dired
  :ensure nil
  :custom
  (dired-listing-switches "-alh --group-directories-first")
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

;; Shorten /nix/store/abc123-foo-1.0 display to …foo-1.0 in dired/shell
;; buffers. Pure cosmetic, no behavioral change.
(use-package pretty-sha-path
  :hook ((dired-mode shell-mode) . pretty-sha-path-mode))

(use-package dirvish
  :demand t
  :after dired
  :custom
  (dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state))
  (dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  :config
  (dirvish-override-dired-mode 1))

(use-package project
  :ensure nil
  :custom
  (project-vc-extra-root-markers '(".project" "package.json" "Cargo.toml" "pyproject.toml" "flake.nix")))

;; Resize all sibling windows proportionally when splitting, instead
;; of always halving the current window.
(setopt window-combination-resize t)

(use-package winner
  :ensure nil
  :config (winner-mode 1))

;; Reversible `C-x 1': first press collapses to a single window,
;; second press restores the previous layout via `winner-mode'.
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
