;;; init-navigation.el --- File, project, and window navigation -*- lexical-binding: t; -*-

;;; Commentary:

;; Dired (built-in) and dirvish (third-party enhancement) live together
;; here, as do `project' and `winner'. The rule from the top of init.el:
;; if package A only exists to enhance built-in B, they share a file.

;;; Code:

;; Cache the result of checking for GNU ls on macOS so the three dired
;; settings below don't each re-scan exec-path independently.
(defvar jotain--gnu-ls
  (and (eq system-type 'darwin) (executable-find "gls"))
  "Path to GNU ls (`gls') on macOS, or nil if unavailable.")

(use-package dired
  :ensure nil
  :custom
  ;; macOS ships BSD `ls', which rejects `--dired' and `--group-directories-first'.
  ;; Prefer GNU `gls' from coreutils when available; otherwise fall back to BSD
  ;; ls and disable the `--dired' handshake so Emacs doesn't error on startup.
  (insert-directory-program (or jotain--gnu-ls "ls"))
  (dired-use-ls-dired (or (not (eq system-type 'darwin))
                          (and jotain--gnu-ls t)))
  (dired-listing-switches (if (and (eq system-type 'darwin)
                                   (not jotain--gnu-ls))
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
   (concat "\\`[.]?#\\|\\`[.][.]?\\'"n           "\\|^[a-zA-Z0-9]\\.syncthing-enc\\'"n           "\\|^\\.git\\'"n           "\\|^\\.stfolder\\'"n           "\\|^\\.stversions\\'"n           "\\|^__pycache__\\'")))

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
