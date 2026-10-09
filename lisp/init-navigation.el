;;; init-navigation.el --- File, project, and window navigation -*- lexical-binding: t; -*-

;;; Commentary:

;; Dired and its enhancements (dirvish and friends) live together here,
;; per the init.el rule that a package enhancing a built-in shares its
;; file.  Also window layout: `winner' and a reversible `C-x 1'.

;;; Code:

;;; @doc Built-in directory editor, Jotain's primary file manager. On
;;; macOS it uses GNU `gls` when available, since BSD ls lacks
;;; `--group-directories-first` and `--dired`. `M-s R` previews a regex
;;; replacement across the marked files as a unified diff. `!`/`&`
;;; suggest the OS default application (`open`/`xdg-open`/`start`) for
;;; common document and media files; files can also be dragged out to
;;; desktop apps with the mouse.
(use-package dired
  :ensure nil
  :custom
  ;; macOS BSD `ls' rejects `--dired' and `--group-directories-first'.
  ;; Prefer GNU `gls'; otherwise use BSD ls without `--dired'.
  (insert-directory-program (or (and (eq system-type 'darwin)
                                     (executable-find "gls"))
                                "ls"))
  (dired-use-ls-dired (or (not (eq system-type 'darwin))
                          (and (executable-find "gls") t)))
  ;; `v': natural number sort (foo2 before foo10).
  (dired-listing-switches (if (and (eq system-type 'darwin)
                                   (not (executable-find "gls")))
                              "-alhv"
                            "-alhv --group-directories-first"))
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-deletion-confirmer #'y-or-n-p)
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  ;; Revert the destination after copy/rename, but not over TRAMP, where
  ;; a round-trip per file op would stall the UI.
  (dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-create-destination-dirs 'ask)
  (dired-free-space nil)
  (dired-vc-rename-file t)
  ;; Keep point on file lines, off the header and trailing blank lines.
  (dired-movement-style 'bounded-files)
  (dired-mouse-drag-files t)
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ("M-s R" . dired-do-replace-regexp-as-diff))
  :config
  ;; Emacs 31+: `dired-hide-details-mode' also hides the absolute path.
  (when (boundp 'dired-hide-details-hide-absolute-location)
    (setopt dired-hide-details-hide-absolute-location t))
  ;; `!'/`&' suggest the OS default handler for documents, images and
  ;; media.
  (when-let* ((opener (cond
                       ((eq system-type 'darwin) "open")
                       ((memq system-type '(gnu gnu/linux gnu/kfreebsd
                                                berkeley-unix))
                        "xdg-open")
                       ((memq system-type '(cygwin windows-nt ms-dos))
                        "start"))))
    (setopt dired-guess-shell-alist-user
            `(("\\.\\(?:docx\\|pdf\\|odt\\|odg\\|ods\\|djvu\\|eps\\)\\'" ,opener)
              ("\\.\\(?:jpe?g\\|webp\\|png\\|gif\\|xpm\\)\\'" ,opener)
              ("\\.xcf\\'" ,opener)
              ("\\.tex\\'" ,opener)
              ("\\.\\(?:mp4\\|mkv\\|m4a\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'"
               ,opener)
              ("\\.\\(?:mp3\\|flac\\)\\'" ,opener)))))

;;; @doc Built-in dired extras. `dired-omit-mode` hides lock and
;;; auto-save files, `.git`, `.DS_Store`, Syncthing folders, `__pycache__`
;;; and flycheck/flymake temp files.
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
           "\\|^\\.DS_Store\\'"
           "\\|^\\.stfolder\\'"
           "\\|^\\.stversions\\'"
           "\\|^__pycache__\\'"
           "\\|^flycheck_.*"
           "\\|^flymake_.*")))

;;; @doc Async file ops for dired: copy, rename, symlink and hardlink run
;;; in a subprocess, so large copies do not freeze the UI.
(use-package dired-async
  :ensure async
  :after dired
  :config (dired-async-mode 1))

;;; @doc rsync from dired (`C-c C-r`), for very large transfers or TRAMP
;;; endpoints: hands the marked files to `rsync` asynchronously with live
;;; progress. `--progress`, not `--info=progress2`, so stock macOS rsync
;;; 2.6.9 still works.
(use-package dired-rsync
  :after dired
  :bind (:map dired-mode-map ("C-c C-r" . dired-rsync))
  :custom
  (dired-rsync-options "-az --progress --human-readable"))

;;; @doc Pure-Lisp ls emulation, used on macOS without GNU coreutils to
;;; get the folders-first sorting BSD ls cannot produce.
(use-package ls-lisp
  :ensure nil
  :if (and (eq system-type 'darwin) (not (executable-find "gls")))
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil)
  (ls-lisp-use-string-collate t)
  (ls-lisp-UCA-like-collation t)
  (ls-lisp-verbosity '(links uid gid)))

;;; @doc Built-in writable dired: C-c C-e makes the listing editable,
;;; so files can be renamed and chmodded with normal editing. Save
;;; (C-c C-c) to apply.
(use-package wdired
  :ensure nil
  :after dired
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t)
  :bind (:map dired-mode-map ("C-c C-e" . wdired-change-to-wdired-mode)))

;;; @doc Extra dired colours by file type, permissions and more.
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;;; @doc Live-filter a dired buffer by typing a fragment after `/`.
(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow)))

;;; @doc Browse the system trash. With `delete-by-moving-to-trash` set
;;; in init-core, dired deletions are recoverable through `M-x trashed`.
(use-package trashed
  :commands trashed
  :custom
  (trashed-action-confirmer 'y-or-n-p)
  (trashed-use-header-line t)
  (trashed-sort-key '("Date deleted" . t)))

;;; @doc Shortens `/nix/store/abc123-foo-1.0` to `…foo-1.0` in dired and
;;; shell buffers.
(use-package pretty-sha-path
  :hook ((dired-mode shell-mode) . pretty-sha-path-mode))

;;; @doc Modern dired front-end with previews, side panels and miller
;;; columns, replacing plain dired everywhere. C-c d opens it, C-c D a
;;; docked side tree. `TAB` expands a directory inline and `<backtab>`
;;; opens the subtree menu. The standalone `dired-subtree` package is
;;; deliberately not loaded: it and dirvish's own subtree engine corrupt
;;; each other's overlays. The side tree covers what speedbar would, so
;;; speedbar is not wired up.
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
         ("C-c D" . dirvish-side)
         :map dired-mode-map
         ("TAB" . dirvish-subtree-toggle)
         ("<backtab>" . dirvish-subtree-menu))
  :config
  (dirvish-override-dired-mode 1))

;; Splitting resizes all sibling windows proportionally.
(setopt window-combination-resize t)

;;; @doc Built-in window-layout undo/redo. Makes `C-x 1` reversible
;;; (see below).
(use-package winner
  :ensure nil
  :config (winner-mode 1))

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
