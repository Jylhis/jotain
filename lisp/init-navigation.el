;;; init-navigation.el --- File, project, and window navigation -*- lexical-binding: t; -*-

;;; Commentary:

;; Dired and its enhancements (dirvish and friends) live together here,
;; per the init.el rule that a package enhancing a built-in shares its
;; file.  Also window layout: `winner' and a reversible `C-x 1'.

;;; Code:

(use-package dired
  :ensure nil
  :custom
  ;; `v': natural number sort (foo2 before foo10).
  (dired-listing-switches "-alhv --group-directories-first")
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-deletion-confirmer #'y-or-n-p)
  ;; Revert the destination after copy/rename, but not over TRAMP, where
  ;; a round-trip per file op would stall the UI.
  (dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-create-destination-dirs 'ask)
  (dired-free-space nil)
  (dired-vc-rename-file t)
  ;; Keep point on file lines, off the header and trailing blank lines.
  (dired-movement-style 'bounded-files)
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ("M-s R" . dired-do-replace-regexp-as-diff))
  :config
  ;; `dired-hide-details-mode' also hides the absolute path.
  (setopt dired-hide-details-hide-absolute-location t)
  ;; `!'/`&' suggest the OS default handler for documents, images and
  ;; media.
  (setopt dired-guess-shell-alist-user
          '(("\\.\\(?:docx\\|pdf\\|odt\\|odg\\|ods\\|djvu\\|eps\\)\\'" "xdg-open")
            ("\\.\\(?:jpe?g\\|webp\\|png\\|gif\\|xpm\\)\\'" "xdg-open")
            ("\\.xcf\\'" "xdg-open")
            ("\\.tex\\'" "xdg-open")
            ("\\.\\(?:mp4\\|mkv\\|m4a\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'"
             "xdg-open")
            ("\\.\\(?:mp3\\|flac\\)\\'" "xdg-open"))))

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

(use-package dired-async
  :ensure async
  :after dired
  :config (dired-async-mode 1))

(use-package dired-rsync
  :after dired
  :bind (:map dired-mode-map ("C-c C-r" . dired-rsync))
  :custom
  (dired-rsync-options "-az --progress --human-readable"))

(use-package wdired
  :ensure nil
  :after dired
  :custom
  (wdired-allow-to-change-permissions t)
  :bind (:map dired-mode-map ("C-c C-e" . wdired-change-to-wdired-mode)))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map ("/" . dired-narrow)))

(use-package trashed
  :commands trashed
  :custom
  (trashed-action-confirmer 'y-or-n-p))

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
         ("C-c D" . dirvish-side)
         :map dired-mode-map
         ("TAB" . dirvish-subtree-toggle)
         ("<backtab>" . dirvish-subtree-menu))
  :config
  (dirvish-override-dired-mode 1))

(setopt window-combination-resize t)

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
