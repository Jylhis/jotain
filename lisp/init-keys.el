;;; init-keys.el --- Global keymap and leader-key setup -*- lexical-binding: t; -*-

;;; Commentary:

;; Only *global* keybindings live here — the kind you want to see in a
;; single place so the whole keymap is reviewable at a glance.
;; Per-package bindings stay in the relevant `use-package' block via
;; `:bind', `:bind-keymap', or `:general'; that way a package's keys
;; live next to the package's config, and removing the package removes
;; the keys with it.

;;; Code:

;; Swap a two-window layout between horizontal and vertical. Bound below.
(defun jotain-toggle-window-split ()
  "Toggle between horizontal and vertical window split.
Only works when there are exactly two windows — buffers, point
positions, and focus are preserved during the swap."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car  this-win-edges)
                                         (car  next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter (if (= (car this-win-edges)
                              (car (window-edges (next-window))))
                           'split-window-horizontally
                         'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (when this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window)     next-win-buffer)
          (select-window first-win)
          (when this-win-2nd (other-window 1))))
    (user-error "Can only toggle split with exactly 2 windows")))

;;; @doc Top-level rebindings — disable accidental suspend (C-z and
;;; @doc C-x C-z), put other-window on M-o for one-key window switching,
;;; @doc and bind C-x j to the two-window rotate helper above.
(use-package emacs
  :ensure nil
  :bind
  (("C-z" . nil)
   ("C-x C-z" . nil)
   ("M-o" . other-window)
   ("C-x j" . jotain-toggle-window-split)))

;;; @doc Built-in directional window switching — Shift-<arrow> moves
;;; @doc focus between split windows. Ships with Emacs; no reason not
;;; @doc to turn it on globally.
(use-package windmove
  :ensure nil
  :config (windmove-default-keybindings))

(provide 'init-keys)
;;; init-keys.el ends here
