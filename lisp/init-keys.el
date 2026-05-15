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

;; DWIM C-g: the default `keyboard-quit' doesn't close a minibuffer that
;; isn't the selected window, which is a frequent papercut once you
;; enable `enable-recursive-minibuffers'. This version aborts whatever
;; the obvious target is — region, completions buffer, or minibuffer —
;; before falling back to plain `keyboard-quit'.
(defun jotain-keyboard-quit-dwim ()
  "Do-What-I-Mean `keyboard-quit'.
Completions window visible → close it.  Minibuffer open (even
when point is in another window) → abort recursive edit.  Region
active → deactivate it.  Otherwise call regular `keyboard-quit'."
  (interactive)
  (cond
   ((get-buffer-window "*Completions*" 'visible) (delete-completion-window))
   ((> (minibuffer-depth) 0)                     (abort-recursive-edit))
   ((region-active-p)                            (keyboard-quit))
   (t                                            (keyboard-quit))))

;;; @doc Top-level rebindings — disable accidental suspend (C-z and
;;; C-x C-z), put other-window on M-o for one-key window switching,
;;; bind C-x j to the two-window rotate helper above, and rebind
;;; C-g to the DWIM quit so it closes minibuffers from elsewhere.
(use-package emacs
  :ensure nil
  :bind
  (("C-z" . nil)
   ("C-x C-z" . nil)
   ([remap keyboard-quit] . jotain-keyboard-quit-dwim)
   ("M-o" . other-window)
   ("C-x j" . jotain-toggle-window-split)))

;;; @doc Built-in directional window switching — Shift-<arrow> moves
;;; focus between split windows. Ships with Emacs; no reason not
;;; to turn it on globally.
(use-package windmove
  :ensure nil
  :config (windmove-default-keybindings))

(provide 'init-keys)
;;; init-keys.el ends here
