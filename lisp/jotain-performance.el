;;; jotain-performance.el --- Performance tweaks and ergonomic hacks -*- lexical-binding:t; -*-

;; Author: Markus Jylhänkangas
;; URL: https://github.com/jylhis/jotain

;;; Commentary:

;; A grab-bag of small performance and ergonomic adjustments cribbed
;; from popular Emacs configurations (Doom, Purcell, Prot, Centaur).
;; Each setting is documented with its origin and rationale.
;;
;; Source:
;; https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/

;;; Code:

;;;; Performance

;; Disable bidirectional text scanning.  Doom Emacs ships this
;; unconditionally — Emacs runs the bidirectional parenthesis algorithm
;; on every redisplay otherwise, which adds noticeable cost in large
;; files (multi-thousand-line JSON, log files).  Safe assuming the user
;; does not edit right-to-left scripts.
(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right)
(setopt bidi-inhibit-bpa t)

;; Defer fontification while the user is actively typing.  Removes
;; micro-stutters in tree-sitter modes and large buffers; the highlight
;; catches up the moment input pauses.
(setopt redisplay-skip-fontification-on-input t)

;; Modern LSP servers (rust-analyzer, clangd, …) routinely send
;; multi-megabyte responses.  Bumping the read buffer cuts the number
;; of read calls dramatically.  4 MB is what Doom, Purcell, and Centaur
;; all ship.
(setopt read-process-output-max (* 4 1024 1024))

;; Don't draw cursors or highlight selections in non-focused windows.
;; Saves a small amount of rendering work and removes a visual
;; distraction in multi-window layouts.
(setq-default cursor-in-non-selected-windows nil)
(setopt highlight-nonselected-windows nil)

;;;; Ergonomic hacks

;; Save the system clipboard into the kill ring before killing, so a
;; URL copied from the browser isn't lost the moment you `C-k' a line
;; in Emacs — `M-y' walks back to it.
(setopt save-interprogram-paste-before-kill t)

;; Killing the same text repeatedly should not waste slots in the
;; kill ring.
(setopt kill-do-not-save-duplicates t)

;; Auto-chmod files with a `#!' shebang on save so freshly-written
;; scripts are immediately executable.
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; Stop `ffap' from pinging hostnames it spots under point — that
;; freezes Emacs for several seconds on slow or firewalled networks.
;; The defcustom only exists once `ffap' is loaded.
(with-eval-after-load 'ffap
  (setopt ffap-machine-p-known 'reject))

;; Resize all sibling windows proportionally when splitting, instead
;; of always halving the current window.  Keeps multi-window layouts
;; balanced.
(setopt window-combination-resize t)

;; After `C-u C-SPC' once, allow plain `C-SPC' to keep popping the
;; mark ring.  Makes mark-ring navigation much less awkward.
(setopt set-mark-command-repeat-pop t)

;; Recenter the buffer after `save-place-mode' restores the cursor —
;; otherwise reopening a file can leave point on the bottom line of
;; the window, which is disorienting.  `save-place-find-file-hook'
;; runs from `find-file-hook' before the new buffer has actually been
;; displayed, so a direct `recenter' would act on the previously
;; selected window.  Defer via a zero-delay timer until the window
;; showing the buffer exists.
(defun jotain-performance--recenter-buffer-window (buffer)
  "Recenter the window currently displaying BUFFER, if any."
  (when-let* ((win (get-buffer-window buffer)))
    (with-selected-window win
      (ignore-errors (recenter)))))

(defun jotain-performance--recenter-after-save-place (&rest _)
  "Schedule a recenter after `save-place-mode' restores point."
  (when buffer-file-name
    (run-with-timer 0 nil
                    #'jotain-performance--recenter-buffer-window
                    (current-buffer))))

(with-eval-after-load 'saveplace
  (advice-add 'save-place-find-file-hook :after
              #'jotain-performance--recenter-after-save-place))

;; Reversible `C-x 1': first press collapses the frame to a single
;; window, second press restores the previous layout via `winner-mode'.
(winner-mode 1)

(defun jotain-performance-toggle-delete-other-windows ()
  "Delete other windows in the frame, or restore the previous layout.
If only one window is visible and `winner-mode' has a previous
configuration to restore, this command undoes the deletion instead."
  (interactive)
  (if (and winner-mode (one-window-p))
      (winner-undo)
    (delete-other-windows)))

(keymap-global-set "C-x 1" #'jotain-performance-toggle-delete-other-windows)

(provide 'jotain-performance)
;;; jotain-performance.el ends here
