;;; init-keys.el --- Global keymap and leader-key setup -*- lexical-binding: t; -*-

;;; Commentary:

;; Only global keybindings live here.  Per-package bindings stay in
;; their `use-package' block (`:bind', `:bind-keymap'), so removing a
;; package removes its keys.

;;; Code:

(defun jotain-toggle-window-split ()
  "Toggle between horizontal and vertical window split.
Works with exactly two windows; their buffers and focus are kept."
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

;; The default `keyboard-quit' doesn't close a minibuffer that isn't the
;; selected window, a frequent papercut with recursive minibuffers.
(defun jotain-keyboard-quit-dwim ()
  "Do-What-I-Mean `keyboard-quit'.
Minibuffer open (even when point is in another window) → abort
recursive edit (this also dismisses any `*Completions*' popup).
Completions window visible with no minibuffer (user popped it via
`display-completion-list' or focused it directly) → close it.
Otherwise call regular `keyboard-quit'."
  (interactive)
  (cond
   ((> (minibuffer-depth) 0)                     (abort-recursive-edit))
   ((get-buffer-window "*Completions*" 'visible) (delete-completion-window))
   (t                                            (keyboard-quit))))

(use-package emacs
  :ensure nil
  :bind
  (("C-z" . nil)
   ("C-x C-z" . nil)
   ([remap keyboard-quit] . jotain-keyboard-quit-dwim)
   ("M-o" . other-window)
   ("C-x j" . jotain-toggle-window-split)))

(dolist (key '("<menu-bar> <tools> <rmail>"          ; Read Mail
               "<menu-bar> <tools> <gnus>"           ; Read Net News
               "<menu-bar> <tools> <games>"          ; Games (incl. doctor)
               "<menu-bar> <help-menu> <describe-distribution>")) ; Getting New Versions
  (keymap-global-unset key))

;; Disable the commands so invoking them via M-x prompts first.
(dolist (cmd '(rmail gnus describe-distribution
               doctor 5x5 blackbox bubbles dunnet gomoku hanoi life
               mpuz pong snake solitaire tetris zone))
  (put cmd 'disabled t))

(use-package windmove
  :ensure nil
  :config (windmove-default-keybindings))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    ;; C-c <letter>
    "C-c a"     "org-agenda"
    "C-c c"     "org-capture"
    "C-c d"     "dirvish"
    "C-c D"     "dirvish-side"
    "C-c e"     "eca"
    "C-c g"     "magit-file"
    "C-c h"     "consult-history"
    "C-c i"     "consult-info"
    "C-c j"     "jujutsu"
    "C-c k"     "consult-kmacro"
    "C-c l"     "org-store-link"
    "C-c m"     "consult-man"
    "C-c n"     "org-roam"
    "C-c o"     "combobulate"
    "C-c q"     "claude-code-ide"
    "C-c r"     "eglot-refactor"
    "C-c s"     "gptel-send"
    "C-c S"     "gptel-menu"
    "C-c t"     "toggle-theme"
    "C-c v"     "devenv"
    ;; C-c <modified>
    "C-c M-j"   "jujutsu-dispatch"
    "C-c M-x"   "consult-mode-command"
    ;; C-x
    "C-x C-a"   "dape (debug)"
    "C-x g"     "magit-status"
    "C-x G"     "git-status-file"
    "C-x M-g"   "magit-dispatch"
    "C-x j"     "jotain-toggle-window-split"
    "C-x J"     "jj-status-file"
    "C-x u"     "vundo"
    "C-x P"     "project (projection)"))

;;;; Repeat maps

;; Emacs binds the window-tree transforms under `C-x w' with arrow keys;
;; this adds single-letter keys under `C-x W' so `C-x W r r r' keeps
;; rotating.
(defvar-keymap jotain-window-layout-repeat-map
  :doc "Repeat map for `window-layout-*' frame transforms."
  :repeat t
  "t" #'window-layout-transpose
  "r" #'window-layout-rotate-clockwise
  "R" #'window-layout-rotate-anticlockwise
  "h" #'window-layout-flip-leftright
  "v" #'window-layout-flip-topdown)
(keymap-global-set "C-x W" jotain-window-layout-repeat-map)

(provide 'init-keys)
;;; init-keys.el ends here
