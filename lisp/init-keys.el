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
Minibuffer open (even when point is in another window) → abort
recursive edit (this also dismisses any `*Completions*' popup).
Completions window visible with no minibuffer (user popped it via
`display-completion-list' or focused it directly) → close it.
Region active → deactivate it.  Otherwise call regular
`keyboard-quit'."
  (interactive)
  (cond
   ((> (minibuffer-depth) 0)                     (abort-recursive-edit))
   ((get-buffer-window "*Completions*" 'visible) (delete-completion-window))
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

;;; @doc Built-in directional window switching — `Shift-<arrow>` moves
;;; focus between split windows. Ships with Emacs; no reason not
;;; to turn it on globally.
(use-package windmove
  :ensure nil
  :config (windmove-default-keybindings))

;;; @doc Prefix labels for `which-key'. Adopts the "memorable,
;;; discoverable, categorised" spirit of leader-key frameworks like
;;; `general.el' without the dependency: each prefix below acquires a
;;; short noun-phrase label that `which-key' surfaces in place of
;;; `+prefix' when the user pauses after the prefix key. The actual
;;; bindings themselves stay colocated with their `use-package' blocks
;;; (per the convention documented at the top of this file); only the
;;; cross-cutting prefix metadata lives here.
;;;
;;; `which-key' is enabled in init-ui.el — we register through
;;; `with-eval-after-load' so this module can stay independent of load
;;; order while still taking effect the moment which-key starts.
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    ;; C-c <letter> — global user namespace.
    "C-c a"     "org-agenda"
    "C-c c"     "org-capture"
    "C-c d"     "dirvish"
    "C-c D"     "dirvish-side"
    "C-c g"     "magit-file"
    "C-c h"     "consult-history"
    "C-c i"     "consult-info"
    "C-c j"     "jujutsu"
    "C-c k"     "consult-kmacro"
    "C-c l"     "org-store-link"
    "C-c n"     "org-roam"
    "C-c o"     "combobulate"
    "C-c r"     "eglot-refactor"
    "C-c t"     "toggle-theme"
    ;; C-c <punct> — AI and special leaves.
    "C-c RET"   "gptel-send"
    "C-c M-RET" "gptel-menu"
    "C-c M-j"   "jujutsu-dispatch"
    "C-c C-'"   "claude-code-ide"
    "C-c M-x"   "consult-mode-command"
    ;; C-x namespace.
    "C-x g"     "magit-status"
    "C-x M-g"   "magit-dispatch"
    "C-x j"     "rotate-window-split"
    "C-x u"     "vundo"
    "C-x P"     "project (projection)"))

;;;; Repeat maps — Emacs-native "one-shot modifier" pattern
;;
;; `repeat-mode' (enabled in init-core.el) lets a tagged command be
;; repeated with single keystrokes immediately after its first
;; invocation: type the chord once, then keep typing the trailing
;; key.  Many built-in commands ship their own repeat-maps already
;; (`other-window', `next-buffer', `undo', `next-error', …) — so
;; `M-o o o' and `C-/ /' Just Work.  The maps below fill the
;; remaining gap for commands that don't have one out of the box.
;; See the "Ergonomics" chapter of the Info manual for background.

;; After `C-x ^', `C-x }', or `C-x {', single `^', `v', `}', or `{'
;; keystrokes keep resizing the window. `shrink-window' has no
;; default key binding but joins the overlay once any other entry
;; command has fired, so `C-x ^ ^ v v' overshoots and corrects.
;; Cleared by `repeat-exit-timeout' (2 s, set in init-core.el).
(defvar-keymap jotain-window-resize-repeat-map
  :doc "Repeat map for window-resize commands."
  :repeat t
  "^" #'enlarge-window
  "v" #'shrink-window
  "}" #'enlarge-window-horizontally
  "{" #'shrink-window-horizontally)

;; Emacs 31+: transpose/rotate/flip the whole window tree without
;; manually deleting and re-splitting. Bound under the unused `C-x W'
;; prefix (capital, so it doesn't clobber the `C-x w' hi-lock map) with
;; a repeat map, so `C-x W r r r' keeps rotating. Guarded because these
;; commands don't exist before Emacs 31; the command symbols are quoted
;; (not `#''), so byte-compiling on Emacs 30 sees data, not an unknown
;; function reference.
(when (fboundp 'window-layout-transpose)
  (defvar-keymap jotain-window-layout-repeat-map
    :doc "Repeat map for `window-layout-*' frame transforms."
    :repeat t
    "t" 'window-layout-transpose
    "r" 'window-layout-rotate-clockwise
    "R" 'window-layout-rotate-counterclockwise
    "h" 'window-layout-flip-leftright
    "v" 'window-layout-flip-topdown)
  (keymap-global-set "C-x W" jotain-window-layout-repeat-map))

(provide 'init-keys)
;;; init-keys.el ends here
