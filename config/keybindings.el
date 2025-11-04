;;; keybindings.el --- Mnemonic keybinding system -*- lexical-binding: t; -*-

;;; Commentary:
;; Mnemonic keybinding system inspired by Doom Emacs and Spacemacs patterns.
;; Uses M-SPC as leader key (non-Evil friendly) for consistent, discoverable bindings.
;; SPC m for major-mode-specific commands provides language-agnostic muscle memory.

;;; Code:

(use-package general
  :ensure
  :demand t
  :config
  ;; Create leader key (M-SPC for non-Evil usage)
  (general-create-definer my/leader-def
    :prefix "M-SPC")

  ;; Create major-mode leader (M-SPC m)
  (general-create-definer my/local-leader-def
    :prefix "M-SPC m")

  ;; Core keybindings following Doom/Spacemacs patterns
  (my/leader-def
    "" '(:ignore t :which-key "leader")

    ;; File operations (f prefix)
    "f" '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save")
    "fS" '(write-file :which-key "save as")
    "fr" '(consult-recent-file :which-key "recent files")
    "fD" '(delete-file :which-key "delete file")
    "fR" '(rename-file :which-key "rename file")
    "fy" '((lambda () (interactive) (kill-new (buffer-file-name))) :which-key "yank path")

    ;; Buffer operations (b prefix)
    "b" '(:ignore t :which-key "buffers")
    "bb" '(consult-buffer :which-key "switch buffer")
    "bd" '(kill-current-buffer :which-key "delete buffer")
    "bk" '(kill-buffer :which-key "kill buffer")
    "br" '(revert-buffer :which-key "revert")
    "bR" '(rename-buffer :which-key "rename")
    "bs" '(save-buffer :which-key "save")
    "bS" '((lambda () (interactive) (save-some-buffers t)) :which-key "save all")
    "by" '((lambda () (interactive) (kill-new (buffer-name))) :which-key "yank name")

    ;; Project operations (p prefix) - using projection
    "p" '(:ignore t :which-key "projects")
    "pf" '(project-find-file :which-key "find file")
    "pp" '(project-switch-project :which-key "switch project")
    "ps" '(consult-ripgrep :which-key "search project")
    "pb" '(project-switch-to-buffer :which-key "switch buffer")
    "pd" '(project-dired :which-key "dired")
    "pk" '(project-kill-buffers :which-key "kill buffers")
    "pc" '(project-compile :which-key "compile")

    ;; Git operations (g prefix)
    "g" '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "status")
    "gl" '(magit-log-current :which-key "log")
    "gL" '(magit-log :which-key "log all")
    "gb" '(magit-blame :which-key "blame")
    "gd" '(magit-diff-unstaged :which-key "diff")
    "gD" '(magit-diff :which-key "diff all")
    "gs" '(magit-stage-file :which-key "stage file")
    "gS" '(magit-stage :which-key "stage")
    "gc" '(magit-commit :which-key "commit")
    "gp" '(magit-push :which-key "push")
    "gP" '(magit-pull :which-key "pull")
    "gf" '(magit-fetch :which-key "fetch")
    "gF" '(magit-find-file :which-key "find file")

    ;; Search operations (s prefix)
    "s" '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "search buffer")
    "sS" '(consult-line-multi :which-key "search buffers")
    "sp" '(consult-ripgrep :which-key "search project")
    "sd" '(consult-fd :which-key "find file")
    "si" '(consult-imenu :which-key "imenu")
    "sI" '(consult-imenu-multi :which-key "imenu all")
    "so" '(consult-outline :which-key "outline")
    "sg" '(consult-grep :which-key "grep")
    "sG" '(consult-git-grep :which-key "git grep")

    ;; Window operations (w prefix)
    "w" '(:ignore t :which-key "windows")
    "ww" '(other-window :which-key "next")
    "wd" '(delete-window :which-key "delete")
    "wD" '(delete-other-windows :which-key "delete others")
    "w/" '(split-window-right :which-key "split right")
    "w-" '(split-window-below :which-key "split below")
    "wh" '(windmove-left :which-key "left")
    "wj" '(windmove-down :which-key "down")
    "wk" '(windmove-up :which-key "up")
    "wl" '(windmove-right :which-key "right")
    "w=" '(balance-windows :which-key "balance")
    "wu" '(winner-undo :which-key "undo")
    "wr" '(winner-redo :which-key "redo")

    ;; Toggle operations (t prefix)
    "t" '(:ignore t :which-key "toggles")
    "tl" '(display-line-numbers-mode :which-key "line numbers")
    "tw" '(whitespace-mode :which-key "whitespace")
    "tt" '(consult-theme :which-key "theme")
    "tf" '(toggle-frame-fullscreen :which-key "fullscreen")
    "tm" '(toggle-frame-maximized :which-key "maximize")
    "tv" '(visual-line-mode :which-key "visual line")
    "tT" '(modus-themes-toggle :which-key "toggle modus theme")

    ;; Help operations (h prefix)
    "h" '(:ignore t :which-key "help")
    "hf" '(describe-function :which-key "function")
    "hv" '(describe-variable :which-key "variable")
    "hk" '(describe-key :which-key "key")
    "hm" '(describe-mode :which-key "mode")
    "hp" '(describe-package :which-key "package")
    "hb" '(embark-bindings :which-key "bindings")
    "hF" '(describe-face :which-key "face")
    "hK" '(describe-keymap :which-key "keymap")
    "hi" '(info :which-key "info")
    "hI" '(consult-info :which-key "consult info")

    ;; Quit operations (q prefix)
    "q" '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-terminal :which-key "quit emacs")
    "qQ" '(kill-emacs :which-key "quit no save")
    "qr" '(restart-emacs :which-key "restart")

    ;; Open operations (o prefix)
    "o" '(:ignore t :which-key "open")
    "ot" '(vterm :which-key "terminal")
    "od" '(dired-jump :which-key "dired")
    "op" '(project-dired :which-key "project dired")

    ;; Code operations (c prefix) - general programming
    "c" '(:ignore t :which-key "code")
    "cc" '(compile :which-key "compile")
    "cC" '(recompile :which-key "recompile")
    "cx" '(compile-multi :which-key "compile multi")

    ;; Evaluate/Execute (e prefix)
    "e" '(:ignore t :which-key "evaluate")
    "eb" '(eval-buffer :which-key "eval buffer")
    "ee" '(eval-last-sexp :which-key "eval sexp")
    "er" '(eval-region :which-key "eval region")

    ;; Errors/Diagnostics (! prefix mapped to x for easier access)
    "x" '(:ignore t :which-key "diagnostics")
    "xl" '(consult-flymake :which-key "list errors")
    "xn" '(flymake-goto-next-error :which-key "next error")
    "xp" '(flymake-goto-prev-error :which-key "previous error")
    "xb" '(flymake-show-buffer-diagnostics :which-key "buffer diagnostics")
    "xP" '(flymake-show-project-diagnostics :which-key "project diagnostics"))

  ;; Generic major-mode keybindings that work everywhere
  ;; Specific language configs will add more bindings
  (my/local-leader-def
    "" '(:ignore t :which-key "major mode")

    ;; Goto operations (g prefix) - common across all languages
    "g" '(:ignore t :which-key "goto")
    "gg" '(xref-find-definitions :which-key "definition")
    "gG" '(xref-find-definitions-other-window :which-key "definition other")
    "gr" '(xref-find-references :which-key "references")
    "gb" '(xref-go-back :which-key "go back")
    "gf" '(xref-go-forward :which-key "go forward")

    ;; Help/Documentation (h prefix)
    "h" '(:ignore t :which-key "help")
    "hh" '(eldoc-doc-buffer :which-key "doc at point")

    ;; Refactor operations (r prefix)
    "r" '(:ignore t :which-key "refactor")
    "rr" '(eglot-rename :which-key "rename")

    ;; Format (=)
    "=" '(eglot-format-buffer :which-key "format")
    "==" '(eglot-format-buffer :which-key "format buffer")
    "=r" '(eglot-format :which-key "format region")

    ;; Actions (a prefix)
    "a" '(:ignore t :which-key "actions")
    "aa" '(eglot-code-actions :which-key "code actions")
    "ao" '(eglot-code-action-organize-imports :which-key "organize imports")
    "aq" '(eglot-code-action-quickfix :which-key "quickfix")))

;; Window management Hydra for modal window operations
(use-package hydra
  :ensure
  :defer t)

(defhydra hydra-window (:color pink :hint nil)
  "
^Navigate^      ^Split^         ^Resize^        ^Other^
_h_: ←         _/_: vertical   _H_: ← wider    _d_: delete
_j_: ↓         _-_: horizontal _J_: ↓ taller   _o_: delete others
_k_: ↑         _=_: balance    _K_: ↑ shorter  _u_: winner undo
_l_: →                         _L_: → narrower _r_: winner redo
_w_: next                                      _q_: quit
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("w" other-window)
  ("/" split-window-right)
  ("-" split-window-below)
  ("H" shrink-window-horizontally)
  ("J" enlarge-window)
  ("K" shrink-window)
  ("L" enlarge-window-horizontally)
  ("=" balance-windows)
  ("d" delete-window)
  ("o" delete-other-windows :exit t)
  ("u" winner-undo)
  ("r" winner-redo)
  ("q" nil :exit t))

;; Bind hydra to leader key
(my/leader-def
  "w." '(hydra-window/body :which-key "window hydra"))

(provide 'keybindings)
;;; keybindings.el ends here
