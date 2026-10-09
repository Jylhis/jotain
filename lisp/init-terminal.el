;;; init-terminal.el --- Ghostel terminal + tty integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Both directions of terminal support:
;;
;; - A terminal inside Emacs: ghostel (libghostty-vt via a native module).
;; - Emacs inside a terminal: kkp (Kitty Keyboard Protocol), clipetty
;;   (OSC 52 clipboard), xterm-mouse, tty-tip.  No-ops in GUI frames.
;;
;; ghostel advertises TERM=xterm-ghostty with Kitty-keyboard and OSC 52
;; support, so a nested `emacs -nw' in a ghostel buffer gets both.
;;
;; The xterm-ghostty TERM alias lives in early-init.el because
;; `tty-run-terminal-initialization' runs before init.el loads.

;;; Code:

;;;; Terminal emulator inside Emacs

;;; @doc Terminal emulator on libghostty-vt (Ghostty's VT engine) via a
;;; native module: a real PTY for tmux/ncurses/TUI programs, plus shell
;;; integration (OSC 7 directory tracking, OSC 133 prompt jumping) for
;;; bash/zsh/fish. A missing module is downloaded into the package
;;; directory on first `M-x ghostel', which needs that directory to be
;;; writable (e.g. a MELPA install under elpa/).
(use-package ghostel
  :commands (ghostel ghostel-project ghostel-other)
  :custom
  ;; `ghostel-module-directory' stays nil (the package directory).  A
  ;; present module is only loaded, never rewritten, so a read-only store
  ;; path is fine; auto-install only fires when the module is missing.
  (ghostel-module-auto-install 'download)
  ;; Programs in the terminal reach the system clipboard via OSC 52.
  (ghostel-enable-osc52 t))

;;;; Terminal compatibility (no-ops in GUI)

;;; @doc Kitty Keyboard Protocol: terminal Emacs can distinguish C-i/TAB,
;;; C-m/RET, C-[/ESC and receive Shift-modified function keys. Loaded in
;;; tty and daemon sessions (a daemon may serve `emacsclient -nw'
;;; frames); pure GUI sessions skip it.
(use-package kkp
  :if (or (daemonp) (not (display-graphic-p)))
  :hook (after-init . global-kkp-mode))

;;; @doc OSC 52 clipboard integration: kills in terminal Emacs reach the
;;; system clipboard, also over ssh and tmux.
(use-package clipetty
  :hook (after-init . global-clipetty-mode))

(defun jotain-terminal--tty-setup ()
  "Per-frame tty setup hook."
  (xterm-mouse-mode 1))
(add-hook 'tty-setup-hook #'jotain-terminal--tty-setup)

;;; @doc Emacs 31+: tooltips (help-echo, button hints) in terminal frames.
;;; No-op in GUI; skipped on Emacs 30.
(when (fboundp 'tty-tip-mode)
  (tty-tip-mode 1))

(provide 'init-terminal)
;;; init-terminal.el ends here
