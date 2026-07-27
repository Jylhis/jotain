;;; init-terminal.el --- Ghostel terminal + tty integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Both directions of terminal support live here:
;;
;; - A terminal *inside* Emacs: ghostel, powered by libghostty-vt
;;   (Ghostty's VT engine) through a native dynamic module.
;; - Emacs *inside* a terminal: kkp (Kitty Keyboard Protocol), clipetty
;;   (OSC 52 clipboard), xterm-mouse, tty-tip.  All no-ops in GUI frames.
;;
;; The two meet in the middle: ghostel's terminal advertises
;; TERM=xterm-ghostty with Kitty-keyboard and OSC 52 support, which is
;; exactly what kkp and clipetty speak — so a nested `emacs -nw' inside
;; a ghostel buffer gets full key fidelity and clipboard access.
;;
;; The xterm-ghostty → xterm-256color TERM alias stays in early-init.el
;; because `tty-run-terminal-initialization' fires before init.el loads.

;;; Code:

;;;; Terminal emulator inside Emacs

;;; @doc Terminal emulator powered by libghostty-vt (the Ghostty VT
;;; engine) via a native dynamic module.  Replaces vterm: real PTY for
;;; tmux/ncurses/TUI programs, plus shell integration (OSC 7 directory
;;; tracking, OSC 133 prompt jumping) injected automatically for
;;; bash/zsh/fish.  The Nix package builds the module from source and
;;; ships it in the package directory; in a source checkout the module
;;; is downloaded into the writable elpa/ dir on first `M-x ghostel'.
(use-package ghostel
  :commands (ghostel ghostel-project ghostel-other)
  :custom
  ;; `ghostel-module-directory' stays nil (= package directory): that
  ;; is where nixpkgs ships the module, and a present module is only
  ;; loaded, never re-written, so the read-only store is safe.
  ;; Auto-install fires only when the module is missing — i.e. only on
  ;; the source-checkout/MELPA path where elpa/ is writable.
  (ghostel-module-auto-install 'download)
  ;; Unified clipboard story with clipetty below: programs inside the
  ;; terminal reach the system clipboard via OSC 52.
  (ghostel-enable-osc52 t))

;;;; Terminal compatibility (no-ops in GUI)

;;; @doc Kitty Keyboard Protocol — lets terminal Emacs distinguish
;;; C-i/TAB, C-m/RET, C-[/ESC, and pass Shift-modified function
;;; keys through. Only loaded in tty and daemon sessions — the
;;; daemon arm matters because a daemon can later serve
;;; `emacsclient -nw' frames; pure GUI sessions skip it entirely.
(use-package kkp
  :if (or (daemonp) (not (display-graphic-p)))
  :hook (after-init . global-kkp-mode))

;;; @doc OSC 52 clipboard integration. Yank/kill in terminal Emacs
;;; reaches the system clipboard even through ssh + tmux.
(use-package clipetty
  :hook (after-init . global-clipetty-mode))

(defun jotain-terminal--tty-setup ()
  "Per-frame tty setup hook."
  (xterm-mouse-mode 1))
(add-hook 'tty-setup-hook #'jotain-terminal--tty-setup)

;;; @doc Emacs 31+: bring tooltips (help-echo, button hints) to terminal
;;; frames, which previously had none. No-op in GUI. Guarded with
;;; `fboundp' so the config still loads on Emacs 30.
(when (fboundp 'tty-tip-mode)
  (tty-tip-mode 1))

(provide 'init-terminal)
;;; init-terminal.el ends here
