;;; init-terminal.el --- Ghostel terminal + tty integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

(defun my-terminal--tty-setup ()
  "Per-frame tty setup hook."
  (xterm-mouse-mode 1))
(add-hook 'tty-setup-hook #'my-terminal--tty-setup)

;;; @doc Tooltips (help-echo, button hints) in terminal frames. No-op in
;;; GUI.
(tty-tip-mode 1)

(provide 'init-terminal)
;;; init-terminal.el ends here
