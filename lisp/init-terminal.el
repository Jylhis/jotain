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

(use-package ghostel
  :commands (ghostel ghostel-project ghostel-other)
  :custom
  ;; `ghostel-module-directory' stays nil (the package directory).  A
  ;; present module is only loaded, never rewritten, so a read-only store
  ;; path is fine; auto-install only fires when the module is missing.
  (ghostel-module-auto-install 'download)
  (ghostel-enable-osc52 t))

;;;; Terminal compatibility (no-ops in GUI)

(use-package kkp
  :if (or (daemonp) (not (display-graphic-p)))
  :hook (after-init . global-kkp-mode))

(use-package clipetty
  :hook (after-init . global-clipetty-mode))

(defun jotain-terminal--tty-setup ()
  "Per-frame tty setup hook."
  (xterm-mouse-mode 1))
(add-hook 'tty-setup-hook #'jotain-terminal--tty-setup)

(tty-tip-mode 1)

(provide 'init-terminal)
;;; init-terminal.el ends here
