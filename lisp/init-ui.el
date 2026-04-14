;;; init-ui.el --- Theme, modeline, fonts, frame tweaks -*- lexical-binding: t; -*-

;;; Commentary:

;; Everything that affects what the editor *looks* like: theme, modeline,
;; fonts, line numbers, smooth scrolling, frame parameters. Both built-in
;; (`display-line-numbers', `pixel-scroll', `which-key', `paren') and
;; third-party (`doom-modeline', `pulsar', `nerd-icons') live here together.

;;; Code:

(defgroup jotain-ui nil
  "User-facing UI knobs for the Jotain configuration."
  :group 'convenience)

;;;; Theme — modus operandi/vivendi tinted, switched by system appearance

(defcustom jotain-theme-light 'modus-operandi-tinted
  "Theme to use when the system is in light mode."
  :type 'symbol
  :group 'jotain-ui)

(defcustom jotain-theme-dark 'modus-vivendi-tinted
  "Theme to use when the system is in dark mode."
  :type 'symbol
  :group 'jotain-ui)

;; Trust all themes by default — modus is built-in and signed, and we
;; never load themes from disk paths we don't control.
(setopt custom-safe-themes t)

(defun jotain-ui--disable-other-themes (_theme &optional _no-confirm no-enable)
  "Disable any active themes before loading a new one.
Without this, switching themes layers the new one on top of the old
and the result is a face-attribute soup."
  (unless no-enable
    (mapc #'disable-theme (copy-sequence custom-enabled-themes))))

(advice-add 'load-theme :before #'jotain-ui--disable-other-themes)

(use-package modus-themes
  :demand t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-mixed-fonts t)
  :config
  ;; Pre-load both themes so auto-dark can flip between them without
  ;; re-evaluating the .el files on every appearance change.
  (load-theme jotain-theme-light t t)
  (load-theme jotain-theme-dark  t t))

(use-package auto-dark
  :diminish
  :demand t
  :after modus-themes
  :bind ("C-c t" . auto-dark-toggle-appearance)
  :custom
  (auto-dark-allow-osascript t)
  (auto-dark-themes `((,jotain-theme-dark) (,jotain-theme-light)))
  :config
  (auto-dark-mode 1))

;;;; Modeline

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 4)
  (doom-modeline-icon t)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-buffer-encoding nil))

;;;; Fonts — minimal version: pick first available monospace

(defcustom jotain-font-preferences
  '(("JetBrainsMono Nerd Font" . 130)
    ("Iosevka Nerd Font"       . 130)
    ("DejaVu Sans Mono"        . 120))
  "Ordered list of (FAMILY . HEIGHT) pairs to try for the default face.
The first family that is actually installed wins."
  :type '(alist :key-type string :value-type integer)
  :group 'jotain-ui)

(defun jotain-ui-apply-font (&optional _frame)
  "Set the default face to the first available font in `jotain-font-preferences'."
  (cl-loop for (family . height) in jotain-font-preferences
           when (find-font (font-spec :family family))
           return (set-face-attribute 'default nil
                                      :family family
                                      :height height)))

(jotain-ui-apply-font)
(add-hook 'server-after-make-frame-hook #'jotain-ui-apply-font)

;;;; Built-in display tweaks

;; Don't draw cursors or highlight selections in non-focused windows.
(setopt cursor-in-non-selected-windows nil)
(setopt highlight-nonselected-windows nil)

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode conf-mode) . display-line-numbers-mode))

(use-package pixel-scroll
  :ensure nil
  :config (pixel-scroll-precision-mode 1))

(use-package hl-line
  :ensure nil
  :hook ((prog-mode conf-mode text-mode) . hl-line-mode))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package which-key
  :ensure nil
  :diminish
  :config (which-key-mode 1))

(use-package calendar
  :ensure nil
  :defer t
  :custom (calendar-week-start-day 1) ; Monday
  :config
  ;; ISO week numbers in the gutter.
  (copy-face 'font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil :height 0.7)
  (setopt calendar-intermonth-text
          '(propertize
            (format "%2d" (car (calendar-iso-from-absolute
                                (calendar-absolute-from-gregorian
                                 (list month day year)))))
            'font-lock-face 'calendar-iso-week-face)))

;;;; Icons (Nerd Font glyphs in dired, ibuffer, corfu, marginalia)

(use-package nerd-icons
  :config
  (when (display-graphic-p)
    (when-let* ((nerd-font
                 (cl-loop for (family . _height) in jotain-font-preferences
                          when (and (string-match-p "Nerd Font" family)
                                    (find-font (font-spec :family family)))
                          return family)))
      (setopt nerd-icons-font-family nerd-font))))

(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-completion
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


(use-package nerd-icons-ibuffer
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;; Polish packages

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces
   '(("TODO"       . "#cc9393")
     ("FIXME"      . "#ff0000")
     ("HACK"       . "#d0bf8f")
     ("REVIEW"     . "#7cb8bb")
     ("NOTE"       . "#7f9f7f")
     ("DEPRECATED" . "#cc9393"))))

(use-package breadcrumb
  :hook (prog-mode . breadcrumb-local-mode))

(use-package pulsar
  :demand t
  :custom
  (pulsar-pulse-functions
   '(recenter-top-bottom move-to-window-line-top-bottom reposition-window
     bookmark-jump other-window delete-window delete-other-windows
     forward-page backward-page scroll-up-command scroll-down-command
     xref-find-definitions xref-find-references xref-go-back
     consult-line consult-goto-line imenu))
  :config (pulsar-global-mode 1))

(use-package rainbow-delimiters
  :hook ((lisp-mode emacs-lisp-mode) . rainbow-delimiters-mode))

(use-package indent-bars
  :custom (indent-bars-treesit-support t)
  :hook (prog-mode . indent-bars-mode))

;;;; Terminal compatibility (no-ops in GUI)

;; Kitty Keyboard Protocol: lets Emacs distinguish C-i/TAB, C-m/RET,
;; C-[/ESC, and pass through Shift-modified function keys. global-kkp-mode
;; is itself a no-op in GUI frames, so it's safe to enable unconditionally.
(use-package kkp
  :config (global-kkp-mode 1))

;; OSC 52 clipboard integration that works through ssh + tmux.
(use-package clipetty
  :diminish
  :hook (after-init . global-clipetty-mode))

(defun jotain-ui--tty-setup ()
  "Per-frame tty setup hook."
  (xterm-mouse-mode 1))
(add-hook 'tty-setup-hook #'jotain-ui--tty-setup)

(provide 'init-ui)
;;; init-ui.el ends here
