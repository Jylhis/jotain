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

;;;; Theme — Jylhis paper/roast, switched by system appearance

(defcustom jotain-theme-light 'jylhis-paper
  "Theme to use when the system is in light mode."
  :type 'symbol
  :group 'jotain-ui)

(defcustom jotain-theme-dark 'jylhis-roast
  "Theme to use when the system is in dark mode."
  :type 'symbol
  :group 'jotain-ui)

;; Trust all themes by default — we only load our own Jylhis themes
;; and never load themes from disk paths we don't control.
(setopt custom-safe-themes t)

(defun jotain-ui--disable-other-themes (_theme &optional _no-confirm no-enable)
  "Disable any active themes before loading a new one.
Without this, switching themes layers the new one on top of the old
and the result is a face-attribute soup."
  (unless no-enable
    (mapc #'disable-theme (copy-sequence custom-enabled-themes))))

(advice-add 'load-theme :before #'jotain-ui--disable-other-themes)

;; Register the Jylhis theme directory on custom-theme-load-path.
;; The package ships jylhis-themes.el as the entry point for this.
(require 'jylhis-themes)

;; Pre-load both themes so auto-dark can flip between them without
;; re-evaluating the .el files on every appearance change.  Guarded
;; against batch mode where custom-theme-load-path may be incomplete.
(unless noninteractive
  (load-theme jotain-theme-light t t)
  (load-theme jotain-theme-dark  t t))

;;; @doc Flips between `jotain-theme-light` and `jotain-theme-dark`
;;; following the system appearance — works on macOS, GNOME, and
;;; anything that exposes a dark/light setting. C-c t toggles
;;; manually.
(use-package auto-dark
  :diminish
  :demand t
  :bind ("C-c t" . auto-dark-toggle-appearance)
  :custom
  (auto-dark-allow-osascript t)
  (auto-dark-themes `((,jotain-theme-dark) (,jotain-theme-light)))
  :config
  (auto-dark-mode 1))

;;;; Modeline

;;; @doc A dense, IDE-style modeline with LSP/eglot status, project
;;; buffer info, and Nerd Font glyphs. Loaded after init so the
;;; primary frame doesn't redraw before fonts are ready.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 4)
  (doom-modeline-icon t)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-buffer-encoding nil))

;;;; Fonts

(defcustom jotain-font-scale 1.0
  "Multiplier applied to every height in the font preference lists.
Increase above 1.0 on large or high-density displays where the
default sizes feel small (e.g. set 1.25 in custom.el for 4K)."
  :type 'float
  :group 'jotain-ui)

(defcustom jotain-font-preferences
  '(("JetBrainsMono Nerd Font" . 140)
    ("Iosevka Nerd Font"       . 140)
    ("DejaVu Sans Mono"        . 130))
  "Ordered list of (FAMILY . HEIGHT) pairs to try for the default face.
HEIGHT is in 1/10 pt units (140 = 14 pt).  The first installed family
wins.  All heights are multiplied by `jotain-font-scale' at runtime."
  :type '(alist :key-type string :value-type integer)
  :group 'jotain-ui)

(defcustom jotain-variable-pitch-font-preferences
  '(("Literata"     . 150)
    ("Iosevka Aile" . 150)
    ("Noto Sans"    . 150)
    ("DejaVu Sans"  . 140))
  "Ordered list of (FAMILY . HEIGHT) pairs to try for the variable-pitch face.
Heights are larger than the monospace default: proportional fonts render
visually smaller at the same point size."
  :type '(alist :key-type string :value-type integer)
  :group 'jotain-ui)

(defun jotain-ui-apply-font (&optional frame)
  "Set default and variable-pitch faces; honours `jotain-font-scale'.
FRAME is used to probe font availability on the right display; face
attributes are applied globally so all frames see the update."
  (when (display-graphic-p frame)
    (cl-loop for (family . height) in jotain-font-preferences
             when (find-font (font-spec :family family) frame)
             return (set-face-attribute 'default nil
                                        :family family
                                        :height (round (* height jotain-font-scale))))
    (cl-loop for (family . height) in jotain-variable-pitch-font-preferences
             when (find-font (font-spec :family family) frame)
             return (set-face-attribute 'variable-pitch nil
                                        :family family
                                        :height (round (* height jotain-font-scale))))))

(jotain-ui-apply-font)
(add-hook 'server-after-make-frame-hook #'jotain-ui-apply-font)

;;;; Built-in display tweaks

;; Don't draw cursors or highlight selections in non-focused windows.
(setopt cursor-in-non-selected-windows nil)
(setopt highlight-nonselected-windows nil)

(defcustom jotain-line-numbers-in-prog t
  "When non-nil, show line numbers in `prog-mode' and `conf-mode' buffers."
  :type 'boolean
  :group 'jotain-ui)

(defun jotain-ui--maybe-line-numbers ()
  "Enable `display-line-numbers-mode' if `jotain-line-numbers-in-prog' is set."
  (when jotain-line-numbers-in-prog
    (display-line-numbers-mode 1)))

;;; @doc Built-in line numbers — only on programming and config buffers,
;;; never on prose or org files where they're noise. Honour the
;;; `jotain-line-numbers-in-prog' toggle so users can flip it off
;;; without editing this file.
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode conf-mode) . jotain-ui--maybe-line-numbers))

;;; @doc Built-in pixel-precision smooth scrolling. Required for usable
;;; trackpad / smooth-mouse scrolling.
(use-package pixel-scroll
  :ensure nil
  :config (pixel-scroll-precision-mode 1))

;;; @doc Built-in current-line highlight — on for code and prose, off
;;; in shells/dired where it would fight the cursor.
(use-package hl-line
  :ensure nil
  :hook ((prog-mode conf-mode text-mode) . hl-line-mode))

;;; @doc Built-in matching-paren highlight. Tuned to flash quickly and
;;; highlight even when point is just outside the pair.
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;;; @doc Built-in keybinding cheatsheet. After a prefix key, displays a
;;; paged list of completions in the echo area. Discoverability
;;; multiplier — a Jotain staple.
(use-package which-key
  :ensure nil
  :diminish
  :config (which-key-mode 1))

;;; @doc Built-in calendar. Configured for ISO week numbering and a
;;; Monday week start so it agrees with how the rest of Europe
;;; thinks about dates.
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

;;; @doc Provides the Nerd-Font glyph alphabet that the rest of the
;;; nerd-icons-* family draws on. Picks the font family from
;;; `jotain-font-preferences` so the icons match the editor face.
(use-package nerd-icons
  :config
  (when (display-graphic-p)
    (when-let* ((nerd-font
                 (cl-loop for (family . _height) in jotain-font-preferences
                          when (and (string-match-p "Nerd Font" family)
                                    (find-font (font-spec :family family)))
                          return family)))
      (setopt nerd-icons-font-family nerd-font))))

;;; @doc Decorates corfu candidates with a kind-specific glyph in the
;;; margin, so completions are scannable at a glance.
(use-package nerd-icons-corfu
  :after (nerd-icons corfu)
  :functions (nerd-icons-corfu-formatter)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; @doc Adds Nerd-Font icons to marginalia annotations (file/buffer
;;; category icons in completion lists).
(use-package nerd-icons-completion
  :after (nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


;;; @doc Adds Nerd-Font glyphs to ibuffer rows so buffer types are
;;; visually distinguished at a glance.
(use-package nerd-icons-ibuffer
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;; Polish packages

;;; @doc Highlights TODO / FIXME / HACK / NOTE / XXX keywords in code
;;; with a face that survives theme changes.
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-highlight-punctuation ":"))

;;; @doc Headerline showing project / file / nested function position —
;;; the missing "where am I in this file?" indicator built on
;;; imenu.
(use-package breadcrumb
  :hook (prog-mode . breadcrumb-local-mode))

;;; @doc Pulses a coloured highlight when point jumps a long distance
;;; (other-window, xref, consult-line). Tells the eye where the
;;; cursor went without staring.
(use-package pulsar
  :demand t
  :functions (pulsar-global-mode)
  :custom
  (pulsar-pulse-functions
   '(recenter-top-bottom move-to-window-line-top-bottom reposition-window
     bookmark-jump other-window delete-window delete-other-windows
     forward-page backward-page scroll-up-command scroll-down-command
     xref-find-definitions xref-find-references xref-go-back
     consult-line consult-goto-line imenu))
  :config (pulsar-global-mode 1))

;;; @doc Colourises matching parens by depth in Lisp buffers — almost
;;; essential for navigating deeply nested forms.
(use-package rainbow-delimiters
  :hook ((lisp-mode emacs-lisp-mode) . rainbow-delimiters-mode))

(defcustom jotain-indent-bars-enabled t
  "When non-nil, enable `indent-bars-mode' in `prog-mode'."
  :type 'boolean
  :group 'jotain-ui)

(declare-function indent-bars-mode "indent-bars" (&optional arg))

(defun jotain-ui--maybe-indent-bars ()
  "Enable `indent-bars-mode' when `jotain-indent-bars-enabled' is non-nil."
  (when jotain-indent-bars-enabled
    (indent-bars-mode 1)))

;;; @doc Vertical indent guides for code, treesit-aware so the bars
;;; follow real syntactic indentation. Toggle via
;;; `jotain-indent-bars-enabled'.
(use-package indent-bars
  :custom (indent-bars-treesit-support t)
  :hook (prog-mode . jotain-ui--maybe-indent-bars))

;;;; Terminal compatibility (no-ops in GUI)

;;; @doc Kitty Keyboard Protocol — lets terminal Emacs distinguish
;;; C-i/TAB, C-m/RET, C-[/ESC, and pass Shift-modified function
;;; keys through. No-op in GUI frames, so safe to enable
;;; unconditionally.
(use-package kkp
  :functions (global-kkp-mode)
  :config (global-kkp-mode 1))

;;; @doc OSC 52 clipboard integration. Yank/kill in terminal Emacs
;;; reaches the system clipboard even through ssh + tmux.
(use-package clipetty
  :diminish
  :hook (after-init . global-clipetty-mode))

(defun jotain-ui--tty-setup ()
  "Per-frame tty setup hook."
  (xterm-mouse-mode 1))
(add-hook 'tty-setup-hook #'jotain-ui--tty-setup)

(provide 'init-ui)
;;; init-ui.el ends here
