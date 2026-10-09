;;; init-ui.el --- Theme, modeline, fonts, frame tweaks -*- lexical-binding: t; -*-

;;; Commentary:

;; What the editor looks like: theme, modeline, fonts, icons, line
;; numbers, scrolling, and frame parameters, built-in and third-party.

;;; Code:

(defgroup jotain-ui nil
  "User-facing UI knobs for the Jotain configuration."
  :group 'convenience)

;;;; Theme — Jylhis light/dark, switched by system appearance

(defcustom jotain-theme-light 'jylhis-light
  "Theme to use when the system is in light mode.
The default is the Jylhis theme's light (Print) mode."
  :type 'symbol
  :group 'jotain-ui)

(defcustom jotain-theme-dark 'jylhis-dark
  "Theme to use when the system is in dark mode.
The default is the Jylhis theme's dark (Negative) mode."
  :type 'symbol
  :group 'jotain-ui)

;; Trust all themes: the config loads only the Jylhis themes or the
;; built-in Modus fallback, never theme files from untrusted paths.
(setopt custom-safe-themes t)

(defun jotain-ui--disable-other-themes (_theme &optional _no-confirm no-enable)
  "Disable any active themes before loading a new one.
Without this, switching themes layers the new one on top of the old
and the result is a face-attribute soup."
  (unless no-enable
    (mapc #'disable-theme (copy-sequence custom-enabled-themes))))

(advice-add 'load-theme :before #'jotain-ui--disable-other-themes)

(defun jotain-ui--fall-back-to-modus (reason)
  "Point the theme variables at the built-in Modus themes.
REASON is reported so the downgrade is visible in *Messages*."
  (setopt jotain-theme-light 'modus-operandi
          jotain-theme-dark 'modus-vivendi)
  (message "jotain: %s; falling back to Modus themes" reason))

;; Requiring jylhis-themes puts the Jylhis themes on
;; `custom-theme-load-path'.
(if (not (require 'jylhis-themes nil t))
    (jotain-ui--fall-back-to-modus "jylhis-themes is unavailable")
  ;; Pre-load both themes so auto-dark can flip between them without
  ;; re-evaluating the files.  Skipped in batch, where
  ;; `custom-theme-load-path' may be incomplete.
  ;;
  ;; `load-theme' signals if a theme is missing (e.g. after an upstream
  ;; rename), and init.el requires this module unguarded, so the error
  ;; would take out every later module.  Degrade to Modus instead.
  (unless noninteractive
    (condition-case err
        (progn
          (load-theme jotain-theme-light t t)
          (load-theme jotain-theme-dark t t))
      (error (jotain-ui--fall-back-to-modus (error-message-string err))))))

;;; @doc Flips between `jotain-theme-light` and `jotain-theme-dark`
;;; following the system appearance (macOS, GNOME, or anything exposing
;;; a dark/light setting). C-c t toggles manually.
(use-package auto-dark
  :demand t
  :bind ("C-c t" . auto-dark-toggle-appearance)
  :custom
  (auto-dark-allow-osascript t)
  (auto-dark-themes `((,jotain-theme-dark) (,jotain-theme-light)))
  :config
  (auto-dark-mode 1))

(defun jotain-ui--ensure-tty-theme (&optional frame)
  "Enable a theme on terminal FRAME when `auto-dark' left none active.
In a bare terminal `auto-dark' has no appearance source (no
macOS/GNOME/D-Bus), so no theme gets enabled.  Pick the dark or light
theme by the frame's background mode.  Runs on
`server-after-make-frame-hook' to cover daemon tty clients.  Themes are
frame-global, so a mixed GUI+tty daemon shares one theme."
  (let ((frame (or frame (selected-frame))))
    (when (and (not (display-graphic-p frame))
               (null custom-enabled-themes))
      (with-selected-frame frame
        (ignore-errors
          (load-theme (if (eq (frame-parameter frame 'background-mode) 'light)
                          jotain-theme-light
                        jotain-theme-dark)
                      t))))))

(add-hook 'server-after-make-frame-hook #'jotain-ui--ensure-tty-theme)
;; Non-daemon `emacs -nw': apply once now; the daemon path is the hook.
(unless (or noninteractive (daemonp) (display-graphic-p))
  (jotain-ui--ensure-tty-theme))

;;;; Modeline

(defun jotain-ui--apply-modeline-icons (&optional frame)
  "Enable doom-modeline glyphs only on a graphical FRAME.
Terminal frames have no Nerd Font, so the icons render as tofu.  Runs
on `server-after-make-frame-hook' so a daemon's GUI frames get glyphs
even though no graphical frame exists at daemon start."
  (when (boundp 'doom-modeline-icon)
    (setopt doom-modeline-icon (and (display-graphic-p frame) t))
    (force-mode-line-update t)))

;;; @doc A dense, IDE-style modeline with LSP/eglot status, project
;;; buffer info, and Nerd Font glyphs. Enabled after init.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 4)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-buffer-encoding nil)
  :config
  (jotain-ui--apply-modeline-icons)
  (add-hook 'server-after-make-frame-hook #'jotain-ui--apply-modeline-icons)
  ;; Upstream bug: the git-worktree indicator looks up the codicon
  ;; "nf-cod-worktree" in the `devicon' set, where no nerd-icons release
  ;; has it, and the lookup is unguarded, so the VCS segment errors on
  ;; every redisplay inside a worktree.  Disable just that probe; worktrees
  ;; show in `jotain-git-stats' (init-vc) and magit's worktree section.
  (when (fboundp 'doom-modeline--in-git-worktree-p)
    (advice-add 'doom-modeline--in-git-worktree-p :override #'ignore)))

;; doom-modeline hides minor-mode lighters by default.  For the stock
;; mode line, Emacs 31's `mode-line-collapse-minor-modes' collapses them
;; behind one indicator.
(when (boundp 'mode-line-collapse-minor-modes)
  (setopt mode-line-collapse-minor-modes t))

;;;; Fonts

(defcustom jotain-font-scale 1.0
  "Multiplier applied to every height in the font preference lists.
Increase above 1.0 on large or high-density displays where the
default sizes feel small (for example, set it to 1.25 in your
machine-local config)."
  :type 'float
  :group 'jotain-ui)

(defcustom jotain-font-preferences
  '(("BlexMono Nerd Font"      . 140)
    ("JetBrainsMono Nerd Font" . 140)
    ("Iosevka Nerd Font"       . 140)
    ("DejaVu Sans Mono"        . 130))
  "Ordered list of (FAMILY . HEIGHT) pairs to try for the default face.
HEIGHT is in 1/10 pt units (140 = 14 pt).  The first installed family
wins.  All heights are multiplied by `jotain-font-scale' at runtime.

BlexMono is IBM Plex Mono with Nerd Font glyphs, the mono role of the
Jylhis design system.  Entries containing \"Nerd Font\" also supply
`nerd-icons-font-family'; keep one ahead of the plain fallbacks."
  :type '(alist :key-type string :value-type integer)
  :group 'jotain-ui)

(defcustom jotain-variable-pitch-font-preferences
  '(("Hanken Grotesk" . 150)
    ("Literata"       . 150)
    ("Iosevka Aile"   . 150)
    ("Noto Sans"      . 150)
    ("DejaVu Sans"    . 140))
  "Ordered list of (FAMILY . HEIGHT) pairs to try for the variable-pitch face.
Heights are larger than the monospace default: proportional fonts render
visually smaller at the same point size.

Hanken Grotesk is the body role of the Jylhis design system."
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

(defcustom jotain-emoji-font-preferences
  (if (eq system-type 'darwin)
      '("Apple Color Emoji" "Noto Color Emoji" "Symbola")
    '("Noto Color Emoji" "Segoe UI Emoji" "Symbola"))
  "Ordered list of family names to try for the `emoji' and `symbol' charsets.
The first installed family wins.  macOS ships Apple Color Emoji; on
Linux the Jotain Home Manager and NixOS modules install Noto Color
Emoji."
  :type '(repeat string)
  :group 'jotain-ui)

(defun jotain-ui-apply-emoji-font (&optional frame)
  "Install a colour-emoji fallback for the `emoji' and `symbol' fontsets.
Without this, code points like U+1F389 render as tofu when the
default face's font lacks them.  FRAME is used to probe font
availability on the right display."
  (when (display-graphic-p frame)
    (let ((family (cl-loop for f in jotain-emoji-font-preferences
                           when (find-font (font-spec :family f) frame)
                           return f)))
      (when family
        (set-fontset-font t 'emoji  (font-spec :family family) frame 'prepend)
        (set-fontset-font t 'symbol (font-spec :family family) frame 'prepend)))))

(jotain-ui-apply-emoji-font)
(add-hook 'server-after-make-frame-hook #'jotain-ui-apply-emoji-font)

;;; @doc Built-in emoji picker: `C-x 8 e e' inserts by name, `C-x 8 e s'
;;; searches, `C-x 8 e l' lists all, `C-x 8 e d' describes the emoji at
;;; point.
(use-package emoji
  :ensure nil
  :defer t)

;;;; Built-in display tweaks

;; Don't draw cursors or highlight selections in non-focused windows.
(setopt cursor-in-non-selected-windows nil)
(setopt highlight-nonselected-windows nil)

;; Resize frames and windows by pixel, not whole character cells, so a
;; frame sits flush in a tiling window manager and splits divide evenly.
(setopt frame-resize-pixelwise t)
(setopt window-resize-pixelwise t)

;; Compact the stock mode line only when it overflows the window
;; (mostly inert under doom-modeline).
(setopt mode-line-compact 'long)
;; System font as the default face until `jotain-ui-apply-font' runs.
;; The variable only exists on builds with system-font support, so guard
;; it for the terminal-only distribution.
(when (boundp 'font-use-system-font)
  (setopt font-use-system-font t))

(defcustom jotain-line-numbers-in-prog t
  "When non-nil, show line numbers in `prog-mode' and `conf-mode' buffers."
  :type 'boolean
  :group 'jotain-ui)

(defun jotain-ui--maybe-line-numbers ()
  "Enable `display-line-numbers-mode' if `jotain-line-numbers-in-prog' is set."
  (when jotain-line-numbers-in-prog
    (display-line-numbers-mode 1)))

;;; @doc Built-in line numbers in programming and config buffers only,
;;; not prose or Org. Toggle with `jotain-line-numbers-in-prog'.
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode conf-mode) . jotain-ui--maybe-line-numbers))

;;; @doc Built-in smooth scrolling, tuned after James Cherti's
;;; "Enhancing Emacs Scrolling" (jamescherti.com). `pixel-scroll-mode'
;;; smooths mouse-wheel scrolling; like the newcomers-presets theme, it is
;;; preferred over `pixel-scroll-precision-mode' (bug#69972).
;;; `fast-but-imprecise-scrolling' skips exact fontification on large
;;; jumps. `scroll-conservatively' 20 scrolls just enough to keep point
;;; visible instead of recentering; `auto-window-vscroll' nil avoids
;;; half-screen jumps on long lines; `scroll-error-top-bottom' moves point
;;; to the buffer edge before signalling; the `hscroll-*' pair scrolls
;;; horizontally one column at a time.
(use-package pixel-scroll
  :ensure nil
  :custom
  (fast-but-imprecise-scrolling t)
  (scroll-conservatively 20)
  (scroll-error-top-bottom t)
  (auto-window-vscroll nil)
  (hscroll-margin 2)
  (hscroll-step 1)
  :config (pixel-scroll-mode 1))

;;; @doc Built-in current-line highlight in code, config, and prose
;;; buffers only.
(use-package hl-line
  :ensure nil
  :hook ((prog-mode conf-mode text-mode) . hl-line-mode))

;;; @doc Built-in matching-paren highlight, quick to appear, also when
;;; point is just inside a paren or in a line's leading/trailing space.
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

;;; @doc Built-in keybinding cheatsheet: after a prefix key, a popup
;;; lists the keys that can follow it.
(use-package which-key
  :ensure nil
  :config (which-key-mode 1))

;;; @doc Built-in calendar with ISO week numbers and a Monday week
;;; start.
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

;;; @doc Nerd Font glyphs for the nerd-icons-* family. The font family
;;; comes from `jotain-font-preferences` so icons match the editor face.
(use-package nerd-icons
  ;; Deferred: doom-modeline pulls it in at after-init, and the :after
  ;; chains below (corfu/completion/ibuffer glue) follow it there.
  :defer t
  :preface
  (defun jotain-ui--apply-nerd-icons-font (&optional frame)
    "Set `nerd-icons-font-family' from `jotain-font-preferences' for FRAME.
Runs on `server-after-make-frame-hook' because no graphical frame
exists when a daemon loads the config."
    (when (display-graphic-p frame)
      (when-let* ((nerd-font
                   (cl-loop for (family . _height) in jotain-font-preferences
                            when (and (string-match-p "Nerd Font" family)
                                      (find-font (font-spec :family family) frame))
                            return family)))
        (setopt nerd-icons-font-family nerd-font))))
  :config
  (jotain-ui--apply-nerd-icons-font)
  (add-hook 'server-after-make-frame-hook #'jotain-ui--apply-nerd-icons-font))

;;; @doc Kind-specific glyphs in the corfu candidate margin.
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


;;; @doc Nerd Font glyphs in ibuffer rows, by buffer type.
(use-package nerd-icons-ibuffer
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;; Polish packages

;;; @doc Highlights TODO / FIXME / HACK / NOTE / XXX keywords in code.
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :custom
  (hl-todo-highlight-punctuation ":"))

;;; @doc Header line showing project, file, and the enclosing
;;; definitions at point.
(use-package breadcrumb
  :hook (prog-mode . breadcrumb-local-mode))

;;; @doc Pulses the current line after a jump (other-window, xref,
;;; consult-line) so the eye finds the cursor.
(use-package pulsar
  :hook (after-init . pulsar-global-mode)
  :custom
  (pulsar-pulse-functions
   '(recenter-top-bottom move-to-window-line-top-bottom reposition-window
     bookmark-jump other-window delete-window delete-other-windows
     forward-page backward-page scroll-up-command scroll-down-command
     xref-find-definitions xref-find-references xref-go-back
     consult-line consult-goto-line imenu)))

;;; @doc Colours parens by nesting depth in Lisp buffers.
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

;;; @doc Vertical indent guides for code, tree-sitter aware. Toggle with
;;; `jotain-indent-bars-enabled'.
(use-package indent-bars
  :custom (indent-bars-treesit-support t)
  :hook (prog-mode . jotain-ui--maybe-indent-bars))

(provide 'init-ui)
;;; init-ui.el ends here
