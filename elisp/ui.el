;;; ui.el --- UI and appearance configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; UI configuration including themes, fonts, and visual enhancements.

;;; Code:

(defgroup j10s-ui nil
  "Customization group for UI."
  :group 'emacs)

(defcustom j10s-theme-light 'modus-operandi-tinted
  "Theme to use when system is in light mode or detection fails."
  :type 'symbol
  :group 'j10s-ui)

(defcustom j10s-theme-dark 'modus-vivendi-tinted
  "Theme to use when system is in dark mode."
  :type 'symbol
  :group 'j10s-ui)

(use-package emacs
  :init
  ;; Trust all themes by default without prompting
  (setq custom-safe-themes t)
  ;; (load-theme 'leuven t)
  ;; (load-theme 'leuven-dark t t)
  (if (and (boundp 'system-uses-dark-theme)
           system-uses-dark-theme)
      (load-theme j10s-theme-dark t)
    (load-theme j10s-theme-light t))
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(defun j10s/toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (if (eq (car custom-enabled-themes) j10s-theme-light)
      (progn
        (disable-theme j10s-theme-light)
        (load-theme j10s-theme-dark t))
    (progn
      (disable-theme j10s-theme-dark)
      (load-theme j10s-theme-light t))))

(use-package modus-themes
  :ensure t
  :bind ("C-c t" . j10s/toggle-theme))

(use-package auto-dark
  :ensure t
  :diminish
  :after modus-themes
  :demand t
  :config
  (setq auto-dark-themes `((,j10s-theme-light) (,j10s-theme-dark)))
  (auto-dark-mode 1)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (when (display-graphic-p frame)
                (with-selected-frame frame (auto-dark-mode 1))))))

(use-package diminish)

(use-package which-key
  :ensure nil  ; Built-in since Emacs 30
  :diminish
  :hook (after-init . which-key-mode))

(use-package hl-line
  :ensure nil
  :custom
  (global-hl-line-sticky-flag t)
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode org-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

(use-package rainbow-delimiters
  :ensure t
  :hook((lisp-mode emacs-lisp-mode) . rainbow-delimiters-mode))

(use-package calendar
  :ensure nil
  :config
  (copy-face 'font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil :height 0.7)
  (setq calendar-week-start-day 1)
  (setq calendar-intermonth-text
        '(propertize (format "%2d" (car (calendar-iso-from-absolute
                                         (calendar-absolute-from-gregorian (list month day year)))))
                     'font-lock-face 'calendar-iso-week-face)))

(use-package breadcrumb
  :ensure t
  :hook (prog-mode . breadcrumb-mode))

(use-package nerd-icons)

(use-package nerd-icons-corfu
  :after nerd-icons corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-ibuffer
  :ensure t
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia nerd-icons
  :demand t
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq
   hl-todo-highlight-punctuation ":"
   hl-todo-keyword-faces
   `(("TODO" warning bold)
     ("FIXME" error bold)
     ("HACK" font-lock-constant-face bold)
     ("REVIEW" font-lock-keyword-face bold)
     ("NOTE" success bold)
     ("DEPRECATED" font-lock-doc-face bold))))

;; Essential built-in enhancements
(use-package winner
  :ensure nil
  :bind (("C-c u" . winner-undo)
         ("C-c r" . winner-redo))
  :init
  (winner-mode 1))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-delay 0.1)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-follow-mouse t))

(use-package pixel-scroll
  :ensure nil
  :when (display-graphic-p)
  :init
  (pixel-scroll-precision-mode 1)
  :config
  ;; Enable for graphical frames when using server-client
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (when (display-graphic-p frame)
                (with-selected-frame frame
                  (pixel-scroll-precision-mode 1))))))

(use-package emojify
  :ensure t
  :defer t
  :custom
  (emojify-inhibit-major-modes '(dired-mode
                                 doc-view-mode
                                 debugger-mode
                                 pdf-view-mode
                                 image-mode
                                 help-mode
                                 ibuffer-mode
                                 magit-popup-mode
                                 magit-diff-mode
                                 nix-mode
                                 ert-results-mode
                                 compilation-mode
                                 proced-mode
                                 mu4e-headers-mode
                                 deft-mode
                                 yaml-mode
                                 prog-mode))

  ;; :hook (after-init . global-emojify-mode)
  )

(use-package kkp
  :ensure t
  :config
  (global-kkp-mode +1))

(provide 'ui)
;;; ui.el ends here
