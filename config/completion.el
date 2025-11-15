;;; completion.el --- Completion and navigation configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern completion UI using vertico, corfu, consult, and embark.

;;; Code:

(use-package vertico
  :ensure
  :init
  (vertico-mode))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :after vertico
  :ensure nil
  :custom
  (vertico-multiform-categories
   '((file buffer grid)
     (imenu (:not indexed mouse))
     (symbol (vertico-sort-function . vertico-sort-alpha))))
  (vertico-multiform-commands
   '((consult-line buffer)
     (consult-git-grep buffer)
     (consult-ripgrep buffer)
     (consult-grep buffer)
     (consult-fd grid)
     (execute-extended-command 'vertical)))
  :init
  (vertico-multiform-mode 1))

(use-package vertico-buffer
  :after vertico
  :ensure nil
  :custom
  (vertico-buffer-hide-prompt nil)
  (vertico-buffer-display-action '(display-buffer-reuse-window)))

(use-package corfu
  :ensure
  :init
  (global-corfu-mode)
  (corfu-history-mode))

(use-package kind-icon
  :ensure
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)  ; Match corfu's face
  (kind-icon-blend-background nil)         ; Use distinct icon colors
  (kind-icon-blend-frac 0.08)              ; Slight background blending
  :config
  ;; Add icon support to corfu
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure
  :init
  ;; Add to completion-at-point-functions globally
  ;; Order matters: more specific completers first
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)

  :custom
  ;; Improve file path completion behavior
  (cape-file-directory-must-exist t)  ; Only complete existing directories
  (cape-file-prefix '("~" "/" "./" "../"))  ; Trigger on ~, /, ./, ../
  ;; Improve dabbrev completion
  (cape-dabbrev-min-length 3)  ; Require at least 3 chars before suggesting

  ;; Add Obsidian completion when available
  ;; (with-eval-after-load 'obsidian
  ;; (when (fboundp 'obsidian-completion-at-point)
  ;; (add-to-list 'completion-at-point-functions #'obsidian-completion-at-point)))
  )

(use-package consult
  :ensure
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)
         ("M-s f" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("C-s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))


;; prescient?
;; https://github.com/Takishima/emacs-config/blob/main/.emacs_lisp/init-emacs.el#L253C14-L253C31
(use-package orderless
  :ensure
  :custom
  (completion-styles '(orderless partial-completion flex basic))
  (completion-category-defaults nil)
  ;; Use partial-completion for files to support path component matching
  ;; e.g., /u/s/a matches /usr/share/applications
  (completion-category-overrides '((file (styles partial-completion orderless))
                                   (buffer (styles orderless))
                                   (project-file (styles partial-completion orderless)))))

(use-package marginalia
  :ensure
  :init
  (marginalia-mode))

(use-package embark
  :ensure
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :bind (:map minibuffer-mode-map
              ("C-c C-o" . embark-export)))

(use-package avy
  :ensure
  :bind (
         ("M-g c"   . avy-goto-char)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :custom (avy-all-windows 'all-frames))

(use-package zoxide
  :ensure
  :bind
  ("M-g z" . zoxide-find-file)
  ("M-g M-z" . zoxide-find-file)
  :hook
  (find-file . zoxide-add))

(provide 'completion)
;;; completion.el ends here
