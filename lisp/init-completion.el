;;; init-completion.el --- Minibuffer + in-buffer completion -*- lexical-binding: t; -*-

;;; Commentary:

;; The vertico stack for the minibuffer side, corfu/cape for the
;; in-buffer side, plus the built-in `minibuffer' tweaks they all rely
;; on. They're in one file because changing one almost always means
;; tweaking the others.
;;
;; Deliberate non-choices:
;;   - C-s remains `isearch-forward'. Use `M-s l' (or `consult-line'
;;     directly) when you want the consult UI for searching.

;;; Code:

;;;; Minibuffer defaults

(use-package minibuffer
  :ensure nil
  :custom
  (completions-detailed t)
  (completions-format 'one-column)
  ;; Emacs 30: sort candidates by minibuffer history frequency.
  (completions-sort 'historical))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless partial-completion flex basic))
  (completion-category-defaults nil)
  ;; `partial-completion' for files so `/u/s/a' matches
  ;; `/usr/share/applications'. orderless as the fallback still.
  (completion-category-overrides
   '((file         (styles partial-completion orderless))
     (buffer       (styles orderless))
     (project-file (styles partial-completion orderless)))))

;;;; Vertico + extensions

(use-package vertico
  :demand t
  :config (vertico-mode 1))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :ensure nil
  :after vertico
  :demand t
  :custom
  ;; Per-category display rules. Each entry is (CATEGORY MODE...);
  ;; valid modes include `buffer', `grid', `flat', `unobtrusive',
  ;; `reverse', `vertical' (default). Conflicting modes don't stack.
  (vertico-multiform-categories
   '((file   grid)
     (symbol (vertico-sort-function . vertico-sort-alpha))))
  ;; Per-command overrides — these beat the category rules above.
  (vertico-multiform-commands
   '((consult-line       buffer)
     (consult-line-multi buffer)
     (consult-ripgrep    buffer)
     (consult-grep       buffer)
     (consult-git-grep   buffer)
     (consult-imenu      buffer)
     (consult-flymake    buffer)
     (consult-fd         grid)))
  :config (vertico-multiform-mode 1))

(use-package vertico-buffer
  :ensure nil
  :after vertico
  :custom
  (vertico-buffer-hide-prompt nil)
  (vertico-buffer-display-action '(display-buffer-reuse-window)))

;;;; Annotations

(use-package marginalia
  :demand t
  :config (marginalia-mode 1))

;;;; Consult — the big binding table

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (;; C-c bindings in `mode-specific-map'
   ("C-c M-x" . consult-mode-command)
   ("C-c h"   . consult-history)
   ("C-c k"   . consult-kmacro)
   ("C-c m"   . consult-man)
   ("C-c i"   . consult-info)
   ([remap Info-search] . consult-info)
   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command)
   ("C-x b"   . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x t b" . consult-buffer-other-tab)
   ("C-x r b" . consult-bookmark)
   ("C-x p b" . consult-project-buffer)
   ;; Registers
   ("M-#"     . consult-register-load)
   ("M-'"     . consult-register-store)
   ("C-M-#"   . consult-register)
   ;; Kill-ring
   ("M-y"     . consult-yank-pop)
   ;; M-g (goto-map)
   ("M-g e"   . consult-compile-error)
   ("M-g f"   . consult-flymake)
   ("M-g g"   . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g o"   . consult-outline)
   ("M-g m"   . consult-mark)
   ("M-g k"   . consult-global-mark)
   ("M-g i"   . consult-imenu)
   ("M-g I"   . consult-imenu-multi)
   ;; M-s (search-map)
   ("M-s d"   . consult-fd)
   ("M-s f"   . consult-find)
   ("M-s c"   . consult-locate)
   ("M-s g"   . consult-grep)
   ("M-s G"   . consult-git-grep)
   ("M-s r"   . consult-ripgrep)
   ("M-s l"   . consult-line)
   ("M-s L"   . consult-line-multi)
   ("M-s k"   . consult-keep-lines)
   ("M-s u"   . consult-focus-lines)
   ("M-s e"   . consult-isearch-history)
   ;; Isearch integration
   :map isearch-mode-map
   ("M-e"     . consult-isearch-history)
   ("M-s e"   . consult-isearch-history)
   ("M-s l"   . consult-line)
   ("M-s L"   . consult-line-multi)
   ;; Minibuffer history
   :map minibuffer-local-map
   ("M-s"     . consult-history)
   ("M-r"     . consult-history))
  :init
  ;; Take over register preview.
  (advice-add #'register-preview :override #'consult-register-window)
  (setopt register-preview-delay 0.5)
  ;; Use consult for xref result lists.
  (setopt xref-show-xrefs-function       #'consult-xref
          xref-show-definitions-function #'consult-xref)
  :config
  ;; Per-command preview debouncing — theme preview is fast, ripgrep/grep
  ;; preview is expensive so it waits for you to stop moving the cursor.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setopt consult-narrow-key "<"))

;;;; Embark — actions on anything

(use-package embark
  :bind
  (("C-."   . embark-act)
   ("C-;"   . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  ;; Replace the built-in `C-h <prefix>' with embark's paged version.
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide modelines in the transient embark collect/live buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :bind (:map minibuffer-local-map
              ("C-c C-o" . embark-export)))

;;;; Jump tools

(use-package avy
  :bind
  (("M-g c" . avy-goto-char)
   ("M-g l" . avy-goto-line)
   ("M-g w" . avy-goto-word-1))
  :custom (avy-all-windows 'all-frames))

(use-package zoxide
  :bind
  (("M-g z"   . zoxide-find-file)
   ("M-g M-z" . zoxide-find-file))
  :hook (find-file . zoxide-add))

;;;; In-buffer completion

(use-package corfu
  :demand t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  :config (global-corfu-mode 1))

(use-package corfu-history
  :ensure nil
  :after corfu
  :config (corfu-history-mode 1))

(use-package cape
  :demand t
  :custom
  (cape-file-directory-must-exist t)
  (cape-file-prefix '("~" "/" "./" "../"))
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword))

(provide 'init-completion)
;;; init-completion.el ends here
