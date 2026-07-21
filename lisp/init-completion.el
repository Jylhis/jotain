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

;;; @doc Built-in minibuffer customisation: detailed annotations and
;;; historical sorting (Emacs 30) so frequent commands surface first.
(use-package minibuffer
  :ensure nil
  :custom
  (completions-detailed t)
  (completions-format 'one-column)
  ;; Emacs 30: sort candidates by minibuffer history frequency.
  (completions-sort 'historical))

;;; @doc Fuzzy, space-separated, order-independent completion. Pairs with
;;; partial-completion (path globbing) so `/u/s/a` matches
;;; `/usr/share/applications`. The single most important UX win in
;;; the minibuffer.
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

;;; @doc Vertical, performant minibuffer completion UI. Replaces the
;;; default `*Completions*` buffer with an inline list. The whole
;;; minibuffer experience hinges on this.
(use-package vertico
  :demand t
  :config (vertico-mode 1))

;;; @doc Path-savvy editing in vertico — RET enters a candidate
;;; directory, DEL/M-DEL delete a path component instead of one
;;; character. Bundled with vertico.
(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;;; @doc Per-category and per-command display modes (grid for files,
;;; buffer for line/grep so the preview window has room). Bundled
;;; with vertico.
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

;;; @doc Lets vertico render in a regular buffer instead of the
;;; minibuffer — used by vertico-multiform for consult-line and the
;;; grep family so candidates have room to breathe.
(use-package vertico-buffer
  :ensure nil
  :after vertico
  :custom
  (vertico-buffer-hide-prompt nil)
  (vertico-buffer-display-action '(display-buffer-reuse-window)))

;;;; Annotations

;;; @doc Adds annotation columns (file size, mode, docstring, …) to every
;;; completion list. Pairs with vertico to make minibuffer choices
;;; self-explanatory.
(use-package marginalia
  :demand t
  :config (marginalia-mode 1))

;;;; Consult — the big binding table

;;; @doc Consult provides preview-as-you-go variants of nearly every
;;; Emacs lookup: buffer switch, line jump, grep, recent files,
;;; imenu, flymake, register store. The big binding table below
;;; replaces a dozen built-ins with a single, consistent UI.
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :functions (consult-xref consult-register-window)
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
  ;; Take over register preview.  `consult-register-window' and
  ;; `consult-xref' both carry autoload cookies, so referencing them
  ;; here does not load consult — every entry point above is a bound
  ;; command, so consult itself loads on first use.
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

;;; @doc Right-click for the keyboard. `C-.' (embark-act) on any
;;; candidate — file, symbol, region, buffer, command name, URL —
;;; opens a menu of actions valid for that thing; `C-;' (embark-dwim)
;;; skips the menu and runs the default action (visit the file, browse
;;; the URL, jump to the definition). Press `C-h' after `C-.' to turn
;;; the menu into a searchable `completing-read' — the best way to
;;; discover what applies. Inside the menu `i'/`w' insert or copy the
;;; candidate, `A' (embark-act-all) acts on every candidate at once,
;;; and `B' (embark-become) re-runs the typed input through another
;;; command; `C-u C-.' keeps the minibuffer open for successive
;;; actions. `C-h B' replaces describe-bindings with a paged view.
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

;;; @doc Glue between embark and consult — teaches `embark-export' to
;;; turn a consult result list into the right major mode: consult-grep
;;; and consult-ripgrep become a grep buffer, consult-line an occur
;;; buffer, file candidates a dired buffer, buffer candidates an
;;; ibuffer buffer. The grep/ripgrep export is the classic search →
;;; `C-c C-o' → wgrep (`C-x C-q') refactor flow that edits every match
;;; in place across all the matched files.
(use-package embark-consult
  ;; Since consult is deferred, this glue (the C-c C-o binding and the
  ;; collect-mode preview hook) activates on the first consult command
  ;; (e.g. the first `C-x b') — which is also the only time it matters.
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :bind (:map minibuffer-local-map
              ("C-c C-o" . embark-export)))

;;;; Jump tools

;;; @doc Tree-style char/word/line jumping. Bound under M-g so it sits
;;; next to the goto family. Multi-frame aware.
(use-package avy
  :bind
  (("M-g c" . avy-goto-char)
   ("M-g l" . avy-goto-line)
   ("M-g w" . avy-goto-word-1))
  :custom (avy-all-windows 'all-frames))

;;; @doc Frecency-ranked directory jump (like the shell zoxide). Adds
;;; visited files automatically; M-g z surfaces the most-recent
;;; matches first.
(use-package zoxide
  :bind
  (("M-g z"   . zoxide-find-file)
   ("M-g M-z" . zoxide-find-file))
  :hook (find-file . zoxide-add))

;;;; In-buffer completion

;;; @doc `tab-always-indent' = `complete' turns TAB into the single
;;; trigger for the whole completion stack: it indents the line when
;;; indentation is pending and otherwise summons corfu with every
;;; `completion-at-point-functions' entry — the major mode's own capf
;;; plus the cape fallbacks wired up below. Auto-completion still fires
;;; on its own, but this gives an explicit, muscle-memory trigger.
(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete))

;;; @doc In-buffer completion popup — the corfu equivalent of company.
;;; Auto-triggered after 2 chars so it feels like a modern editor
;;; without a long delay.
(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1))

;;; @doc Persists corfu's pick history into savehist so frequent
;;; completions float to the top across sessions. Bundled with
;;; corfu.
(use-package corfu-history
  :ensure nil
  :after corfu
  :config (corfu-history-mode 1))

;;; @doc Completion-at-point Extensions — extra capf functions (dabbrev,
;;; file path, keyword) that feed corfu when the major mode's own
;;; capf finds nothing useful. These three go on the *global*
;;; `completion-at-point-functions' so they act as fallbacks after the
;;; buffer-local, major-mode capfs run. `cape-elisp-symbol' is instead
;;; added buffer-locally in Elisp buffers, where it completes symbols
;;; even inside comments and docstrings (out there `elisp-completion-at-point'
;;; only fires in code), without polluting completion elsewhere.
(use-package cape
  ;; The capfs hooked below are autoloaded, so cape itself only loads
  ;; on the first completion-at-point that reaches them.
  :defer t
  :functions (cape-dabbrev cape-file cape-keyword cape-elisp-symbol)
  :custom
  (cape-file-directory-must-exist t)
  (cape-file-prefix '("~" "/" "./" "../"))
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (defun jotain-cape-setup-elisp ()
    "Add `cape-elisp-symbol' as an Elisp fallback capf.
The positive depth appends it after the mode's own
`elisp-completion-at-point', so in code the richer built-in wins and
`cape-elisp-symbol' only kicks in where the built-in returns nil —
comments and docstrings."
    (add-hook 'completion-at-point-functions #'cape-elisp-symbol 90 t))
  (add-hook 'emacs-lisp-mode-hook #'jotain-cape-setup-elisp)
  (add-hook 'lisp-interaction-mode-hook #'jotain-cape-setup-elisp))

(provide 'init-completion)
;;; init-completion.el ends here
