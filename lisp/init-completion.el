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

;;;; User options
;;
;; Every knob below is read once, at load time, and needs a restart: there
;; are deliberately no `:set' functions, since half-reactive options confuse
;; more than static ones.  For a one-off change, `M-x corfu-mode' toggles the
;; popup in the current buffer and `M-x global-corfu-mode' everywhere.

(defgroup jotain-completion nil
  "In-buffer completion behaviour for the configuration."
  :group 'convenience)

(defcustom jotain-completion-auto-modes '(prog-mode-hook)
  "Hooks whose buffers get the corfu popup automatically.
Elsewhere completion runs only on request (TAB or `jotain-completion-key').
Set to nil for manual-only completion everywhere, including code.

These must be major-mode hooks: `corfu-auto' is read once when
`corfu-mode' turns on, and `global-corfu-mode' dispatches from
`after-change-major-mode-hook', which runs after them.  Buffers already
open when corfu first starts keep their old value."
  :type '(repeat symbol)
  :group 'jotain-completion)

(defcustom jotain-completion-key "C-M-i"
  "Key bound to `completion-at-point', or nil to bind nothing.
`C-M-i' is the manual's recommended key (window managers often take
`M-TAB'; on a terminal the two are the same event).  It replaces the
stock global `complete-symbol', so `C-u C-M-i' no longer runs
`info-complete-symbol'."
  :type '(choice (string :tag "Key sequence") (const :tag "Do not bind" nil))
  :group 'jotain-completion)

(defcustom jotain-completion-free-return t
  "When non-nil, RET never accepts a completion candidate.
Corfu binds RET to `corfu-insert'; this unbinds it so RET is always a
newline.  Set to nil to restore corfu's default."
  :type 'boolean
  :group 'jotain-completion)

(defcustom jotain-completion-free-tab nil
  "When non-nil, TAB never completes, it only indents.
Non-nil sets `tab-always-indent' to t (the stock default) and removes
the TAB bindings of the corfu popup and the inline preview.

With the default nil, TAB both indents and completes:
  - `tab-always-indent' is `complete' (set by the newcomers-presets
    theme): TAB indents, and on an already indented line runs
    `completion-at-point' (opening the popup);
  - inside the popup TAB runs `corfu-insert', so a second TAB accepts
    the candidate (and expands a snippet);
  - with only the inline preview showing, TAB accepts the ghost text.
RET is governed by `jotain-completion-free-return'."
  :type 'boolean
  :group 'jotain-completion)

(defcustom jotain-completion-fallbacks t
  "When non-nil, add the cape fallback capfs to the global capf list.
These are `cape-dabbrev', `cape-file' and `cape-keyword', which run when
a buffer has nothing better to offer."
  :type 'boolean
  :group 'jotain-completion)

(defcustom jotain-completion-snippets t
  "When non-nil, snippet names appear as candidates in the popup.
`tempel-complete' is added to the buffer-local capf list, and in
eglot-managed buffers it is merged with the server's capf so snippet and
server candidates share one popup.  nil adds no tempel capf; `M-+' and
`M-*' keep working.

Read at load time by `init-snippets.el'."
  :type 'boolean
  :group 'jotain-completion)

(defcustom jotain-completion-eglot-nonexclusive t
  "When non-nil, stop the LSP capf suppressing the cape fallbacks.
Eglot's capf declares no `:exclusive', so it is exclusive and every capf
after it is skipped.  `cape-capf-super' is non-exclusive only when every
input is, so merging the snippet capf into it does not help.  Wrapping
the merge in `cape-capf-nonexclusive' lets the fallbacks run when the
server offers nothing at point (see `test/completion-test.el').  nil
leaves the capf exactly as eglot installs it."
  :type 'boolean
  :group 'jotain-completion)

(defcustom jotain-completion-doc-popup t
  "When non-nil, show a documentation panel beside the corfu popup.
Enables `corfu-popupinfo-mode', a child frame showing the selected
candidate's docstring or source location.  Read at load time."
  :type 'boolean
  :group 'jotain-completion)

(defcustom jotain-completion-inline-preview t
  "When non-nil, show inline \"ghost text\" of the top candidate as you type.
The newcomers-presets theme enables `global-completion-preview-mode';
nil turns it off again.

TAB accepts the preview unless `jotain-completion-free-tab' is set;
`M-RET' accepts it too, and `M-i' completes the common prefix.  RET stays
a newline.  Read at load time; `completion-preview-mode' toggles it
per buffer."
  :type 'boolean
  :group 'jotain-completion)

;;;; Minibuffer defaults

(use-package minibuffer
  :ensure nil
  :custom
  (completions-format 'one-column)
  (completions-sort 'historical))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless partial-completion flex basic))
  (completion-category-defaults nil)
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
  (vertico-multiform-categories
   '((file   grid)
     (symbol posframe (vertico-sort-function . vertico-sort-alpha))
     (t      posframe)))
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

(use-package vertico-posframe
  :after vertico
  :custom
  (vertico-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
  :custom-face
  ;; Inherit corfu's faces so both floating panels match; posframe reads
  ;; faces at display time, so a theme toggle restyles both.
  (vertico-posframe ((t :inherit corfu-default)))
  (vertico-posframe-border ((t :inherit corfu-border))))

;;;; Annotations

(use-package marginalia
  ;; No `completing-read' runs before `after-init-hook', so marginalia can
  ;; wait for it.  vertico and orderless stay eager: they must be live for
  ;; the first minibuffer session, which can follow startup immediately.
  :hook (after-init . marginalia-mode))

;;;; Consult

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
  ;; `consult-register-window' and `consult-xref' are autoloaded, so
  ;; these references do not load consult; it loads on first use.
  (advice-add #'register-preview :override #'consult-register-window)
  (setopt register-preview-delay 0.5)
  (setopt xref-show-xrefs-function       #'consult-xref
          xref-show-definitions-function #'consult-xref)
  :config
  ;; Expensive previews (grep, files) wait longer before firing.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setopt consult-narrow-key "<"))

;;;; Embark

(use-package embark
  :bind
  (("C-."   . embark-act)
   ("C-;"   . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  ;; Activates on the first consult command, the only time it matters.
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

(defun jotain-completion--zoxide-quiet-sentinel (fn &rest args)
  "Silence the async \"zoxide add\" process spawned by `zoxide-run'.
FN is the advised `zoxide-run'; ARGS are its arguments.  The process has
no sentinel, so the default one echoes \"Process zoxide finished\" on
every `find-file'."
  (let ((proc (apply fn args)))
    (when (processp proc)
      (set-process-sentinel proc #'ignore))
    proc))

(use-package zoxide
  ;; zoxide rides the wrapper PATH (nix/runtime-deps.nix); without it,
  ;; skip the find-file hook rather than fail on every file open.
  :if (executable-find "zoxide")
  :custom
  ;; Pin the binary now, while `exec-path' is the global one.  Resolved
  ;; lazily it can run in a devenv buffer whose `exec-path' lacks zoxide,
  ;; caching nil and breaking every `zoxide-add' for the session.
  (zoxide-executable (executable-find "zoxide"))
  :bind
  (("M-g z"   . zoxide-find-file)
   ("M-g M-z" . zoxide-find-file))
  :hook (find-file . zoxide-add)
  :config
  (advice-add 'zoxide-run :around #'jotain-completion--zoxide-quiet-sentinel))

;;;; In-buffer completion

(when jotain-completion-free-tab
  (setopt tab-always-indent t))

;; Bind `completion-at-point', not the stock `complete-symbol': a remap
;; only fires for the command a key resolves to, so this is what lets
;; corfu-map's `<remap> <completion-at-point>' make the same key accept.
(when jotain-completion-key
  (keymap-global-set jotain-completion-key #'completion-at-point))

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :preface
  ;; corfu.el is not loaded at byte-compile time.
  (defvar corfu-map)
  (defun jotain-completion--enable-auto ()
    "Turn on corfu's auto-popup in the current buffer.
Must run before `corfu-mode' turns on, since its body reads `corfu-auto'
once.  Major-mode hooks run first; `corfu-mode-hook' would be too late."
    (setq-local corfu-auto t))
  :init
  (dolist (hook jotain-completion-auto-modes)
    (add-hook hook #'jotain-completion--enable-auto))
  :custom
  (corfu-cycle t)
  ;; corfu's default `insert' commits the selected candidate when you
  ;; keep typing, silently accepting one you never chose.
  (corfu-preview-current nil)
  ;; Give the accept key something to insert; safe because only an
  ;; explicit TAB / `C-M-i' commits.
  (corfu-preselect 'first)
  :config
  ;; REMOVE = t deletes the entry so the key falls through to other maps.
  (when jotain-completion-free-return
    (keymap-unset corfu-map "RET" t))
  ;; TAB in the popup: strict mode falls through to indentation; otherwise
  ;; use `corfu-insert' instead of corfu's `corfu-complete', which only
  ;; extends the prefix and skips the capf `:exit-function' (so snippets
  ;; would not expand).  "<tab>" too, so GUI and terminal behave alike.
  (if jotain-completion-free-tab
      (progn
        (keymap-unset corfu-map "TAB" t)
        (keymap-unset corfu-map "<tab>" t))
    (keymap-set corfu-map "TAB" #'corfu-insert)
    (keymap-set corfu-map "<tab>" #'corfu-insert))
  ;; Same for `C-M-i', which reaches the popup through this remap.
  (keymap-set corfu-map "<remap> <completion-at-point>" #'corfu-insert))

(use-package corfu-history
  :ensure nil
  :after corfu
  :config (corfu-history-mode 1))

(use-package corfu-popupinfo
  :ensure nil
  :when jotain-completion-doc-popup
  :after corfu
  :functions (corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(1.0 . 0.5))
  :config (corfu-popupinfo-mode 1))

(use-package cape
  ;; The capfs are autoloaded; cape loads on first use.
  :defer t
  :functions (cape-dabbrev cape-file cape-keyword cape-elisp-symbol)
  :custom
  (cape-file-prefix '("~" "/" "./" "../"))
  :init
  (when jotain-completion-fallbacks
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-file)
    (add-hook 'completion-at-point-functions #'cape-keyword))
  (defun jotain-cape-setup-elisp ()
    "Add `cape-elisp-symbol' as an Elisp fallback capf.
Depth 90 puts it after `elisp-completion-at-point', so it only answers
where the built-in returns nil: comments and docstrings."
    (add-hook 'completion-at-point-functions #'cape-elisp-symbol 90 t))
  (add-hook 'emacs-lisp-mode-hook #'jotain-cape-setup-elisp)
  (add-hook 'lisp-interaction-mode-hook #'jotain-cape-setup-elisp))

;; The theme turns the preview on everywhere; this opt-out wins over it
;; (see `jotain-ui--keep-config-overrides').
(unless jotain-completion-inline-preview
  (setopt global-completion-preview-mode nil))

(use-package completion-preview
  :ensure nil
  :when jotain-completion-inline-preview
  :defer t
  :functions (completion-preview-insert)
  :preface
  ;; Neither library is loaded at byte-compile time.
  (defvar completion-preview-active-mode-map)
  (defvar completion-preview-idle-delay)
  (defvar completion-preview-inhibit-functions)
  (defvar corfu-sort-function)
  (defun jotain-completion--preview-inhibit-in-comment ()
    "Return non-nil inside a comment or string.
Added to `completion-preview-inhibit-functions' so the ghost
text does not appear where symbol completion is meaningless."
    (nth 8 (syntax-ppss)))
  :config
  ;; Same 0.2s as corfu's auto delay, so one keystroke does not fire two
  ;; capf passes at different times (costly with an LSP capf).
  (setopt completion-preview-idle-delay 0.2)
  ;; `C-i' is the TAB event.  Strict mode drops it so TAB only indents;
  ;; otherwise bind it explicitly rather than rely on the shipped default.
  ;; A visible popup wins regardless: corfu-map's TAB takes precedence.
  (if jotain-completion-free-tab
      (keymap-unset completion-preview-active-mode-map "C-i" t)
    (keymap-set completion-preview-active-mode-map "C-i"
                #'completion-preview-insert))
  (keymap-set completion-preview-active-mode-map "M-RET"
              #'completion-preview-insert)
  ;; Match corfu's top row once corfu has loaded (it loads after init).
  (with-eval-after-load 'corfu
    (setopt completion-preview-sort-function corfu-sort-function))
  (add-hook 'completion-preview-inhibit-functions
            #'jotain-completion--preview-inhibit-in-comment))

(provide 'init-completion)
;;; init-completion.el ends here
