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
  "In-buffer completion behaviour for the Jotain configuration."
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

(defcustom jotain-completion-auto-delay 0.2
  "Idle seconds before the automatic popup appears.
Applies only in `jotain-completion-auto-modes' buffers.  Also drives
`completion-preview-idle-delay' when `jotain-completion-inline-preview'
is on, so the ghost text and the popup wait the same beat.  `0.2' is
corfu's default; corfu warns that shorter delays create high load."
  :type 'number
  :group 'jotain-completion)

(defcustom jotain-completion-auto-prefix 3
  "Characters typed before the automatic popup appears.
Applies only in `jotain-completion-auto-modes' buffers.  `3' is corfu's
default and matches `completion-preview-minimum-symbol-length', so the
inline preview and the popup start at the same point."
  :type 'integer
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
  - `tab-always-indent' is `complete': TAB indents, and on an already
    indented line runs `completion-at-point' (opening the popup);
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
Enables `global-completion-preview-mode' (Emacs 31); on Emacs 30, which
lacks it, `completion-preview-mode' goes on the
`jotain-completion-auto-modes' hooks instead.

TAB accepts the preview unless `jotain-completion-free-tab' is set;
`M-RET' accepts it too, and `M-i' completes the common prefix.  RET stays
a newline.  Read at load time; `completion-preview-mode' toggles it
per buffer."
  :type 'boolean
  :group 'jotain-completion)

;;;; Minibuffer defaults

;;; @doc Built-in minibuffer customisation: detailed annotations and
;;; historical sorting (Emacs 30) so frequent commands surface first.
(use-package minibuffer
  :ensure nil
  :custom
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-sort 'historical)
  ;; Newcomers-presets theme knobs; with vertico on, these only affect
  ;; the default *Completions* UI.
  (minibuffer-visible-completions t)
  (completions-group t)
  (completion-auto-select 'second-tab)
  :config
  ;; `completion-eager-update' is Emacs 31; guarded for the 30.1 floor.
  (when (boundp 'completion-eager-update)
    (setopt completion-eager-update t)))

;;; @doc Fuzzy, space-separated, order-independent completion. Pairs with
;;; partial-completion (path globbing) so `/u/s/a` matches
;;; `/usr/share/applications`.
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

;;; @doc Vertical minibuffer completion UI. Replaces the default
;;; `*Completions*` buffer with an inline list.
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

;;; @doc Per-category and per-command display modes: grid for files,
;;; a buffer for line/grep/imenu/flymake so candidates have room, and
;;; posframe for everything else. Bundled with vertico.
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

;;; @doc Lets vertico render in a regular buffer instead of the
;;; minibuffer; vertico-multiform uses it for consult-line and the grep
;;; family.
(use-package vertico-buffer
  :ensure nil
  :after vertico
  :custom
  (vertico-buffer-hide-prompt nil)
  (vertico-buffer-display-action '(display-buffer-reuse-window)))

;;; @doc Renders the vertico candidate list in a floating child frame,
;;; styled like the corfu popup. Driven through vertico-multiform (the
;;; `t' catch-all above) rather than a global vertico-posframe-mode, as
;;; upstream recommends, so the other multiform entries keep their own
;;; displays. On a tty posframe cannot work, so vertico stays in the
;;; minibuffer.
(use-package vertico-posframe
  :after vertico
  :custom
  ;; Bottom-centre, where the eye already looks for the minibuffer.
  (vertico-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  ;; Inner padding so candidate text does not sit against the border.
  (vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
  ;; On a tty stay in the minibuffer; `vertico-buffer-mode' would take
  ;; over a whole window for every M-x.
  (vertico-posframe-fallback-mode 'ignore)
  :custom-face
  ;; Inherit corfu's faces so both floating panels match; posframe reads
  ;; faces at display time, so a theme toggle restyles both.
  (vertico-posframe ((t :inherit corfu-default)))
  (vertico-posframe-border ((t :inherit corfu-border))))

;;;; Annotations

;;; @doc Adds annotation columns (file size, mode, docstring, …) to every
;;; completion list.
(use-package marginalia
  ;; No `completing-read' runs before `after-init-hook', so marginalia can
  ;; wait for it.  vertico and orderless stay eager: they must be live for
  ;; the first minibuffer session, which can follow startup immediately.
  :hook (after-init . marginalia-mode))

;;;; Consult — the big binding table

;;; @doc Preview-as-you-go variants of most Emacs lookups: buffer
;;; switch, line jump, grep, recent files, imenu, flymake, registers.
;;; The binding table replaces a dozen built-ins with one consistent UI.
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

;;;; Embark — actions on anything

;;; @doc Context actions on the thing at point or the current candidate.
;;; `C-.' (embark-act) opens a menu of actions for it (file, symbol,
;;; region, buffer, URL, …); `C-;' (embark-dwim) runs the default action
;;; directly. `C-h' after `C-.' makes the menu searchable. In the menu,
;;; `i'/`w' insert or copy the candidate, `A' acts on every candidate and
;;; `B' re-runs the input through another command. `C-h B' is a
;;; searchable describe-bindings.
(use-package embark
  :bind
  (("C-."   . embark-act)
   ("C-;"   . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide modelines in the transient embark collect/live buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;; @doc Glue between embark and consult: `embark-export' (`C-c C-o' in
;;; the minibuffer) turns grep results into a grep buffer, consult-line
;;; into occur, files into dired and buffers into ibuffer. Exporting grep
;;; results and pressing `C-x C-q' (wgrep) edits every match in place.
(use-package embark-consult
  ;; Activates on the first consult command, the only time it matters.
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :bind (:map minibuffer-local-map
              ("C-c C-o" . embark-export)))

;;;; Jump tools

;;; @doc Jump to a visible char, word or line by typing a short label.
;;; Bound under M-g next to the goto family; works across all frames.
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

;;; @doc Frecency-ranked directory jump via the zoxide CLI. Every
;;; `find-file' records its directory; M-g z opens a file from a ranked
;;; directory.
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

;;; @doc TAB indents and completes: `tab-always-indent' is `complete',
;;; so TAB indents the line and, once it is indented, runs
;;; `completion-at-point'. A second TAB in the popup accepts the
;;; candidate. `jotain-completion-free-tab' restores the stock `t'
;;; (indent only).
(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent (if jotain-completion-free-tab t 'complete)))

;; Bind `completion-at-point', not the stock `complete-symbol': a remap
;; only fires for the command a key resolves to, so this is what lets
;; corfu-map's `<remap> <completion-at-point>' make the same key accept.
(when jotain-completion-key
  (keymap-global-set jotain-completion-key #'completion-at-point))

;;; @doc In-buffer completion popup. It opens on its own only in
;;; `jotain-completion-auto-modes' (default: programming modes), so prose
;;; stays quiet. By default TAB and `C-M-i' (`jotain-completion-key')
;;; each work twice: the first press opens the popup, the second inserts
;;; the preselected top candidate (`corfu-insert', which also expands
;;; snippets). RET is unbound in the popup, so Enter is always a newline
;;; (`jotain-completion-free-return'). `M-n'/`M-p' move, `C-g' dismisses.
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
  (corfu-auto nil)
  (corfu-auto-prefix jotain-completion-auto-prefix)
  (corfu-auto-delay jotain-completion-auto-delay)
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

;;; @doc Sorts recently picked candidates first; persisted across
;;; sessions by savehist. Bundled with corfu.
(use-package corfu-history
  :ensure nil
  :after corfu
  :config (corfu-history-mode 1))

;;; @doc Documentation panel beside the popup: a child frame with the
;;; selected candidate's docstring or source location. Gated on
;;; `jotain-completion-doc-popup'. The delay is (INITIAL . SUBSEQUENT):
;;; a longer wait before it first appears so it does not flash, then a
;;; quick refresh as you move between candidates. `M-t' toggles it.
;;; Bundled with corfu.
(use-package corfu-popupinfo
  :ensure nil
  :when jotain-completion-doc-popup
  :after corfu
  :functions (corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(1.0 . 0.5))
  :config (corfu-popupinfo-mode 1))

;;; @doc Extra capfs. dabbrev, file path and keyword go on the global
;;; `completion-at-point-functions', so they run as fallbacks after the
;;; buffer-local major-mode capfs. `cape-elisp-symbol' is added only in
;;; Elisp buffers, where it also completes inside comments and
;;; docstrings.
(use-package cape
  ;; The capfs are autoloaded; cape loads on first use.
  :defer t
  :functions (cape-dabbrev cape-file cape-keyword cape-elisp-symbol)
  :custom
  (cape-file-directory-must-exist t)
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

;;; @doc Inline completion preview (built-in, Emacs 30+): greys out the
;;; most likely completion after point, drawn from the same capfs as the
;;; corfu popup. Enabled in every buffer via
;;; `global-completion-preview-mode' on Emacs 31; on Emacs 30 only in
;;; `jotain-completion-auto-modes'. Gated on
;;; `jotain-completion-inline-preview'. TAB (unless
;;; `jotain-completion-free-tab') and `M-RET' accept the preview, `M-i'
;;; its common prefix; RET stays a newline. Suppressed in comments and
;;; strings, and sorted like corfu so it matches the popup's top row.
(use-package completion-preview
  :ensure nil
  :when jotain-completion-inline-preview
  :defer t
  :functions (completion-preview-insert
              completion-preview-mode
              global-completion-preview-mode)
  :preface
  ;; Neither library is loaded at byte-compile time.
  (defvar completion-preview-active-mode-map)
  (defvar completion-preview-idle-delay)
  (defvar completion-preview-inhibit-functions)
  (defvar corfu-sort-function)
  (defun jotain-completion--preview-inhibit-in-comment ()
    "Return non-nil inside a comment or string.
Added to `completion-preview-inhibit-functions' (Emacs 31) so the ghost
text does not appear where symbol completion is meaningless."
    (nth 8 (syntax-ppss)))
  :init
  ;; Require first so the `fboundp' probe can see the Emacs 31 global mode.
  (require 'completion-preview)
  (if (fboundp 'global-completion-preview-mode)
      (global-completion-preview-mode 1)
    (dolist (hook jotain-completion-auto-modes)
      (add-hook hook #'completion-preview-mode)))
  :config
  ;; Match the popup's delay so one keystroke does not fire two capf
  ;; passes at different times (costly with an LSP capf).
  (setopt completion-preview-idle-delay jotain-completion-auto-delay)
  ;; `C-i' is the TAB event.  Strict mode drops it so TAB only indents;
  ;; otherwise bind it explicitly rather than rely on the shipped default.
  ;; A visible popup wins regardless: corfu-map's TAB takes precedence.
  (if jotain-completion-free-tab
      (keymap-unset completion-preview-active-mode-map "C-i" t)
    (keymap-set completion-preview-active-mode-map "C-i"
                #'completion-preview-insert))
  ;; Accept without TAB or RET.
  (keymap-set completion-preview-active-mode-map "M-RET"
              #'completion-preview-insert)
  ;; Match corfu's top row.  `completion-preview-sort-function' is a user
  ;; option only in Emacs 31.
  (when (and (get 'completion-preview-sort-function 'custom-type)
             (boundp 'corfu-sort-function))
    (setopt completion-preview-sort-function corfu-sort-function))
  ;; Suppress in comments/strings (Emacs 31 hook; absent on 30).
  (when (boundp 'completion-preview-inhibit-functions)
    (add-hook 'completion-preview-inhibit-functions
              #'jotain-completion--preview-inhibit-in-comment)))

(provide 'init-completion)
;;; init-completion.el ends here
