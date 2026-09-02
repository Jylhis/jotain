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

;; Defined inside a `use-package' :config block below; declared for the
;; clean-session per-file compile (nix/config-compiled-split.nix).
(declare-function jotain-cape-setup-elisp nil)

;;;; User options
;;
;; Every knob below is read once, at load time, and needs a restart to take
;; effect -- there are deliberately no `:set' functions.  Half-reactive
;; options are worse than clearly static ones: core's own
;; `text-mode-ispell-word-completion' splits its `:set' across a keymap and a
;; mode-entry read, and that split is a documented source of confusion.
;;
;; For a one-off change without a restart, `M-x corfu-mode' toggles the popup
;; in the current buffer and `M-x global-corfu-mode' toggles the whole stack.

(defgroup jotain-completion nil
  "In-buffer completion behaviour for the Jotain configuration."
  :group 'convenience)

(defcustom jotain-completion-auto-modes '(prog-mode-hook)
  "Hooks whose buffers get the corfu popup automatically.
Everywhere else completion happens only when asked for, via
`jotain-completion-key'.  Set to nil for manual-only completion
everywhere, including code.

`corfu-auto' is read once when `corfu-mode' turns on, so these must be
mode hooks that run before it: `global-corfu-mode' dispatches from
`after-change-major-mode-hook', which runs after major-mode hooks.
Buffers already open when corfu first starts keep their old value."
  :type '(repeat symbol)
  :group 'jotain-completion)

(defcustom jotain-completion-auto-delay 0.2
  "Idle seconds before the automatic popup appears.
Applies only in `jotain-completion-auto-modes' buffers, and also drives
`completion-preview-idle-delay' when `jotain-completion-inline-preview'
is on, so the inline ghost text and the popup wait the same beat.

`0.2' is corfu's own shipped default.  corfu's docstrings explicitly
caution against very short delays -- they \"create high load for Emacs,
in particular if executing the completion backend is costly\" -- so the
previous `0.1' sat below the default in the direction upstream warns
about, which is the setting most likely to feel like the popup fires
while you are still typing."
  :type 'number
  :group 'jotain-completion)

(defcustom jotain-completion-auto-prefix 3
  "Characters typed before the automatic popup appears.
Applies only in `jotain-completion-auto-modes' buffers.  `3' is corfu's
shipped default; it also matches `completion-preview-minimum-symbol-length'
so the inline preview and the popup start suggesting at the same point.
Lower values pop up on one- or two-character fragments, which is the
main source of \"it keeps interrupting me\" noise."
  :type 'integer
  :group 'jotain-completion)

(defcustom jotain-completion-key "C-M-i"
  "Key bound to `completion-at-point', or nil to bind nothing.
`C-M-i' is the binding the Emacs manual recommends, which also steers
users away from `M-TAB' because window managers reserve Alt+Tab.  Note
that binding it here replaces the stock global `complete-symbol', so
`C-u C-M-i' no longer runs `info-complete-symbol'.

On a terminal `C-M-i', `M-TAB' and `ESC TAB' are the same event and
cannot be told apart."
  :type '(choice (string :tag "Key sequence") (const :tag "Do not bind" nil))
  :group 'jotain-completion)

(defcustom jotain-completion-free-return t
  "When non-nil, RET never accepts a completion candidate.
Corfu binds RET to `corfu-insert' by default; this unbinds it so RET is
always a newline.  Freeing RET is an upstream-documented configuration.
Set to nil to restore corfu's default."
  :type 'boolean
  :group 'jotain-completion)

(defcustom jotain-completion-free-tab t
  "When non-nil, TAB never completes -- it only indents.
Two things are needed for that: `tab-always-indent' set to t (the stock
Emacs default), and corfu's own TAB binding removed, since the popup's
keymap would otherwise take precedence while it is open.  This option
covers the second.  Set to nil to restore corfu's default."
  :type 'boolean
  :group 'jotain-completion)

(defcustom jotain-completion-fallbacks t
  "When non-nil, add the cape fallback capfs to the global capf list.
These are `cape-dabbrev', `cape-file' and `cape-keyword' -- the generic
sources that run when a buffer has nothing better to offer."
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
`eglot.el' declares no `:exclusive' property, so its capf is exclusive
and every capf ordered after it is skipped -- and `cape-capf-super'
propagates non-exclusivity only when *every* input is non-exclusive, so
merging a snippet capf into it does not help.  The upshot is that
`cape-dabbrev', `cape-file' and `cape-keyword' never run in an LSP
buffer.  Wrapping the merged capf in `cape-capf-nonexclusive' lets them
run when the server offers nothing for the text at point.

Both halves of that are measured in `test/completion-test.el'.  nil
leaves the capf exactly as eglot installs it."
  :type 'boolean
  :group 'jotain-completion)

(defcustom jotain-completion-doc-popup t
  "When non-nil, show a documentation panel beside the corfu popup.
Enables `corfu-popupinfo-mode' -- a child-frame panel next to the
candidate list that renders the selected candidate's docstring or source
location, the way an IDE shows a detail pane.  The delay is a cons
\(INITIAL . SUBSEQUENT): a longer wait before it first appears so it does
not flash on every brief pause, and a short refresh as you move between
candidates so it keeps up.  It binds only `M-t' / `M-h' / `M-g' /
`C-M-v', so it never collides with the freed RET and TAB or with
`M-n' / `M-p'.

Read at load time; nil skips `corfu-popupinfo-mode' entirely."
  :type 'boolean
  :group 'jotain-completion)

(defcustom jotain-completion-inline-preview t
  "When non-nil, show inline \"ghost text\" of the top candidate as you type.
Enables the built-in `completion-preview-mode' in
`jotain-completion-auto-modes' buffers -- the same modes that get the
auto-popup -- so prose stays quiet (it inherits nothing) while code gains
a greyed-out preview of the most likely completion after point, the way a
modern editor does.  The popup still lists the alternatives; this is the
single inline hint beside it.

TAB is kept safe: `completion-preview-active-mode-map' binds `C-i' (which
IS the TAB event) to accept the preview, so this config unbinds it -- TAB
still only indents while a preview shows.  RET is untouched by the mode
and stays a newline.  Accept the whole preview with `M-RET'; `M-i'
completes just the common prefix.

Read at load time; nil adds no preview mode.  `completion-preview-mode'
also toggles it per-buffer on demand."
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
  :functions (vertico-mode)
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
  :functions (vertico-multiform-mode)
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
  ;; Deferred to `after-init': marginalia only decorates completion
  ;; candidates, and no `completing-read' fires before `after-init-hook'
  ;; runs, so it need not sit on the module-load path. vertico and
  ;; orderless stay eager — vertico-mode must be live and
  ;; `completion-styles' must name orderless before the first minibuffer
  ;; session, which can happen the instant startup finishes.
  :hook (after-init . marginalia-mode))

;;;; Consult — the big binding table

;;; @doc Consult provides preview-as-you-go variants of nearly every
;;; Emacs lookup: buffer switch, line jump, grep, recent files,
;;; imenu, flymake, register store. The big binding table below
;;; replaces a dozen built-ins with a single, consistent UI.
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :functions (consult-xref consult-register-window consult--customize-put)
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
  :functions (embark-prefix-help-command)
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

(defun jotain-completion--zoxide-quiet-sentinel (fn &rest args)
  "Silence the async \"zoxide add\" process spawned by `zoxide-run'.
FN is the advised `zoxide-run'; ARGS are its arguments.  The async
branch starts a process with no sentinel, so Emacs' default sentinel
echoes \"Process zoxide finished\" once per `find-file'.  Attach an
ignoring sentinel to just that process."
  (let ((proc (apply fn args)))
    (when (processp proc)
      (set-process-sentinel proc #'ignore))
    proc))

;;; @doc Frecency-ranked directory jump (like the shell zoxide). Adds
;;; visited files automatically; M-g z surfaces the most-recent
;;; matches first.
(use-package zoxide
  ;; The Nix modules put the zoxide binary on the wrapper PATH
  ;; (nix/runtime-deps.nix); outside those delivery modes, skip the
  ;; find-file hook rather than shell out to a missing binary on
  ;; every file open.
  :if (executable-find "zoxide")
  :custom
  ;; Pin the binary at init time, when `exec-path' is still the global
  ;; wrapper PATH where zoxide is present.  Left unset, the defcustom
  ;; default re-derives it via `executable-find' at lazy-load time, which
  ;; can happen inside a buffer whose `devenv-env-mode' `exec-path' lacks
  ;; zoxide — capturing nil for the session and breaking every
  ;; `zoxide-add' on `find-file' with "Wrong type argument: stringp, nil".
  (zoxide-executable (executable-find "zoxide"))
  :bind
  (("M-g z"   . zoxide-find-file)
   ("M-g M-z" . zoxide-find-file))
  :hook (find-file . zoxide-add)
  :config
  (advice-add 'zoxide-run :around #'jotain-completion--zoxide-quiet-sentinel))

;;;; In-buffer completion

;;; @doc `tab-always-indent' = t is the stock Emacs default, restored here
;;; deliberately: TAB indents the line and does nothing else.
;;; `indent-for-tab-command's only call to `completion-at-point' is
;;; guarded by (eq tab-always-indent 'complete), so with t that branch is
;;; unreachable and TAB provably cannot summon a capf. Completion lives on
;;; `C-M-i' instead (`jotain-completion-key'). `tab-first-completion' is
;;; inert unless `tab-always-indent' is `complete', so it is irrelevant
;;; here and is never set.
(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent t))

;; The one completion gesture.  `completion-at-point' rather than the stock
;; `complete-symbol' is load-bearing: `corfu-map' contains a
;; `<remap> <completion-at-point>' entry, and a remap only fires for the
;; command the key actually resolves to.  Binding the command itself is
;; therefore what makes the same key accept the selected candidate while the
;; popup is open (the `:config' below repoints that remap at `corfu-insert'),
;; so no second accept key is needed.
(when jotain-completion-key
  (keymap-global-set jotain-completion-key #'completion-at-point))

;;; @doc In-buffer completion popup — the corfu equivalent of company.
;;; The popup appears on its own only in the modes listed by
;;; `jotain-completion-auto-modes' (default: programming modes), so prose
;;; stays quiet. `C-M-i' is the whole gesture, used twice: with no popup it
;;; runs the global `completion-at-point' and opens the popup; with the popup
;;; showing the same key inserts the selected candidate. RET and TAB are
;;; unbound in `corfu-map', so Enter always inserts a newline and TAB
;;; always indents, even while the popup is showing. The top candidate is
;;; always preselected (`corfu-preselect' `first'), so it is highlighted and
;;; ready the instant the popup opens; the second `C-M-i' commits it (the
;;; map's `<remap> <completion-at-point>' is repointed from `corfu-complete'
;;; to `corfu-insert', which finishes the completion and expands a snippet).
;;; `M-n'/`M-p' move, `C-g' dismisses.
(use-package corfu
  :hook (after-init . global-corfu-mode)
  :preface
  ;; `corfu-map' is a `defvar-keymap' inside corfu.el, which is not loaded
  ;; when this file is byte-compiled; declare it so the `:config' keymap
  ;; edits below compile clean under `byte-compile-error-on-warn'.
  (defvar corfu-map)
  (defun jotain-completion--enable-auto ()
    "Turn on corfu's auto-popup in the current buffer.
Must run before `corfu-mode' is enabled: `corfu-auto' is read exactly
once, in the `corfu-mode' body, and nothing re-reads it afterwards.
Major-mode hooks satisfy that because `global-corfu-mode' dispatches from
`after-change-major-mode-hook', which `run-mode-hooks' runs after them.
Setting `corfu-auto' from `corfu-mode-hook' would be too late, since
`define-minor-mode' runs the mode hook after the body."
    (setq-local corfu-auto t))
  :init
  (dolist (hook jotain-completion-auto-modes)
    (add-hook hook #'jotain-completion--enable-auto))
  :custom
  (corfu-cycle t)
  (corfu-auto nil)
  (corfu-auto-prefix jotain-completion-auto-prefix)
  (corfu-auto-delay jotain-completion-auto-delay)
  ;; corfu's default is `insert', which previews the selected candidate
  ;; inline AND commits it on further input -- so typing past an open popup
  ;; can silently accept a candidate you never chose.  `nil' shows the
  ;; menu with no inline text and inserts nothing until an explicit
  ;; `corfu-complete' (`C-M-i'), which is the "never accept unless I ask"
  ;; behaviour the freed RET/TAB already aim for -- and it leaves the one
  ;; inline surface to `completion-preview-mode's ghost text.
  (corfu-preview-current nil)
  ;; Always preselect (and highlight, via the `corfu-current' face) the top
  ;; candidate, so `C-M-i' has something to insert and the user sees what it
  ;; will insert.  Safe here because RET/TAB are freed -- only the explicit
  ;; `C-M-i' commits, so `first' cannot cause the accidental accept it can
  ;; when RET/TAB accept.
  (corfu-preselect 'first)
  :config
  ;; REMOVE = t genuinely deletes the entry rather than binding it to nil,
  ;; so the key falls through to the buffer and global maps.
  (when jotain-completion-free-return
    (keymap-unset corfu-map "RET" t))
  (when jotain-completion-free-tab
    (keymap-unset corfu-map "TAB" t)
    (keymap-unset corfu-map "<tab>" t))
  ;; `C-M-i' reaches the popup via corfu-map's `<remap> <completion-at-point>'.
  ;; corfu's default target, `corfu-complete', only extends the common prefix
  ;; (status `exact'), so it neither commits the highlighted candidate nor runs
  ;; a capf's `:exit-function' (tempel snippets would not expand).  Repoint it
  ;; at `corfu-insert', which inserts the selected candidate with status
  ;; `finished' -- the actual "finish the completion" gesture, and the one that
  ;; expands snippets.  Set with the same `<remap>' string corfu ships.
  (keymap-set corfu-map "<remap> <completion-at-point>" #'corfu-insert))

;;; @doc Persists corfu's pick history into savehist so frequent
;;; completions float to the top across sessions. Bundled with
;;; corfu.
(use-package corfu-history
  :ensure nil
  :after corfu
  :functions (corfu-history-mode)
  :config (corfu-history-mode 1))

;;; @doc Documentation panel beside the popup — a child frame that shows
;;; the selected candidate's docstring or source location, the way an IDE
;;; shows a detail pane. Off unless `jotain-completion-doc-popup' is
;;; non-nil. The delay is a cons (INITIAL . SUBSEQUENT): it waits a beat
;;; before first appearing so it does not flash on every pause, then
;;; refreshes quickly as you move between candidates. Bundled with corfu;
;;; binds only `M-t'/`M-h'/`M-g'/`C-M-v', so it never touches the freed
;;; RET/TAB or the `M-n'/`M-p' navigation keys.
(use-package corfu-popupinfo
  :ensure nil
  :when jotain-completion-doc-popup
  :after corfu
  :functions (corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(1.0 . 0.5))
  :config (corfu-popupinfo-mode 1))

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
  (when jotain-completion-fallbacks
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-file)
    (add-hook 'completion-at-point-functions #'cape-keyword))
  (defun jotain-cape-setup-elisp ()
    "Add `cape-elisp-symbol' as an Elisp fallback capf.
The positive depth appends it after the mode's own
`elisp-completion-at-point', so in code the richer built-in wins and
`cape-elisp-symbol' only kicks in where the built-in returns nil —
comments and docstrings."
    (add-hook 'completion-at-point-functions #'cape-elisp-symbol 90 t))
  (add-hook 'emacs-lisp-mode-hook #'jotain-cape-setup-elisp)
  (add-hook 'lisp-interaction-mode-hook #'jotain-cape-setup-elisp))

;;; @doc Inline completion preview — the built-in `completion-preview-mode'
;;; (Emacs 30, extended in 31). It greys out the most likely completion
;;; after point as you type, the way a modern editor does, drawing its
;;; candidate from the same `completion-at-point-functions' the corfu
;;; popup uses. Enabled only in `jotain-completion-auto-modes' buffers (so
;;; prose stays quiet), and only when `jotain-completion-inline-preview'
;;; is non-nil. Two things keep it inside this config's rules: it binds
;;; `C-i' — which is the TAB event — to accept the preview, so that entry
;;; is removed and TAB still only indents; and it never binds RET, so
;;; Enter stays a newline. Accept the whole preview with `M-RET', or just
;;; the common prefix with `M-i'. The preview is suppressed inside
;;; comments and strings, and its sort is paired with corfu's so the ghost
;;; text matches the popup's top row.
(use-package completion-preview
  :ensure nil
  :when jotain-completion-inline-preview
  :defer t
  :functions (completion-preview-insert)
  :preface
  ;; Defined in completion-preview.el / corfu.el, neither loaded at
  ;; byte-compile time; declare them so the `:config' edits below compile
  ;; clean under `byte-compile-error-on-warn'.
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
  (dolist (hook jotain-completion-auto-modes)
    (add-hook hook #'completion-preview-mode))
  :config
  ;; Match the popup's debounce so the inline preview and corfu wait the
  ;; same beat, and one keystroke does not fire two capf passes at
  ;; different times (relevant for a costly LSP capf).  Set in `:config'
  ;; (after load) rather than `:custom' so touching this deferred built-in
  ;; never forces it to load at startup.
  (setopt completion-preview-idle-delay jotain-completion-auto-delay)
  ;; R2: `C-i' is the TAB event, and the active-mode map binds it to
  ;; `completion-preview-insert'.  REMOVE = t drops it so TAB falls
  ;; through to `indent-for-tab-command' while a preview shows.
  (keymap-unset completion-preview-active-mode-map "C-i" t)
  ;; A non-TAB, non-RET accept gesture for the whole candidate; `M-i'
  ;; (common-prefix complete) is left as upstream ships it.
  (keymap-set completion-preview-active-mode-map "M-RET"
              #'completion-preview-insert)
  ;; Pair the previewed candidate with corfu's top row.  In Emacs 31
  ;; `completion-preview-sort-function' is a user option added for exactly
  ;; this; guard on its custom type so Emacs 30 is left untouched.
  (when (and (get 'completion-preview-sort-function 'custom-type)
             (boundp 'corfu-sort-function))
    (setopt completion-preview-sort-function corfu-sort-function))
  ;; Suppress in comments/strings (Emacs 31 hook; absent on 30).
  (when (boundp 'completion-preview-inhibit-functions)
    (add-hook 'completion-preview-inhibit-functions
              #'jotain-completion--preview-inhibit-in-comment)))

(provide 'init-completion)
;;; init-completion.el ends here
