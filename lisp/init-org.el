;;; init-org.el --- Org mode and friends -*- lexical-binding: t; -*-

;;; Commentary:

;; Org gets its own file because it's an entire writing/agenda/literate-
;; programming environment, not a single feature. Treat the built-in
;; `org' the same as a third-party package — it's big enough to earn it.
;;
;; The Babel half of that environment — source blocks, sessions, inline
;; results, tangling — is configured further down as a notebook: a
;; curated language set, a trust-aware evaluation prompt, and a `C-c b'
;; prefix for the run/restart/clear loop.  See docs/usage/notebooks.mdx.

;;; Code:

(require 'seq)

;; Defined in init-writing.el, which init.el loads before this file.
(defvar jotain-notes-directory)

(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))

;;; @doc Built-in Org — outline, agenda, capture, literate-programming.
;;; Treated as a "third-party" package despite being built-in
;;; because it's the size and surface area of one. Capture templates
;;; and a small bind table live below.
(use-package org
  :ensure nil
  :commands (org-mode org-capture org-agenda)
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :custom
  ;; Shared notes root (`jotain-notes-directory', init-writing.el) so
  ;; org-capture and org-roam land next to denote notes instead of
  ;; dropping loose .org files into the user's home folders.
  (org-directory               jotain-notes-directory)
  (org-agenda-files            (list org-directory))
  (org-default-notes-file      (expand-file-name "inbox.org" org-directory))
  (org-startup-indented        t)
  (org-startup-folded          'content)
  (org-hide-emphasis-markers   t)
  (org-pretty-entities         t)
  (org-log-done                'time)
  (org-return-follows-link     t)
  (org-fold-catch-invisible-edits 'show-and-error)
  :config
  ;; Capture templates: keep the list short and obvious. Add more here
  ;; rather than scattering them across other modules.
  (setopt org-capture-templates
        '(("t" "Todo" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %U\n  %a")
          ("n" "Note" entry
           (file+headline org-default-notes-file "Notes")
           "* %?\n  %U"))))

;;; Org Babel — the notebook half of Org
;;
;; Everything below turns Org's literate-programming engine into a
;; usable replacement for a Jupyter notebook: evaluate a block with
;; `C-c C-c', keep state in a `:session', get plots back inline, and
;; tangle the whole thing out to real source files when it graduates
;; from an experiment to a program.

;; `ob-core' and friends are loaded lazily behind `:after org', so the
;; byte-compiler needs to be told these exist. Declared rather than
;; required: pulling Org in at compile time would defeat the deferral.
(declare-function org-babel-execute-buffer "ob-core" (&optional arg))
(declare-function org-babel-execute-subtree "ob-core" (&optional arg))
(declare-function org-babel-initiate-session "ob-core" (&optional arg info))
(declare-function org-babel-remove-result-one-or-many "ob-core" (&optional arg))
(declare-function org-babel-switch-to-session "ob-core" (&optional arg info))
(declare-function org-babel-tangle "ob-tangle" (&optional arg target-file lang-re))

;; Set in `:config' below with `setq' rather than `setopt' because
;; ob-python declares it with `defvar', not `defcustom'.
(defvar org-babel-default-header-args:python)

(defconst jotain-org-babel-languages
  '(emacs-lisp org
    shell eshell
    python C R haskell js css
    sql sqlite
    awk sed calc
    dot gnuplot latex)
  "Languages Org Babel may evaluate in a source block.
Every entry must be backed by an `ob-LANG' library that ships with
Org itself — no entry here may need a package from ELPA, so a
source block never fails on a machine where the config is only
half-installed.  `C' covers C, C++ and D; `shell' covers bash, sh
and every other shell dialect Org knows about.

The interpreter is a separate question: enabling `python' teaches
Org how to run a Python block, it does not put Python on PATH.
Interpreters come from the project's own environment, exactly like
the LSP servers in `init-prog'.")

(defun jotain-org-babel-trusted-p ()
  "Return non-nil when the current buffer is an Org file we consider ours.
A file counts as ours when it lives under `org-directory' (the notes
tree) or inside the current project.  Anything else — a downloaded
`.org' file, a mail attachment, a gist opened straight from a
browser — does not, because evaluating a source block runs arbitrary
code with the user's privileges."
  (when-let* ((file (buffer-file-name (buffer-base-buffer))))
    (let ((file (expand-file-name file)))
      (or (and (stringp org-directory)
               (file-in-directory-p file org-directory))
          (when-let* ((project (project-current nil (file-name-directory file))))
            (file-in-directory-p file (project-root project)))))))

(defun jotain-org-babel-confirm-evaluate (_lang _body)
  "Decide whether to prompt before evaluating a source block.
Suitable as the value of `org-confirm-babel-evaluate', which calls
its function with the block's language and body and prompts when the
result is non-nil.  Both arguments are ignored: the question is not
what the block contains but where it came from, so the answer comes
from `jotain-org-babel-trusted-p'.

The effect is that your own notes and project files evaluate with no
friction — the notebook loop stays `C-c C-c' — while a source block
in a file from anywhere else still has to be confirmed."
  (not (jotain-org-babel-trusted-p)))

(defun jotain-org-babel-redisplay-images ()
  "Refresh inline images after a source block runs.
Added to `org-babel-after-execute-hook' so a block that writes a plot
to `:file' shows the new image instead of the previous run's.

Org 9.8 (Emacs 31) folded the inline-image commands into the generic
link-preview machinery and marked the old names obsolete, so the
command is looked up at runtime instead of being named directly —
that is what keeps this file byte-compiling warning-clean against
both Org 9.7 and 9.8.  Errors are demoted because refreshing an
image is cosmetic: it must never abort a block that just ran fine."
  (when (derived-mode-p 'org-mode)
    (when-let* ((refresh (seq-find #'fboundp
                                   '(org-link-preview-region
                                     org-redisplay-inline-images))))
      (with-demoted-errors "Inline image refresh failed: %S"
        (funcall refresh)))))

(defun jotain-org-babel-restart-session-and-execute-buffer ()
  "Kill the session of the block at point, then re-run the whole buffer.
The Org equivalent of a notebook's \"restart kernel and run all\": the
one command you want when the session has accumulated state you can no
longer account for.  Blocks that do not use a `:session' have no
session to kill, so this degrades to `org-babel-execute-buffer'."
  (interactive)
  (when-let* ((session (save-window-excursion
                         (ignore-errors (org-babel-initiate-session))))
              (buffer (and (stringp session) (get-buffer session))))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer)))
  (org-babel-execute-buffer))

;;; @doc Org Babel, configured as a notebook. Enables a curated set of
;;; Org-provided languages (see `jotain-org-babel-languages'), swaps
;;; the blanket evaluation prompt for a trust check — your notes and
;;; project files run on `C-c C-c', a `.org' from anywhere else still
;;; asks — and redisplays inline images after every run so `:file'
;;; plots refresh in place. Adds a `C-c b' notebook prefix in Org
;;; buffers: `b' run buffer, `e' run subtree, `r' restart session and
;;; run buffer, `s' switch to session, `k' clear results, `t'
;;; tangle. Blocks are not re-evaluated during export (`:eval
;;; never-export'); what you see in the buffer is what gets exported.
(use-package ob-core
  :ensure nil
  :after org
  :bind
  (:map org-mode-map
        ("C-c b b" . org-babel-execute-buffer)
        ("C-c b e" . org-babel-execute-subtree)
        ("C-c b r" . jotain-org-babel-restart-session-and-execute-buffer)
        ("C-c b s" . org-babel-switch-to-session)
        ("C-c b k" . org-babel-remove-result-one-or-many)
        ("C-c b t" . org-babel-tangle))
  :hook (org-babel-after-execute . jotain-org-babel-redisplay-images)
  :custom
  (org-confirm-babel-evaluate #'jotain-org-babel-confirm-evaluate)
  ;; Org's own defaults plus two opinions: `:exports both' so a block
  ;; and its output both survive export, and `:eval never-export' so
  ;; exporting a document never re-runs code behind your back — the
  ;; results already in the buffer are the ones that get published.
  (org-babel-default-header-args
   '((:session . "none")
     (:results . "replace")
     (:exports . "both")
     (:eval    . "never-export")
     (:cache   . "no")
     (:noweb   . "no")
     (:hlines  . "no")
     (:tangle  . "no")))
  ;; A block that writes a plot to `:file' is only useful if the plot
  ;; shows up. Cap the width so a 2000px figure doesn't push the text
  ;; column off-screen.
  (org-startup-with-inline-images t)
  (org-image-actual-width '(600))
  :config
  (setopt org-babel-load-languages
          (mapcar (lambda (lang) (cons lang t)) jotain-org-babel-languages))
  ;; `org-babel-default-header-args:python' is a plain defvar, not a
  ;; defcustom, so `setq' is the correct setter here.
  ;;
  ;; Org's default for Python is `:results value', which returns the
  ;; value of a `return' statement and therefore shows nothing at all
  ;; for the print-and-see-what-happens style a notebook invites.
  ;; `output' captures stdout instead, which is what people mean.
  ;; Per-block `:results value' still works when you want the value.
  (setq org-babel-default-header-args:python '((:results . "output replace"))))

;;; @doc Built-in source-block editing (`C-c '`). Edits open in the
;;; current window rather than stealing the frame layout, and
;;; indentation is left exactly as written — Org's default of
;;; re-indenting on exit corrupts Python blocks, where leading
;;; whitespace is syntax.
(use-package org-src
  :ensure nil
  :after org
  :custom
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  (org-src-window-setup 'current-window)
  (org-src-ask-before-returning-to-edit-buffer nil))

;;; @doc Built-in `<KEY TAB' block expansion, plus entries for the
;;; languages this config actually runs — `<py', `<sh', `<el',
;;; `<sql', `<jp' (Python with a session, for notebook-style
;;; work). Org's own list covers the structural blocks (`<s',
;;; `<q', `<e'); these add the source blocks worth two keystrokes.
(use-package org-tempo
  :ensure nil
  :after org
  :config
  (setopt org-structure-template-alist
          (seq-uniq (append '(("py"  . "src python")
                              ("jp"  . "src python :session notebook :results output")
                              ("sh"  . "src bash")
                              ("el"  . "src emacs-lisp")
                              ("sql" . "src sql")
                              ("dot" . "src dot :file diagram.png"))
                            org-structure-template-alist)
                    (lambda (a b) (equal (car a) (car b))))))

;;; @doc Built-in time tracking with persistence across restarts. Lets
;;; you resume an interrupted clock without losing the entry.
(use-package org-clock
  :ensure nil
  :after org
  :custom
  (org-clock-persist t)
  (org-clock-idle-time 15)
  (org-clock-into-drawer t)
  :config
  (org-clock-persistence-insinuate))

;;; @doc Reveal Org emphasis markers (`*` `_` `/` `~`) only when point is
;;; on them — best of both visual worlds: clean reading, easy
;;; editing.
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

;;; @doc Modern visual styling for Org buffers and agenda — typographic
;;; bullets, faux-rendered blocks, agenda decorations.
(use-package org-modern
  :hook ((org-mode            . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

;;; @doc Zettelkasten-style note-linking on top of Org. SQLite database
;;; auto-syncs in the background; C-c n f / i / c are the three
;;; bindings you actually use day-to-day.
(use-package org-roam
  :commands (org-roam-node-find org-roam-capture)
  :functions (org-roam-db-autosync-mode)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture))
  :custom
  (org-roam-directory (expand-file-name "org-roam/" org-directory))
  :config
  (make-directory org-roam-directory t)
  (org-roam-db-autosync-mode 1))

(provide 'init-org)
;;; init-org.el ends here
