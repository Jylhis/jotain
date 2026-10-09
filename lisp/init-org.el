;;; init-org.el --- Org mode and friends -*- lexical-binding: t; -*-

;;; Commentary:

;; Org gets its own file: it is a whole writing, agenda and literate-
;; programming environment.  Babel is configured as a notebook: a curated
;; language set, a trust-aware evaluation prompt, and a `C-c b' prefix for
;; the run/restart/clear loop.  See docs/usage/notebooks.mdx.

;;; Code:

(require 'seq)

;; Defined in init-writing.el, which init.el loads before this file.
(defvar jotain-notes-directory)

(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))

;;; @doc Built-in Org: outline, agenda, capture, literate programming.
;;; Notes, agenda and capture all live under `jotain-notes-directory`.
;;; C-c a agenda, C-c c capture, C-c l store link.
(use-package org
  :ensure nil
  :commands (org-mode org-capture org-agenda)
  :preface
  (defun jotain-org--disable-visual-wrap-prefix ()
    "Turn off `visual-wrap-prefix-mode' in Org buffers.
init-writing.el enables it in `text-mode', which Org derives from.  But
`org-indent-mode' (`org-startup-indented') writes the same
`line-prefix'/`wrap-prefix' properties, so with both on the indentation
of list items and wrapped lines jumps as you type.  `visual-line-mode'
stays on, and org-indent supplies the wrap prefix."
    (when (bound-and-true-p visual-wrap-prefix-mode)
      (visual-wrap-prefix-mode -1)))
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link))
  :hook (org-mode . jotain-org--disable-visual-wrap-prefix)
  :custom
  ;; The shared notes root, so capture and org-roam land next to denote
  ;; notes.
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
  ;; Keep all capture templates here.
  (setopt org-capture-templates
        '(("t" "Todo" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %U\n  %a")
          ("n" "Note" entry
           (file+headline org-default-notes-file "Notes")
           "* %?\n  %U"))))

;;; Org Babel — the notebook half of Org
;;
;; A Jupyter-style loop: `C-c C-c' a block, keep state in a `:session',
;; get plots inline, tangle to real source files.

;; Declared, not required: loading Org at compile time would defeat the
;; deferral.
(declare-function org-babel-execute-buffer "ob-core" (&optional arg))
(declare-function org-babel-execute-subtree "ob-core" (&optional arg))
(declare-function org-babel-initiate-session "ob-core" (&optional arg info))
(declare-function org-babel-remove-result-one-or-many "ob-core" (&optional arg))
(declare-function org-babel-switch-to-session "ob-core" (&optional arg info))
(declare-function org-babel-tangle "ob-tangle" (&optional arg target-file lang-re))

;; A plain `defvar' in ob-python, so `:config' sets it with `setq'.
(defvar org-babel-default-header-args:python)

(defconst jotain-org-babel-languages
  '(emacs-lisp org
    shell eshell
    python C R haskell js css
    sql sqlite
    awk sed calc
    dot gnuplot latex)
  "Languages Org Babel may evaluate in a source block.
Every entry must have an `ob-LANG' library that ships with Org, never
one from ELPA.  `C' covers C, C++ and D; `shell' covers every shell
dialect Org knows.  Enabling a language does not provide its
interpreter: that comes from the project's environment, like the LSP
servers in `init-prog'.")

(defun jotain-org-babel-trusted-p ()
  "Return non-nil when the current buffer is an Org file we consider ours.
That is, it lives under `org-directory' or inside a project.  Anything
else (a download, a mail attachment) is untrusted, because evaluating
a source block runs arbitrary code with the user's privileges."
  (when-let* ((file (buffer-file-name (buffer-base-buffer))))
    (let ((file (expand-file-name file)))
      (or (and (stringp org-directory)
               (file-in-directory-p file org-directory))
          (when-let* ((project (project-current nil (file-name-directory file))))
            (file-in-directory-p file (project-root project)))))))

(defun jotain-org-babel-confirm-evaluate (_lang _body)
  "Decide whether to prompt before evaluating a source block.
Value for `org-confirm-babel-evaluate', which passes the block's
language and body and prompts on non-nil.  Both are ignored: only
where the file lives matters (`jotain-org-babel-trusted-p')."
  (not (jotain-org-babel-trusted-p)))

(defun jotain-org-babel-redisplay-images ()
  "Refresh inline images after a source block runs.
So a block that writes a plot to `:file' shows the new image.  Org 9.8
\(Emacs 31) made the old inline-image commands obsolete, so the command
is looked up at runtime to compile warning-free on Org 9.7 and 9.8.
Errors are demoted: a cosmetic refresh must not abort a block."
  (when (derived-mode-p 'org-mode)
    (when-let* ((refresh (seq-find #'fboundp
                                   '(org-link-preview-region
                                     org-redisplay-inline-images))))
      (with-demoted-errors "Inline image refresh failed: %S"
        (funcall refresh)))))

(defun jotain-org-babel-restart-session-and-execute-buffer ()
  "Kill the session of the block at point, then re-run the whole buffer.
A notebook's \"restart kernel and run all\".  Without a `:session'
this is just `org-babel-execute-buffer'."
  (interactive)
  (when-let* ((session (save-window-excursion
                         (ignore-errors (org-babel-initiate-session))))
              (buffer (and (stringp session) (get-buffer session))))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer)))
  (org-babel-execute-buffer))

;;; @doc Org Babel, configured as a notebook. Enables a curated set of
;;; Org-provided languages (`jotain-org-babel-languages`). The evaluation
;;; prompt only appears for files outside your notes and projects. Inline
;;; images refresh after every run. `C-c b` prefix in Org buffers: `b` run
;;; buffer, `e` run subtree, `r` restart session and run buffer, `s`
;;; switch to session, `k` clear results, `t` tangle. Export never
;;; re-evaluates blocks (`:eval never-export`).
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
  ;; Org's defaults plus `:exports both' and `:eval never-export', so
  ;; export publishes the code and the results already in the buffer.
  (org-babel-default-header-args
   '((:session . "none")
     (:results . "replace")
     (:exports . "both")
     (:eval    . "never-export")
     (:cache   . "no")
     (:noweb   . "no")
     (:hlines  . "no")
     (:tangle  . "no")))
  ;; Show `:file' plots, capped at 600px wide.
  (org-startup-with-inline-images t)
  (org-image-actual-width '(600))
  :config
  (setopt org-babel-load-languages
          (mapcar (lambda (lang) (cons lang t)) jotain-org-babel-languages))
  ;; Org's Python default, `:results value', shows nothing for the
  ;; print-style code a notebook invites; capture stdout instead.
  (setq org-babel-default-header-args:python '((:results . "output replace"))))

;;; @doc Built-in source-block editing (`C-c '`). Edits open in the
;;; current window, and indentation is preserved exactly: re-indenting
;;; would corrupt Python blocks, where whitespace is syntax.
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

;;; @doc Built-in `<KEY TAB' block expansion, with extra source-block
;;; keys: `<py', `<sh', `<el', `<sql', `<dot' (Graphviz to a file) and
;;; `<jp' (Python with a session, for notebook-style work).
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

;;; @doc Built-in time tracking. Clocks persist across restarts, so an
;;; interrupted clock can be resumed.
(use-package org-clock
  :ensure nil
  :after org
  :custom
  (org-clock-persist t)
  (org-clock-idle-time 15)
  (org-clock-into-drawer t)
  :config
  (org-clock-persistence-insinuate))

;;; @doc Reveal hidden Org emphasis markers (`*` `_` `/` `~`) only while
;;; point is on them.
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

;;; @doc Modern visual styling for Org buffers and agenda — typographic
;;; bullets, faux-rendered blocks, agenda decorations.
(use-package org-modern
  :hook ((org-mode            . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

;;; @doc Zettelkasten-style note linking on top of Org. The SQLite
;;; database syncs automatically. C-c n f find, i insert, c capture.
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
