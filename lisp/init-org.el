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
  (setopt org-capture-templates
        '(("t" "Todo" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %U\n  %a")
          ("n" "Note" entry
           (file+headline org-default-notes-file "Notes")
           "* %?\n  %U"))))

;;;; Org Babel

;; Declared, not required: loading Org at compile time would defeat the
;; deferral.
(declare-function org-babel-execute-buffer "ob-core" (&optional arg))
(declare-function org-babel-execute-subtree "ob-core" (&optional arg))
(declare-function org-babel-initiate-session "ob-core" (&optional arg info))
(declare-function org-babel-remove-result-one-or-many "ob-core" (&optional arg))
(declare-function org-babel-switch-to-session "ob-core" (&optional arg info))
(declare-function org-babel-tangle "ob-tangle" (&optional arg target-file lang-re))
(declare-function org-link-preview-region "ol"
                  (&optional include-linked refresh beg end))

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
So a block that writes a plot to `:file' shows the new image.
Errors are demoted: a cosmetic refresh must not abort a block."
  (when (derived-mode-p 'org-mode)
    (with-demoted-errors "Inline image refresh failed: %S"
      (org-link-preview-region))))

(defun jotain-org-babel-restart-session-and-execute-buffer ()
  "Kill the session of the block at point, then re-run the whole buffer.
A notebook's \"restart kernel and run all\".  Without a `:session'
this is just `org-babel-execute-buffer'."
  (interactive)
  (when-let* ((session (save-window-excursion
                         (ignore-errors (org-babel-initiate-session))))
              (buffer (if (bufferp session) session
                        (and (stringp session) (get-buffer session)))))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer)))
  (org-babel-execute-buffer))

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
  (org-startup-with-inline-images t)
  (org-image-actual-width '(600))
  :config
  (setopt org-babel-load-languages
          (mapcar (lambda (lang) (cons lang t)) jotain-org-babel-languages))
  ;; Org's Python default, `:results value', shows nothing for the
  ;; print-style code a notebook invites; capture stdout instead.
  (setq org-babel-default-header-args:python '((:results . "output replace"))))

(use-package org-src
  :ensure nil
  :after org
  :custom
  ;; Re-indenting would corrupt Python blocks, where whitespace is syntax.
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  (org-src-window-setup 'current-window)
  (org-src-ask-before-returning-to-edit-buffer nil))

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

(use-package org-clock
  :ensure nil
  :after org
  :custom
  (org-clock-persist t)
  (org-clock-idle-time 15)
  :config
  (org-clock-persistence-insinuate))

(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(use-package org-modern
  :custom
  ;; The replacement bullets carry no face of their own, so they take the
  ;; proportional `mixed-pitch' font and change width when font lock reruns.
  ;; A fixed-pitch face keeps the list indentation stable while typing.
  (org-modern-list
   (mapcar (pcase-lambda (`(,char . ,bullet))
             (cons char (propertize bullet 'face 'fixed-pitch)))
           '((?+ . "◦") (?- . "–") (?* . "•"))))
  :hook ((org-mode            . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

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
