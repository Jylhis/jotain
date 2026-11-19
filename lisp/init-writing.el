;;; init-writing.el --- Prose: spell, markdown, denote -*- lexical-binding: t; -*-

;;; Commentary:

;; Prose: wrapping, proportional fonts, spell check, Markdown, notes, and
;; PDFs.  Org lives in init-org.el.

;;; Code:

(declare-function mixed-pitch-mode "mixed-pitch" (&optional arg))
(declare-function global-jinx-mode "jinx" (&optional arg))
(declare-function flymake-mode "flymake" (&optional arg))
(declare-function flymake-diag-region "flymake" (buffer line &optional col))
(declare-function flymake-make-diagnostic "flymake"
                  (locus beg end type text &optional data overlay-properties))

(defgroup jotain-writing nil
  "Prose and note-taking settings."
  :group 'text)

(defcustom jotain-notes-directory (expand-file-name "~/Documents/notes/")
  "Directory holding plain-text notes.
Shared root for `denote-directory' (below) and `org-directory'
(init-org.el), so denote, org-capture and org-roam files land in
one tree.  It need not exist yet."
  :type 'directory
  :group 'jotain-writing)

;;; @doc Built-in text-mode tweaks for prose: visual line wrap and
;;; hanging indent on wrapped lines. Proportional fonts come from
;;; `mixed-pitch' (below); code modes stay monospaced.
(use-package text-mode
  :ensure nil
  :custom
  ;; Emacs 30's default adds `ispell-completion-at-point' to text-mode
  ;; capfs.  Without a word list at `ispell-alternate-dictionary' (no
  ;; /usr/share/dict/words on NixOS) every corfu popup in a text buffer
  ;; errors "No plain word-list found".  jinx does spell checking.
  (text-mode-ispell-word-completion nil)
  :hook
  ((text-mode . visual-line-mode)
   (text-mode . visual-wrap-prefix-mode)))

;;; @doc Proportional fonts for prose via the `variable-pitch' face,
;;; while code, tables, and verbatim spans stay monospaced
;;; (`fixed-pitch'). This keeps Org tables aligned, where a blanket
;;; `variable-pitch-mode' would leave them ragged. Enabled in every
;;; text-mode buffer and opted out of the column-sensitive ones: YAML
;;; (see init-lang-data), commit messages, and email.
(use-package mixed-pitch
  :preface
  (defun jotain-writing--keep-monospace ()
    "Turn off `mixed-pitch-mode' in a column-sensitive text-mode buffer."
    (mixed-pitch-mode -1))
  :hook ((text-mode        . mixed-pitch-mode)
         (git-commit-setup . jotain-writing--keep-monospace)
         (message-mode     . jotain-writing--keep-monospace))
  :config
  ;; Extend the list after load: a `:custom' value that reads the
  ;; variable runs before the deferred package defines it.
  (dolist (face '(markdown-list-face markdown-code-face markdown-inline-code-face))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

;;; @doc Lint feedback for mdoc(7) manual pages (`.1', `.5', `.7',
;;; `.8') in the built-in `nroff-mode'. A Flymake backend pipes the buffer
;;; through `mandoc -Tlint -Wwarning' and shows each finding at its line.
;;; It reports nothing while `mandoc' is not on the buffer's PATH. Pages
;;; keep one sentence per line, so auto-fill, visual-line wrapping and the
;;; proportional font stay off; jinx still checks the prose.
(use-package nroff-mode
  :ensure nil
  :preface
  (defvar-local jotain-writing--mandoc-proc nil
    "Running `mandoc' process of the current buffer's Flymake check.")
  (defun jotain-writing--mandoc-type (level)
    "Return the Flymake diagnostic type for the mandoc LEVEL string."
    (pcase level
      ("ERROR" :error)
      ("STYLE" :note)
      (_ :warning)))
  (defun jotain-writing--mandoc-diagnostics (source output)
    "Parse mandoc OUTPUT into Flymake diagnostics for buffer SOURCE."
    (let (diags)
      (dolist (line (split-string output "\n" t))
        (when (string-match
               "\\`mandoc: <stdin>:\\([0-9]+\\):\\([0-9]+\\): \\([A-Z]+\\): \\(.*\\)\\'"
               line)
          (let* ((lnum (string-to-number (match-string 1 line)))
                 (col (string-to-number (match-string 2 line)))
                 (type (jotain-writing--mandoc-type (match-string 3 line)))
                 (text (match-string 4 line))
                 (region (flymake-diag-region source lnum col)))
            (push (flymake-make-diagnostic
                   source (car region) (cdr region) type text)
                  diags))))
      (nreverse diags)))
  (defun jotain-writing--mandoc-sentinel (proc source report-fn)
    "Report the finished PROC output for SOURCE through REPORT-FN.
Skip it when a newer check replaced PROC."
    (when (memq (process-status proc) '(exit signal))
      (unwind-protect
          (when (and (buffer-live-p source)
                     (eq proc (buffer-local-value 'jotain-writing--mandoc-proc source)))
            (let ((output (with-current-buffer (process-buffer proc) (buffer-string))))
              (with-current-buffer source
                (save-restriction
                  (widen)
                  (funcall report-fn
                           (jotain-writing--mandoc-diagnostics source output))))))
        (kill-buffer (process-buffer proc)))))
  (defun jotain-writing-mandoc-flymake (report-fn &rest _args)
    "Flymake backend: lint the buffer with `mandoc -Tlint -Wwarning'.
REPORT-FN is the Flymake callback."
    (when (process-live-p jotain-writing--mandoc-proc)
      (kill-process jotain-writing--mandoc-proc))
    (if (not (executable-find "mandoc"))
        ;; No error: the project environment can supply mandoc later.
        (funcall report-fn nil)
      (jotain-writing--mandoc-start (current-buffer) report-fn)))
  (defun jotain-writing--mandoc-start (source report-fn)
    "Pipe SOURCE to mandoc and report its diagnostics through REPORT-FN."
    (with-current-buffer source
      (save-restriction
        (widen)
        (setq jotain-writing--mandoc-proc
              (make-process
               :name "mandoc-flymake" :noquery t :connection-type 'pipe
               :buffer (generate-new-buffer " *mandoc-flymake*")
               :command '("mandoc" "-Tlint" "-Wwarning")
               :sentinel (lambda (proc _event)
                           (jotain-writing--mandoc-sentinel proc source report-fn))))
        (process-send-region jotain-writing--mandoc-proc (point-min) (point-max))
        (process-send-eof jotain-writing--mandoc-proc))))
  ;; mandoc only reads stdin, so it is safe in untrusted buffers.
  (function-put 'jotain-writing-mandoc-flymake 'flymake-always-safe t)
  (defun jotain-writing--setup-man-page ()
    "Set up the current `nroff-mode' buffer as an mdoc page editor.
Turn off the prose wrapping and fonts of `text-mode-hook', and start
Flymake with the mandoc backend."
    (mixed-pitch-mode -1)
    (visual-line-mode -1)
    (when (bound-and-true-p visual-wrap-prefix-mode)
      (visual-wrap-prefix-mode -1))
    (auto-fill-mode -1)
    (add-hook 'flymake-diagnostic-functions #'jotain-writing-mandoc-flymake nil t)
    (flymake-mode 1))
  :hook (nroff-mode . jotain-writing--setup-man-page))

;;; @doc Just-in-time spell check using enchant. `global-jinx-mode'
;;; replaces `flyspell-mode' and `flyspell-prog-mode': jinx keys off
;;; faces, so in code it checks only comments and strings. M-$ corrects
;;; the word at point (C-u M-$ walks every misspelling); C-M-$ switches
;;; dictionaries. YAML buffers opt out (init-lang-data).
(use-package jinx
  :preface
  (defun jotain-writing--enable-jinx ()
    "Enable `global-jinx-mode', demoting errors so startup continues.
Jinx compiles a native module against enchant on first load; if that
fails (MELPA fallback without cc/enchant), a raw error would abort the
remaining `emacs-startup-hook' functions."
    (with-demoted-errors "init-writing: jinx unavailable: %S"
      (global-jinx-mode 1)))
  :custom
  ;; The Nix distribution bundles en/fi/de/fr aspell dictionaries
  ;; (nix/mk-overlay.nix); switch per buffer with C-M-$, or set e.g.
  ;; "en_GB fi".  One default language stops foreign words passing as
  ;; correct English.
  (jinx-languages "en_GB")
  :hook (emacs-startup . jotain-writing--enable-jinx)
  :bind (("M-$"   . jinx-correct)
         ("C-M-$" . jinx-languages)))

;;; @doc Markdown major mode with native code-block fontification,
;;; heading scaling, and nested imenu. README.md opens in gfm-mode and
;;; `.mdx' files open in plain markdown-mode. Beside
;;; the stock `C-c C-...' bindings, a `C-c m ...' prefix holds the common
;;; ones: markup/image hiding, link and code-block insertion, follow-link,
;;; promote/demote, and live preview. gfm-mode inherits them. Wrapped
;;; lines get no hanging indent (`visual-wrap-prefix-mode' is off here:
;;; it makes list items shake under a proportional font).
(use-package markdown-mode
  :preface
  (defun jotain-writing--disable-visual-wrap-prefix ()
    "Turn off `visual-wrap-prefix-mode' in the current buffer.
The `text-mode' hook enables it, and `markdown-mode' derives from
`text-mode'.  The mode measures the list marker prefix in pixels and
re-measures it while font lock reruns, so with a proportional font the
text of a list item shifts as you type.  `visual-line-mode' stays on."
    (when (bound-and-true-p visual-wrap-prefix-mode)
      (visual-wrap-prefix-mode -1)))
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.mdx\\'"      . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  :hook (markdown-mode . jotain-writing--disable-visual-wrap-prefix)
  :bind (:map markdown-mode-map
              ("C-c m h" . markdown-toggle-markup-hiding)
              ("C-c m i" . markdown-toggle-inline-images)
              ("C-c m l" . markdown-insert-link)
              ("C-c m c" . markdown-insert-gfm-code-block)
              ("C-c m o" . markdown-follow-thing-at-point)
              ("C-c m d" . markdown-do)
              ("C-c m p" . markdown-live-preview-mode)
              ("C-c m <left>"  . markdown-promote)
              ("C-c m <right>" . markdown-demote)))

;;; @doc Plain-text notes with strict file-naming rules, stored under
;;; `jotain-notes-directory` (default `~/Documents/notes`), shared with
;;; Org (init-org.el).
(use-package denote
  :commands (denote denote-create-note denote-open-or-create)
  :custom
  (denote-directory jotain-notes-directory)
  (denote-known-keywords '("emacs" "nix" "linux" "writing")))

;;; @doc In-Emacs PDF viewing: search, annotate, follow links, outline.
;;; The Nix pdf-tools package ships the poppler-based `epdfinfo` server.
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode))

(provide 'init-writing)
;;; init-writing.el ends here
