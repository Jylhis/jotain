;;; init-writing.el --- Prose: spell, markdown, denote -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

(defgroup my-writing nil
  "Prose and note-taking settings."
  :group 'text)

(defcustom my-notes-directory (expand-file-name "~/Documents/notes/")
  "Directory holding plain-text notes.
Shared root for `denote-directory' (below) and `org-directory'
(init-org.el), so denote, org-capture and org-roam files land in
one tree.  It need not exist yet."
  :type 'directory
  :group 'my-writing)

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
  (defun my-writing--keep-monospace ()
    "Turn off `mixed-pitch-mode' in a column-sensitive text-mode buffer."
    (mixed-pitch-mode -1))
  :hook ((text-mode        . mixed-pitch-mode)
         (git-commit-setup . my-writing--keep-monospace)
         (message-mode     . my-writing--keep-monospace))
  :config
  ;; Extend the list after load: a `:custom' value that reads the
  ;; variable runs before the deferred package defines it.
  (dolist (face '(markdown-list-face markdown-code-face markdown-inline-code-face))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

;;; @doc Lint feedback for mdoc(7) manual pages (`.1', `.5', `.7',
;;; `.8') in the built-in `nroff-mode'. A Flymake backend pipes the buffer
;;; through `mandoc -Tlint -Wwarning', the same check the repo runs, and
;;; shows each finding at its line. It reports nothing while `mandoc' is
;;; not on the buffer's PATH. Pages keep one sentence per line, so auto-fill,
;;; visual-line wrapping and the proportional font stay off; jinx still
;;; checks the prose.
(use-package nroff-mode
  :ensure nil
  :preface
  (defvar-local my-writing--mandoc-proc nil
    "Running `mandoc' process of the current buffer's Flymake check.")
  (defun my-writing--mandoc-type (level)
    "Return the Flymake diagnostic type for the mandoc LEVEL string."
    (pcase level
      ("ERROR" :error)
      ("STYLE" :note)
      (_ :warning)))
  (defun my-writing--mandoc-diagnostics (source output)
    "Parse mandoc OUTPUT into Flymake diagnostics for buffer SOURCE."
    (let (diags)
      (dolist (line (split-string output "\n" t))
        (when (string-match
               "\\`mandoc: <stdin>:\\([0-9]+\\):\\([0-9]+\\): \\([A-Z]+\\): \\(.*\\)\\'"
               line)
          (let* ((lnum (string-to-number (match-string 1 line)))
                 (col (string-to-number (match-string 2 line)))
                 (type (my-writing--mandoc-type (match-string 3 line)))
                 (text (match-string 4 line))
                 (region (flymake-diag-region source lnum col)))
            (push (flymake-make-diagnostic
                   source (car region) (cdr region) type text)
                  diags))))
      (nreverse diags)))
  (defun my-writing--mandoc-sentinel (proc source report-fn)
    "Report the finished PROC output for SOURCE through REPORT-FN.
Skip it when a newer check replaced PROC."
    (when (memq (process-status proc) '(exit signal))
      (unwind-protect
          (when (and (buffer-live-p source)
                     (eq proc (buffer-local-value 'my-writing--mandoc-proc source)))
            (let ((output (with-current-buffer (process-buffer proc) (buffer-string))))
              (with-current-buffer source
                (save-restriction
                  (widen)
                  (funcall report-fn
                           (my-writing--mandoc-diagnostics source output))))))
        (kill-buffer (process-buffer proc)))))
  (defun my-writing-mandoc-flymake (report-fn &rest _args)
    "Flymake backend: lint the buffer with `mandoc -Tlint -Wwarning'.
REPORT-FN is the Flymake callback."
    (when (process-live-p my-writing--mandoc-proc)
      (kill-process my-writing--mandoc-proc))
    (if (not (executable-find "mandoc"))
        ;; No error: the project environment can supply mandoc later.
        (funcall report-fn nil)
      (my-writing--mandoc-start (current-buffer) report-fn)))
  (defun my-writing--mandoc-start (source report-fn)
    "Pipe SOURCE to mandoc and report its diagnostics through REPORT-FN."
    (with-current-buffer source
      (save-restriction
        (widen)
        (setq my-writing--mandoc-proc
              (make-process
               :name "mandoc-flymake" :noquery t :connection-type 'pipe
               :buffer (generate-new-buffer " *mandoc-flymake*")
               :command '("mandoc" "-Tlint" "-Wwarning")
               :sentinel (lambda (proc _event)
                           (my-writing--mandoc-sentinel proc source report-fn))))
        (process-send-region my-writing--mandoc-proc (point-min) (point-max))
        (process-send-eof my-writing--mandoc-proc))))
  ;; mandoc only reads stdin, so it is safe in untrusted buffers.
  (function-put 'my-writing-mandoc-flymake 'flymake-always-safe t)
  (defun my-writing--setup-man-page ()
    "Set up the current `nroff-mode' buffer as an mdoc page editor.
Turn off the prose wrapping and fonts of `text-mode-hook', and start
Flymake with the mandoc backend."
    (mixed-pitch-mode -1)
    (visual-line-mode -1)
    (when (bound-and-true-p visual-wrap-prefix-mode)
      (visual-wrap-prefix-mode -1))
    (auto-fill-mode -1)
    (add-hook 'flymake-diagnostic-functions #'my-writing-mandoc-flymake nil t)
    (flymake-mode 1))
  :hook (nroff-mode . my-writing--setup-man-page))

;;; @doc Just-in-time spell check using enchant. `global-jinx-mode'
;;; replaces `flyspell-mode' and `flyspell-prog-mode': jinx keys off
;;; faces, so in code it checks only comments and strings. M-$ corrects
;;; the word at point (C-u M-$ walks every misspelling); C-M-$ switches
;;; dictionaries. YAML buffers opt out (init-lang-data).
(use-package jinx
  :preface
  (defun my-writing--enable-jinx ()
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
  :hook (emacs-startup . my-writing--enable-jinx)
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
  (defun my-writing--disable-visual-wrap-prefix ()
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
  :hook (markdown-mode . my-writing--disable-visual-wrap-prefix)
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
;;; `my-notes-directory` (default `~/Documents/notes`), shared with
;;; Org (init-org.el).
(use-package denote
  :commands (denote denote-create-note denote-open-or-create)
  :custom
  (denote-directory my-notes-directory)
  (denote-known-keywords '("emacs" "nix" "linux" "writing")))

;;; @doc In-Emacs PDF viewing: search, annotate, follow links, outline.
;;; The Nix pdf-tools package ships the poppler-based `epdfinfo` server.
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode))

(provide 'init-writing)
;;; init-writing.el ends here
