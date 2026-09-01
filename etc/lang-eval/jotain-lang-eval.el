;;; jotain-lang-eval.el --- Tier-1 static per-language capability probe -*- lexical-binding: t; -*-

;;; Commentary:

;; Loads the full Jotain configuration and, for every entry in
;; `jotain-lang-registry', inspects the *live* config to determine which IDE
;; features are wired up for that language.  It answers "what does the config
;; provide" — deterministically, with no language toolchains required.  Whether
;; a wired feature actually responds end-to-end (i.e. the server is on PATH and
;; talks) is the job of the Tier-2 live probe (`jotain-lang-live.el').
;;
;; Per language it records:
;;   • the major mode a sample file resolves to (via `set-auto-mode') and
;;     whether the tree-sitter grammar is loadable (`treesit-ready-p');
;;   • whether eglot resolves a language server for the buffer
;;     (reusing `jotain-prog--eglot-guess-program') and whether any declared
;;     server binary is on PATH in the build environment;
;;   • the apheleia formatter mapping and its binary's availability;
;;   • the declared DAP adapter and its binary's availability;
;;   • whether curated tempel snippets and inlay hints apply.
;;
;; Every per-language inspection is wrapped in `condition-case', so one broken
;; language records an error cell instead of sinking the whole run.
;;
;; Batch entry point (used by nix/lang-eval.nix):
;;   emacs --batch --init-directory=<writable config copy> \
;;     -L <this dir> -l jotain-lang-eval.el
;; with JOTAIN_LANG_EVAL_OUT pointing at the output directory.  Set
;; JOTAIN_LANG_EVAL_STRICT=1 to exit non-zero on a routing/override regression,
;; which is how the derivation gates the build.
;;
;; Outside lisp/ and test/ on purpose (see jotain-lang-registry.el).

;;; Code:

(require 'jotain-lang-registry)
(require 'seq)
(require 'subr-x)

;; Force the config's lazily-loaded prog packages so their `:config' forms run
;; and populate `eglot-server-programs', `apheleia-mode-alist', `dape-configs'.
(require 'eglot)
(require 'apheleia nil t)
(require 'dape nil t)
(declare-function jotain-prog--eglot-guess-program "init-prog")

;;;; Helpers

(defun jotain-lang-eval--snippet-modes ()
  "Return the set of major-mode symbols that have a tempel section.
Reads templates/jotain.eld (bare symbols are section headers, lists are
templates) rather than depending on tempel's internal state."
  (let* ((root (locate-dominating-file
                (or load-file-name buffer-file-name default-directory) "init.el"))
         (eld (and root (expand-file-name "templates/jotain.eld" root)))
         (modes '()))
    (when (and eld (file-readable-p eld))
      (with-temp-buffer
        (insert-file-contents eld)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (let ((form (read (current-buffer))))
                (when (symbolp form) (push form modes))))
          (end-of-file nil))))
    modes))

(defun jotain-lang-eval--formatter-symbol ()
  "Return the apheleia formatter symbol for the current buffer's mode, or nil."
  (when (boundp 'apheleia-mode-alist)
    (cdr (seq-find (lambda (cell)
                     (and (symbolp (car cell))
                          (provided-mode-derived-p major-mode (car cell))))
                   apheleia-mode-alist))))

(defun jotain-lang-eval--dape-configured-p ()
  "Return non-nil when some `dape-configs' entry targets the current mode."
  (when (boundp 'dape-configs)
    (seq-some
     (lambda (config)
       (let* ((plist (cdr config))
              (modes (or (plist-get plist 'modes) (plist-get plist :modes))))
         (seq-some (lambda (m) (provided-mode-derived-p major-mode m))
                   (if (listp modes) modes (list modes)))))
     dape-configs)))

(defun jotain-lang-eval--any-executable (names)
  "Return the first of NAMES found on PATH, or nil."
  (seq-find #'executable-find names))

;;;; Per-language evaluation

(defun jotain-lang-eval--one (entry snippet-modes)
  "Evaluate one registry ENTRY, returning a result plist.
SNIPPET-MODES is the set of modes with a tempel section (computed once by
the caller, since it re-reads templates/jotain.eld)."
  (condition-case err
      (with-temp-buffer
        (let ((buffer-file-name
               (expand-file-name (plist-get entry :sample) temporary-file-directory)))
          ;; Mode routing (auto-mode-alist + major-mode-remap-alist) and the
          ;; globals we read (eglot-server-programs, apheleia-mode-alist,
          ;; dape-configs) do not depend on mode hooks, so suppress them:
          ;; running every prog-mode hook in batch for 30 throwaway buffers is
          ;; just noise and a source of spurious per-language errors.
          (delay-mode-hooks (set-auto-mode))
          (let* ((expected (plist-get entry :mode))
                 (classic (jotain-lang-get entry :classic))
                 (grammar (jotain-lang-get entry :grammar))
                 (servers (jotain-lang-get entry :servers))
                 (formatter (jotain-lang-get entry :formatter))
                 (dape (jotain-lang-get entry :dape))
                 (mode-ok (or (eq major-mode expected)
                              (and classic (eq major-mode classic))
                              (provided-mode-derived-p major-mode expected)))
                 (guessed (ignore-errors (jotain-prog--eglot-guess-program))))
            (list :id (plist-get entry :id)
                  :name (plist-get entry :name)
                  :expected-mode expected
                  :actual-mode major-mode
                  :mode-ok mode-ok
                  :skip-mode (jotain-lang-get entry :skip-mode)
                  :grammar grammar
                  :grammar-ready (and grammar
                                      (fboundp 'treesit-ready-p)
                                      (treesit-ready-p grammar t))
                  :servers servers
                  :override (jotain-lang-get entry :override)
                  :server-wired (and guessed t)
                  :server-guessed guessed
                  :server-avail (and servers (jotain-lang-eval--any-executable servers))
                  :formatter formatter
                  :formatter-mapped (and (jotain-lang-eval--formatter-symbol) t)
                  :formatter-avail (and formatter (executable-find formatter))
                  :dape dape
                  :dape-configured (jotain-lang-eval--dape-configured-p)
                  :snippets (and (memq expected snippet-modes) t)
                  :inlay (and (jotain-lang-get entry :inlay) t)
                  :live (and (jotain-lang-get entry :live) t)))))
    (error (list :id (plist-get entry :id)
                 :name (plist-get entry :name)
                 :error (error-message-string err)))))

(defun jotain-lang-eval-run ()
  "Evaluate every registry entry, returning a list of result plists.
Computes the tempel snippet-mode set once and shares it across entries."
  (let ((snippet-modes (jotain-lang-eval--snippet-modes)))
    (mapcar (lambda (e) (jotain-lang-eval--one e snippet-modes))
            jotain-lang-registry)))

;;;; Rendering

(defun jotain-lang-eval--cell (result key)
  "Render RESULT's KEY as a matrix cell string."
  (pcase key
    (:mode
     (let ((m (plist-get result :actual-mode)))
       (cond ((not m) "—")
             ((plist-get result :mode-ok) (format "`%s`" m))
             ((plist-get result :skip-mode) (format "`%s`" m))
             (t (format "⚠ `%s` (want `%s`)" m (plist-get result :expected-mode))))))
    (:grammar
     (cond ((not (plist-get result :grammar)) "—")
           ((plist-get result :grammar-ready) "✓")
           (t "✗ not loadable")))
    (:lsp
     (let ((servers (plist-get result :servers)))
       (cond ((not servers) "—")
             (t (format "%s`%s`%s"
                        (if (plist-get result :server-wired) "" "⚠ ")
                        (car servers)
                        (if (plist-get result :server-avail) " ·on PATH" ""))))))
    (:format
     (let ((f (plist-get result :formatter)))
       (cond ((not f) "—")
             (t (format "`%s`%s" f (if (plist-get result :formatter-avail) " ·on PATH" ""))))))
    (:debug
     (let ((d (plist-get result :dape)))
       (if d (format "`%s`" d) "—")))
    (:snippets (if (plist-get result :snippets) "✓" "—"))
    (:inlay (if (plist-get result :inlay) "✓" "—"))
    (:live (if (plist-get result :live) "✓" "—"))))

(defconst jotain-lang-eval--legend
  "\
**Baseline for every `prog-mode` buffer, no server required:** in-buffer
completion (corfu + cape), on-the-fly diagnostics (flymake), echo-area
documentation (eldoc), and code navigation (xref + imenu + tree-sitter fold +
breadcrumb). A language server, when its binary is on the project's PATH,
*upgrades* completion, docs, navigation, and diagnostics to semantic LSP and
adds go-to-definition into dependencies, references, rename, code actions, and
workspace-symbol search. The columns below capture the language-specific
wiring on top of that baseline.

Legend: ✓ wired · — not wired · ⚠ wired but the live config disagrees with the
registry · *·on PATH* the binary was found in this build environment (servers
normally come from the project's own devenv, so absence here is expected).")

(defun jotain-lang-eval--render-markdown (results)
  "Return the capability matrix for RESULTS as a Markdown string."
  (let ((rows
         (mapconcat
          (lambda (r)
            (if (plist-get r :error)
                (format "| %s | ERROR: %s | | | | | | |"
                        (plist-get r :name) (plist-get r :error))
              (format "| %s | %s | %s | %s | %s | %s | %s | %s |"
                      (plist-get r :name)
                      (jotain-lang-eval--cell r :mode)
                      (jotain-lang-eval--cell r :grammar)
                      (jotain-lang-eval--cell r :lsp)
                      (jotain-lang-eval--cell r :format)
                      (jotain-lang-eval--cell r :debug)
                      (jotain-lang-eval--cell r :snippets)
                      (jotain-lang-eval--cell r :inlay))))
          results "\n")))
    (concat
     "# Language support matrix\n\n"
     "Which editor features Jotain wires up for each language it supports, "
     "generated by inspecting the live configuration. Do not edit by hand — "
     "regenerate with `just docs-refresh-lang-matrix`.\n\n"
     jotain-lang-eval--legend
     "\n\n"
     "| Language | Major mode | Tree-sitter | Language server | Formatter | Debug (DAP) | Snippets | Inlay hints |\n"
     "| --- | --- | --- | --- | --- | --- | --- | --- |\n"
     rows
     "\n")))

(defun jotain-lang-eval--plist->alist (pl)
  "Convert plist PL to an alist with string keys and JSON-friendly values."
  (let (out)
    (while pl
      (let ((k (substring (symbol-name (pop pl)) 1))
            (v (pop pl)))
        (push (cons k (cond ((and (symbolp v) (not (memq v '(t nil)))) (symbol-name v))
                            ((eq v t) t)
                            ((null v) :json-null)
                            ((and (listp v) (seq-every-p #'stringp v)) (vconcat v))
                            ((symbolp v) (symbol-name v))
                            (t v)))
              out)))
    (nreverse out)))

(defun jotain-lang-eval--json (results)
  "Return RESULTS as a pretty JSON array string."
  (require 'json)
  (let ((json-encoding-pretty-print t))
    (json-encode (vconcat (mapcar #'jotain-lang-eval--plist->alist results)))))

;;;; Batch driver

(defun jotain-lang-eval--strict-failures (results)
  "Return a list of human-readable regression strings in RESULTS."
  (let (fails)
    (dolist (r results)
      (let ((name (plist-get r :name)))
        (cond
         ((plist-get r :error)
          (push (format "%s: probe errored: %s" name (plist-get r :error)) fails))
         ((and (not (plist-get r :skip-mode)) (not (plist-get r :mode-ok)))
          (push (format "%s: routed to `%s`, expected `%s`"
                        name (plist-get r :actual-mode) (plist-get r :expected-mode))
                fails))
         ((and (plist-get r :override) (not (plist-get r :server-wired)))
          (push (format "%s: has an explicit eglot-server-programs override but eglot resolved no server"
                        name)
                fails)))))
    (nreverse fails)))

(defun jotain-lang-eval-batch ()
  "Run the probe and write matrix.md + matrix.json to JOTAIN_LANG_EVAL_OUT.
With JOTAIN_LANG_EVAL_STRICT set, exit non-zero on any regression."
  (let* ((out (or (getenv "JOTAIN_LANG_EVAL_OUT")
                  (error "JOTAIN_LANG_EVAL_OUT is not set")))
         (results (jotain-lang-eval-run)))
    (make-directory out t)
    (with-temp-file (expand-file-name "matrix.md" out)
      (insert (jotain-lang-eval--render-markdown results)))
    (with-temp-file (expand-file-name "matrix.json" out)
      (insert (jotain-lang-eval--json results)))
    (let ((fails (jotain-lang-eval--strict-failures results)))
      (when fails
        (message "jotain-lang-eval: %d regression(s):" (length fails))
        (dolist (f fails) (message "  - %s" f))
        (when (getenv "JOTAIN_LANG_EVAL_STRICT")
          (kill-emacs 1))))
    (message "jotain-lang-eval: wrote matrix for %d languages to %s"
             (length results) out)))

;; Auto-run only when invoked as a batch job with an output directory set, so
;; the file can be `require'd for unit testing without side effects.
(when (and noninteractive (getenv "JOTAIN_LANG_EVAL_OUT"))
  (jotain-lang-eval-batch))

(provide 'jotain-lang-eval)
;;; jotain-lang-eval.el ends here
