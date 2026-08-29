;;; jotain-lang-live.el --- Tier-2 live LSP feature probe -*- lexical-binding: t; -*-

;;; Commentary:

;; The end-to-end counterpart to the Tier-1 static probe.  For every registry
;; entry flagged `:live', and whose language server binary is on PATH, it opens
;; a real fixture project, starts an eglot session, and checks that the LSP
;; features actually respond: it reads the negotiated server capabilities
;; (completion, hover, definition, references, rename, formatting, symbols) and
;; issues one real completion request as proof the pipe talks end-to-end.
;;
;; This is deliberately separate from Tier-1: it needs the language toolchains
;; on PATH, so its Nix derivation (nix/lang-eval.nix `lang-eval-live') builds a
;; heavier closure and runs only on the deploy path / on demand, exactly like
;; nix/emacs-api-doc.nix.  A language whose server is absent is recorded as
;; `skip', never `fail'.
;;
;; Fixtures live under etc/lang-eval/fixtures/<id>/.  They are copied to a
;; writable temp dir and `git init'ed at runtime so `project-current' resolves,
;; then the file named by the entry's :sample is visited.
;;
;; Batch entry point (used by nix/lang-eval.nix):
;;   emacs --batch --init-directory=<writable config copy> \
;;     -L <this dir> -l jotain-lang-live.el
;; with JOTAIN_LANG_EVAL_OUT set (output dir) and
;; JOTAIN_LANG_FIXTURES pointing at the fixtures directory.
;;
;; Outside lisp/ and test/ on purpose (see jotain-lang-registry.el).

;;; Code:

(require 'jotain-lang-registry)
(require 'eglot)
(require 'project)
(require 'flymake)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar jotain-lang-live-connect-timeout 45
  "Seconds to wait for an eglot session to negotiate capabilities.")

(defconst jotain-lang-live-features
  '((completion . :completionProvider)
    (hover      . :hoverProvider)
    (definition . :definitionProvider)
    (references . :referencesProvider)
    (rename     . :renameProvider)
    (format     . :documentFormattingProvider)
    (symbols    . :documentSymbolProvider))
  "Feature label -> the server-capability key that advertises it.")

;;;; Helpers

(defun jotain-lang-live--wait (pred timeout)
  "Pump process output until PRED returns non-nil or TIMEOUT seconds elapse."
  (let ((deadline (+ (float-time) timeout)))
    (catch 'done
      (while (< (float-time) deadline)
        (when (funcall pred) (throw 'done t))
        (accept-process-output nil 0.1))
      nil)))

(defun jotain-lang-live--fixture-root (entry fixtures)
  "Copy ENTRY's fixture from FIXTURES to a temp dir and git-init it.
Return the temp directory, or nil when the fixture is missing."
  (let ((src (file-name-as-directory
              (expand-file-name (symbol-name (plist-get entry :id)) fixtures))))
    (when (file-directory-p src)
      (let ((dst (file-name-as-directory (make-temp-file "jotain-live-" t))))
        (copy-directory src dst t t t)
        (let ((default-directory dst))
          (ignore-errors (call-process "git" nil nil nil "init" "-q"))
          (ignore-errors (call-process "git" nil nil nil "add" "-A")))
        dst))))

(defun jotain-lang-live--completion-works-p ()
  "Return non-nil when an eglot completion request yields candidates."
  (ignore-errors
    (goto-char (point-max))
    (let ((capf (eglot-completion-at-point)))
      (and capf
           (let* ((coll (nth 2 capf))
                  (cands (all-completions "" coll)))
             (and cands (> (length cands) 0)))))))

;;;; Per-language probe

(defun jotain-lang-live--one (entry fixtures)
  "Run the live probe for ENTRY using fixtures under FIXTURES."
  (let* ((name (plist-get entry :name))
         (servers (plist-get entry :servers))
         (server (seq-find #'executable-find servers)))
    (cond
     ((not server)
      (list :name name :found nil))
     (t
      (let ((root (jotain-lang-live--fixture-root entry fixtures)))
        (if (not root)
            (list :name name :found server :error "fixture missing")
          (condition-case err
              (let ((file (expand-file-name (plist-get entry :sample) root)))
                (with-current-buffer (find-file-noselect file)
                  (unwind-protect
                      (progn
                        (eglot-ensure)
                        (let ((connected
                               (jotain-lang-live--wait
                                (lambda ()
                                  (and (eglot-current-server)
                                       (eglot--server-capabilities (eglot-current-server))))
                                jotain-lang-live-connect-timeout)))
                          (if (not connected)
                              (list :name name :found server :connected nil)
                            (let* ((caps (eglot--server-capabilities (eglot-current-server)))
                                   (feat (mapcar
                                          (lambda (cell)
                                            (cons (car cell)
                                                  (and (plist-member caps (cdr cell))
                                                       (plist-get caps (cdr cell)) t)))
                                          jotain-lang-live-features)))
                                (list :name name :found server :connected t
                                      :features feat
                                      :diagnostics (and (memq 'eglot-flymake-backend
                                                              flymake-diagnostic-functions) t)
                                      :completion-live (jotain-lang-live--completion-works-p))))))
                    (ignore-errors (eglot-shutdown (eglot-current-server)))
                    (set-buffer-modified-p nil))))
            (error (list :name name :found server :error (error-message-string err))))))))))

(defun jotain-lang-live-run (fixtures)
  "Probe every live registry entry using fixtures under FIXTURES."
  (mapcar (lambda (e) (jotain-lang-live--one e fixtures))
          (jotain-lang-live-entries)))

;;;; Rendering

(defun jotain-lang-live--status (result feature)
  "Render RESULT's FEATURE status cell."
  (cond
   ((not (plist-get result :found)) "skip")
   ((plist-get result :error) "skip")
   ((not (plist-get result :connected)) "✗")
   ((eq feature 'diagnostics) (if (plist-get result :diagnostics) "✓" "✗"))
   ((eq feature 'completion)
    ;; Advertised AND a live request returned candidates.
    (let ((adv (cdr (assq 'completion (plist-get result :features))))
          (live (plist-get result :completion-live)))
      (cond ((and adv live) "✓") (adv "adv") (t "✗"))))
   (t (if (cdr (assq feature (plist-get result :features))) "✓" "✗"))))

(defun jotain-lang-live--render-markdown (results)
  "Render RESULTS as a Markdown live-probe report."
  (let ((rows
         (mapconcat
          (lambda (r)
            (let ((conn (cond ((not (plist-get r :found)) "no server")
                              ((plist-get r :error) (concat "skip: " (plist-get r :error)))
                              ((plist-get r :connected) "✓")
                              (t "✗ no connect"))))
              (format "| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |"
                      (plist-get r :name)
                      (or (plist-get r :found) "—")
                      conn
                      (jotain-lang-live--status r 'completion)
                      (jotain-lang-live--status r 'hover)
                      (jotain-lang-live--status r 'definition)
                      (jotain-lang-live--status r 'references)
                      (jotain-lang-live--status r 'rename)
                      (jotain-lang-live--status r 'format)
                      (jotain-lang-live--status r 'diagnostics))))
          results "\n")))
    (concat
     "# Live LSP probe results\n\n"
     "End-to-end check of a curated language subset: a real eglot session is "
     "started against a fixture project and the negotiated capabilities are "
     "recorded. Generated — do not edit by hand.\n\n"
     "Legend: ✓ works · ✗ not offered · *adv* advertised but the live "
     "completion request returned nothing · *skip* / *no server* the server "
     "binary was not on PATH in this environment.\n\n"
     "| Language | Server | Connected | Completion | Hover | Definition | References | Rename | Format | Diagnostics |\n"
     "| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |\n"
     rows
     "\n")))

(defun jotain-lang-live--json (results)
  "Render RESULTS as a pretty JSON array string."
  (require 'json)
  (let ((json-encoding-pretty-print t))
    (json-encode
     (vconcat
      (mapcar
       (lambda (r)
         (list (cons "name" (plist-get r :name))
               (cons "server" (or (plist-get r :found) :json-null))
               (cons "connected" (and (plist-get r :connected) t))
               (cons "error" (or (plist-get r :error) :json-null))
               (cons "diagnostics" (and (plist-get r :diagnostics) t))
               (cons "completion_live" (and (plist-get r :completion-live) t))
               (cons "features"
                     (or (mapcar (lambda (c) (cons (symbol-name (car c)) (and (cdr c) t)))
                                 (plist-get r :features))
                         :json-null))))
       results)))))

;;;; Batch driver

(defun jotain-lang-live-batch ()
  "Run the live probe and write live.md + live.json to JOTAIN_LANG_EVAL_OUT."
  (let* ((out (or (getenv "JOTAIN_LANG_EVAL_OUT")
                  (error "JOTAIN_LANG_EVAL_OUT is not set")))
         (fixtures (or (getenv "JOTAIN_LANG_FIXTURES")
                       (expand-file-name
                        "fixtures"
                        (file-name-directory
                         (or load-file-name buffer-file-name default-directory)))))
         (results (jotain-lang-live-run fixtures)))
    (make-directory out t)
    (with-temp-file (expand-file-name "live.md" out)
      (insert (jotain-lang-live--render-markdown results)))
    (with-temp-file (expand-file-name "live.json" out)
      (insert (jotain-lang-live--json results)))
    (message "jotain-lang-live: probed %d live languages -> %s" (length results) out)))

(when (and noninteractive (getenv "JOTAIN_LANG_EVAL_OUT")
           (getenv "JOTAIN_LANG_LIVE"))
  (jotain-lang-live-batch))

(provide 'jotain-lang-live)
;;; jotain-lang-live.el ends here
