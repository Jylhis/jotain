;;; init-lang-kotlin.el --- Kotlin language support -*- lexical-binding: t; -*-

;;; Commentary:

;; Kotlin tree-sitter mode, Gradle test keys, a REPL, and ktlint/detekt
;; Flymake backends.  The language server (eglot), ktfmt (apheleia), and
;; kotlin-debug-adapter (dape) are wired in `init-prog'.
;;
;; No Kotlin binaries ship with this config.  Expected on the project
;; PATH: a JDK, kotlin-lsp or kotlin-language-server, ktfmt, and
;; optionally ktlint, detekt, kotlin-debug-adapter, and ki or kotlinc
;; (the REPL).  Gradle commands use the project's `./gradlew'.

;;; Code:

(require 'project)

(declare-function comint-check-proc "comint" (buffer))
(declare-function comint-send-string "comint" (process string))
(declare-function flymake-diag-region "flymake" (buffer line &optional col))
(declare-function flymake-make-diagnostic "flymake"
                  (locus beg end type text &optional data overlay-properties))

;;;; Gradle

(defun jotain-kotlin--project-root ()
  "Return the current project's root, or `default-directory' outside one."
  (if-let* ((project (project-current)))
      (project-root project)
    default-directory))

(defun jotain-kotlin--gradle (root)
  "Return the Gradle command for the project at ROOT.
The wrapper wins when the project has one."
  (if (file-exists-p (expand-file-name "gradlew" root)) "./gradlew" "gradle"))

(defun jotain-kotlin-test-project ()
  "Run every Gradle test of the current project."
  (interactive)
  (let ((default-directory (jotain-kotlin--project-root)))
    (compile (concat (jotain-kotlin--gradle default-directory)
                     " --console=plain test"))))

;;;; REPL

(defconst jotain-kotlin--repl-buffer-name "*kotlin*"
  "Name of the Kotlin REPL buffer.")

(defun jotain-kotlin--repl-command ()
  "Return the Kotlin REPL command line on the buffer's PATH.
`ki' wins.  The `kotlinc' REPL is opt-in with `-Xrepl' from Kotlin 2.2;
an older compiler warns about the flag and starts its REPL."
  (cond ((executable-find "ki") '("ki"))
        ((executable-find "kotlinc") '("kotlinc" "-Xrepl"))
        (t (user-error "No Kotlin REPL (ki or kotlinc) on PATH"))))

(defun jotain-kotlin--repl-buffer ()
  "Return the Kotlin REPL buffer; start the REPL in the project root if needed."
  (require 'comint)
  (if (comint-check-proc jotain-kotlin--repl-buffer-name)
      (get-buffer jotain-kotlin--repl-buffer-name)
    (let ((command (jotain-kotlin--repl-command))
          (default-directory (jotain-kotlin--project-root)))
      (apply #'make-comint "kotlin" (car command) nil (cdr command)))))

(defun jotain-kotlin-repl ()
  "Show the Kotlin REPL; start it when it is not running."
  (interactive)
  (pop-to-buffer (jotain-kotlin--repl-buffer)))

(defun jotain-kotlin-send-region (start end)
  "Send the text between START and END to the Kotlin REPL."
  (interactive "r")
  (let ((text (string-trim-right (buffer-substring-no-properties start end))))
    (comint-send-string (get-buffer-process (jotain-kotlin--repl-buffer))
                        (concat text "\n"))))

(defun jotain-kotlin-send-buffer ()
  "Send the whole buffer to the Kotlin REPL."
  (interactive)
  (jotain-kotlin-send-region (point-min) (point-max)))

;;;; Flymake: ktlint and detekt
;;
;; The language server reports compiler diagnostics; these two add style
;; and code-smell findings.  Each backend looks for its binary when it
;; runs, not when the mode starts: the project env (envrc, devenv) loads
;; after the mode hook.

(defvar-local jotain-kotlin--flymake-procs nil
  "Alist from lint tool symbol to its running Flymake process.")

(defun jotain-kotlin--parse-lint-line (line)
  "Parse LINE of ktlint or detekt output (`FILE:LINE:COL: MESSAGE').
Return (LINE COL MESSAGE), or nil when LINE is not a finding."
  (when (string-match
         "\\`.*?:\\([0-9]+\\):\\([0-9]+\\):? +\\(.+\\)\\'" line)
    (list (string-to-number (match-string 1 line))
          (string-to-number (match-string 2 line))
          (match-string 3 line))))

(defun jotain-kotlin--flymake-diags (source output)
  "Return Flymake diagnostics for buffer SOURCE from lint buffer OUTPUT."
  (let (diags)
    (with-current-buffer output
      (goto-char (point-min))
      (while (not (eobp))
        (pcase (jotain-kotlin--parse-lint-line
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))
          (`(,line ,col ,message)
           (when-let* ((region (flymake-diag-region source line col)))
             (push (flymake-make-diagnostic source (car region) (cdr region)
                                            :warning message)
                   diags))))
        (forward-line 1)))
    diags))

(defun jotain-kotlin--flymake-sentinel (tool source report-fn)
  "Return a sentinel that reports TOOL's findings for SOURCE to REPORT-FN.
A process that a newer check replaced reports nothing."
  (lambda (proc _event)
    (when (memq (process-status proc) '(exit signal))
      (unwind-protect
          (when (and (buffer-live-p source)
                     (eq proc (alist-get tool (buffer-local-value
                                               'jotain-kotlin--flymake-procs
                                               source))))
            (funcall report-fn
                     (jotain-kotlin--flymake-diags source (process-buffer proc))))
        (kill-buffer (process-buffer proc))))))

(defun jotain-kotlin--flymake-run (tool command send-buffer report-fn)
  "Run lint COMMAND for TOOL and report its findings to REPORT-FN.
With SEND-BUFFER non-nil, the process reads the buffer text on stdin.
Reports no findings when the program is not on the buffer's PATH."
  (when-let* ((old (alist-get tool jotain-kotlin--flymake-procs))
              ((process-live-p old)))
    (kill-process old))
  (if (not (executable-find (car command)))
      (funcall report-fn nil)
    (let ((proc (make-process
                 :name (format "kotlin-flymake-%s" tool)
                 :buffer (generate-new-buffer
                          (format " *kotlin-flymake-%s*" tool))
                 :command command
                 :noquery t
                 :connection-type 'pipe
                 :sentinel (jotain-kotlin--flymake-sentinel
                            tool (current-buffer) report-fn))))
      (setf (alist-get tool jotain-kotlin--flymake-procs) proc)
      (when send-buffer
        (save-restriction
          (widen)
          (process-send-region proc (point-min) (point-max)))
        (process-send-eof proc)))))

(defun jotain-kotlin-flymake-ktlint (report-fn &rest _args)
  "Flymake backend: report ktlint findings for the buffer text to REPORT-FN."
  (jotain-kotlin--flymake-run
   'ktlint '("ktlint" "--stdin" "--reporter=plain" "--log-level=none")
   t report-fn))

(defun jotain-kotlin--detekt-command (file root)
  "Return the detekt command line for FILE in the project at ROOT.
The project's `config/detekt/detekt.yml' extends the default rules."
  (let ((config (expand-file-name "config/detekt/detekt.yml" root)))
    (append (list "detekt" "--input" file)
            (when (file-exists-p config)
              (list "--config" config "--build-upon-default-config")))))

(defun jotain-kotlin-flymake-detekt (report-fn &rest _args)
  "Flymake backend: report detekt findings for the saved file to REPORT-FN.
detekt reads files only, so a modified buffer has no detekt findings."
  (if (or (not buffer-file-name) (buffer-modified-p))
      (funcall report-fn nil)
    (jotain-kotlin--flymake-run
     'detekt
     (jotain-kotlin--detekt-command buffer-file-name (jotain-kotlin--project-root))
     nil report-fn)))

(defun jotain-kotlin--setup-flymake ()
  "Add the ktlint and detekt Flymake backends to a Kotlin buffer."
  (when (derived-mode-p 'kotlin-ts-mode)
    (add-hook 'flymake-diagnostic-functions #'jotain-kotlin-flymake-ktlint nil t)
    (add-hook 'flymake-diagnostic-functions #'jotain-kotlin-flymake-detekt nil t)))

;; Eglot replaces `flymake-diagnostic-functions' with its own backend when
;; it starts to manage a buffer, after the mode hook.  Add the two again
;; so that they run beside the server's diagnostics.
(add-hook 'eglot-managed-mode-hook #'jotain-kotlin--setup-flymake)

;;;; Mode

(use-package kotlin-ts-mode
  :mode "\\.kts?\\'"
  :hook (kotlin-ts-mode . jotain-kotlin--setup-flymake)
  :bind (:map kotlin-ts-mode-map
              ("C-c C-t t" . kotlin-ts-mode-run-current-test-function)
              ("C-c C-t f" . kotlin-ts-mode-run-current-test-class)
              ("C-c C-t p" . jotain-kotlin-test-project)
              ("C-c C-t g" . kotlin-ts-mode-goto-test-file)
              ("C-c C-z"   . jotain-kotlin-repl)
              ("C-c C-r"   . jotain-kotlin-send-region)
              ("C-c C-b"   . jotain-kotlin-send-buffer)))

(provide 'init-lang-kotlin)
;;; init-lang-kotlin.el ends here
