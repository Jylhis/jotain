;;; devenv-test.el --- ERT tests for devenv.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Batch-safe unit tests for the pure functions in lisp/devenv.el.
;; No devenv binary, no network, no subprocesses: everything here
;; exercises parsing, routing, and cache logic on canned data.
;;
;; Run with:
;;   emacs --batch -L lisp -L test -l ert -l test/devenv-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'eglot) ; make `eglot-server-programs' a special variable
(require 'devenv)

;;;; Command construction

(ert-deftest devenv-test-command-basic ()
  "Global arguments are spliced between the executable and args."
  (let ((devenv-executable "devenv")
        (devenv-global-arguments '("-q")))
    (should (equal (devenv--command '("info"))
                   '("devenv" "-q" "info")))))

(ert-deftest devenv-test-command-with-transient-args ()
  "Extra global flags land before the subcommand."
  (let ((devenv-executable "devenv")
        (devenv-global-arguments '("-q")))
    (should (equal (devenv--command '("test") '("--clean"))
                   '("devenv" "-q" "--clean" "test")))))

(ert-deftest devenv-test-shell-command-quotes ()
  "Arguments with shell metacharacters are quoted."
  (let ((cmd (devenv--shell-command '("devenv" "shell" "my script"))))
    (should (string-match-p "devenv shell" cmd))
    (should-not (equal cmd "devenv shell my script"))))

;;;; First-line extraction (error reporting)

(ert-deftest devenv-test-first-line ()
  "The first non-empty line is extracted from noisy stderr."
  (should (equal (devenv--first-line "\n\nerror: boom\nmore") "error: boom"))
  (should (equal (devenv--first-line "") ""))
  (should (equal (devenv--first-line nil) "")))

;;;; Cache

(ert-deftest devenv-test-cache-freshness ()
  "Entries are fresh within the TTL and stale after it."
  (let ((devenv-cache-ttl 30)
        (now (float-time)))
    (should (devenv--cache-fresh-p (cons now 'value) now))
    (should (devenv--cache-fresh-p (cons (- now 29) 'value) now))
    (should-not (devenv--cache-fresh-p (cons (- now 31) 'value) now))
    (should-not (devenv--cache-fresh-p nil now))))

(ert-deftest devenv-test-cache-roundtrip-and-invalidate ()
  "Cached values are returned without refetch; invalidation refetches."
  (let ((devenv--cache (make-hash-table :test #'equal))
        (devenv-cache-ttl 300)
        (calls 0))
    (cl-flet ((fetch () (cl-incf calls) 'fetched))
      (should (eq (devenv--cached "/proj/" 'tasks #'fetch) 'fetched))
      (should (eq (devenv--cached "/proj/" 'tasks #'fetch) 'fetched))
      (should (= calls 1))
      ;; Other roots are unaffected by a scoped invalidation.
      (devenv--cached "/other/" 'tasks #'fetch)
      (should (= calls 2))
      (devenv--cache-invalidate "/proj/")
      (devenv--cached "/proj/" 'tasks #'fetch)
      (should (= calls 3))
      (devenv--cached "/other/" 'tasks #'fetch)
      (should (= calls 3)))))

;;;; Task list parsing

(ert-deftest devenv-test-tasks-parse-array ()
  "A JSON array of task objects yields (NAME . DESCRIPTION) pairs."
  (let ((parsed '(((name . "app:build") (description . "Build the app"))
                  ((name . "app:test")))))
    (should (equal (devenv--tasks-parse parsed)
                   '(("app:build" . "Build the app")
                     ("app:test" . nil))))))

(ert-deftest devenv-test-tasks-parse-object ()
  "A JSON object keyed by task name also yields pairs."
  (let ((parsed '((app:build . ((description . "Build the app")))
                  (app:test . ((exec . "pytest"))))))
    (should (equal (devenv--tasks-parse parsed)
                   '(("app:build" . "Build the app")
                     ("app:test" . nil))))))

(ert-deftest devenv-test-tasks-parse-empty ()
  "Empty or unrecognized input parses to nil, not an error."
  (should (null (devenv--tasks-parse nil)))
  (should (null (devenv--tasks-parse "garbage"))))

;;;; Environment parsing (print-dev-env --json)

(defconst devenv-test--env-json
  (concat
   "{\"variables\":{"
   "\"PATH\":{\"type\":\"exported\",\"value\":\"/nix/store/aaa/bin:/usr/bin\"},"
   "\"DEVENV_ROOT\":{\"type\":\"exported\",\"value\":\"/proj\"},"
   "\"PS1\":{\"type\":\"exported\",\"value\":\"$ \"},"
   "\"shellHook\":{\"type\":\"var\",\"value\":\"echo hi\"},"
   "\"BROKEN\":{\"type\":\"exported\",\"value\":null}"
   "}}")
  "Canned `devenv print-dev-env --json' output.")

(ert-deftest devenv-test-env-parse-exported-only ()
  "Only exported string variables become NAME=VALUE pairs."
  (let* ((devenv-env-ignored-variables '("PS1" "SHLVL" "TERM" "HOME"))
         (pairs (devenv-env--parse devenv-test--env-json)))
    (should (member "PATH=/nix/store/aaa/bin:/usr/bin" pairs))
    (should (member "DEVENV_ROOT=/proj" pairs))
    ;; Ignored variable filtered out.
    (should-not (seq-find (lambda (p) (string-prefix-p "PS1=" p)) pairs))
    ;; Non-exported and null-valued variables filtered out.
    (should-not (seq-find (lambda (p) (string-prefix-p "shellHook=" p)) pairs))
    (should-not (seq-find (lambda (p) (string-prefix-p "BROKEN=" p)) pairs))))

(ert-deftest devenv-test-env-parse-custom-ignore ()
  "The IGNORED argument overrides `devenv-env-ignored-variables'."
  (let ((pairs (devenv-env--parse devenv-test--env-json '("PATH"))))
    (should-not (seq-find (lambda (p) (string-prefix-p "PATH=" p)) pairs))
    (should (seq-find (lambda (p) (string-prefix-p "PS1=" p)) pairs))))

(ert-deftest devenv-test-env-path-extraction ()
  "PATH pairs convert to an `exec-path'-shaped list."
  (let ((path (devenv-env--path-from-pairs
               '("FOO=bar" "PATH=/nix/store/aaa/bin:/usr/bin"))))
    (should (equal (butlast path)
                   '("/nix/store/aaa/bin/" "/usr/bin/")))
    ;; `exec-directory' stays at the tail, envrc-style.
    (should (equal (car (last path)) exec-directory)))
  (should (null (devenv-env--path-from-pairs '("FOO=bar")))))

(ert-deftest devenv-test-env-apply-buffer-local ()
  "Applying pairs sets buffer-local env without touching globals."
  (with-temp-buffer
    (devenv-env--apply (current-buffer)
                       '("DEVENV_TEST_MARKER=yes"
                         "PATH=/devenv-test-bin:/usr/bin"))
    (should (local-variable-p 'process-environment))
    (should (local-variable-p 'exec-path))
    (should (equal (getenv "DEVENV_TEST_MARKER") "yes"))
    (should (member "/devenv-test-bin/" exec-path)))
  ;; Global environment untouched.
  (should-not (getenv "DEVENV_TEST_MARKER")))

;;;; Process list parsing

(ert-deftest devenv-test-processes-parse-json-array ()
  "A JSON array of process objects yields (NAME STATUS DETAIL) rows."
  (let ((rows (devenv-processes--parse-json
               '(((name . "web") (status . "running") (pid . 4242))
                 ((name . "db") (status . "stopped"))))))
    (should (equal (car rows) '("web" "running" "4242")))
    (should (equal (cadr rows) '("db" "stopped" "")))))

(ert-deftest devenv-test-processes-parse-json-object ()
  "A JSON object keyed by process name also yields rows."
  (let ((rows (devenv-processes--parse-json
               '((web . ((status . "running") (pid . 4242)))))))
    (should (equal (car rows) '("web" "running" "4242")))))

(ert-deftest devenv-test-processes-parse-table ()
  "The plain-text table output parses into rows, skipping headers."
  (let ((rows (devenv-processes--parse-table
               "NAME      STATUS    PID\nweb       running   4242\ndb        stopped\n")))
    (should (equal (car rows) '("web" "running" "4242")))
    (should (equal (cadr rows) '("db" "stopped" "")))))

(ert-deftest devenv-test-processes-entries-shape ()
  "Rows become tabulated-list entries keyed by process name."
  (let ((entries (devenv-processes--entries '(("web" "running" "4242")))))
    (should (equal (caar entries) "web"))
    (should (equal (cadar entries) ["web" "running" "4242"]))))

;;;; devenv.nix routing predicate

(ert-deftest devenv-test-devenv-nix-file-p ()
  "Only devenv.nix / devenv.local.nix inside a devenv project match."
  (let* ((root (make-temp-file "devenv-test-root" t))
         (config (expand-file-name "devenv.nix" root)))
    (unwind-protect
        (progn
          (with-temp-file config (insert "{ }\n"))
          (should (devenv--devenv-nix-file-p config))
          (with-temp-file (expand-file-name "devenv.local.nix" root)
            (insert "{ }\n"))
          (should (devenv--devenv-nix-file-p
                   (expand-file-name "devenv.local.nix" root)))
          ;; Other Nix files in the same project do not match.
          (should-not (devenv--devenv-nix-file-p
                       (expand-file-name "flake.nix" root)))
          ;; A devenv.nix basename outside any devenv project does not
          ;; match either (no dominating devenv.nix above temp roots
          ;; is assumed; the file itself dominates its own directory).
          (should-not (devenv--devenv-nix-file-p nil)))
      (delete-directory root t))))

;;;; Eglot contact routing

(ert-deftest devenv-test-eglot-setup-captures-fallback ()
  "Setup captures the previous Nix contact and installs the router."
  (let ((eglot-server-programs
         (list (cons '(nix-mode nix-ts-mode) '("nil"))))
        (devenv--eglot-fallback-contact '("nil")))
    (devenv-eglot-setup)
    (should (equal devenv--eglot-fallback-contact '("nil")))
    (should (eq (cdar eglot-server-programs) #'devenv--eglot-contact))
    ;; Idempotent: a second call adds nothing.
    (let ((len (length eglot-server-programs)))
      (devenv-eglot-setup)
      (should (= (length eglot-server-programs) len)))))

(ert-deftest devenv-test-eglot-contact-fallback-for-plain-nix ()
  "Non-devenv Nix buffers get the captured fallback contact."
  (let ((devenv--eglot-fallback-contact '("nil")))
    (with-temp-buffer
      ;; No file name: never a devenv.nix buffer.
      (should (equal (devenv--eglot-contact) '("nil"))))))

(ert-deftest devenv-test-eglot-contact-functional-fallback ()
  "A functional fallback (eglot-alternatives) is called through."
  (let ((devenv--eglot-fallback-contact
         (lambda (&optional _interactive) '("alt-server"))))
    (with-temp-buffer
      (should (equal (devenv--eglot-contact) '("alt-server"))))))

(ert-deftest devenv-test-eglot-entry-covers-nix-p ()
  "Key matching handles symbols, lists, and :language-id forms."
  (should (devenv--eglot-entry-covers-nix-p 'nix-mode))
  (should (devenv--eglot-entry-covers-nix-p '(nix-mode nix-ts-mode)))
  (should (devenv--eglot-entry-covers-nix-p
           '((nix-ts-mode :language-id "nix"))))
  (should-not (devenv--eglot-entry-covers-nix-p 'python-mode))
  (should-not (devenv--eglot-entry-covers-nix-p '(go-mode go-ts-mode))))

;;;; MCP server naming

(ert-deftest devenv-test-mcp-server-name ()
  "MCP server entries are named after the project directory."
  (should (equal (devenv--mcp-server-name "/home/u/proj/") "devenv-proj"))
  (should (equal (devenv--mcp-server-name "/home/u/proj") "devenv-proj")))

(ert-deftest devenv-test-mcp-command-quoting ()
  "The MCP launch command shell-quotes both the root and the binary."
  (let ((devenv-executable "/opt/my tools/devenv"))
    (let ((cmd (devenv--mcp-command "/home/u/my proj/")))
      (should-not (string-match-p "/home/u/my proj" cmd))
      (should-not (string-match-p "/opt/my tools/devenv mcp" cmd))
      (should (string-suffix-p " mcp" cmd))
      (should (string-prefix-p "cd " cmd)))))

(provide 'devenv-test)
;;; devenv-test.el ends here
