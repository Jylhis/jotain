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
(require 'cl-lib)
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
   ;; Build-only variables devenv's own shellHook unsets or nix never
   ;; exports.  TMPDIR names the build sandbox and must never be applied.
   "\"TMPDIR\":{\"type\":\"exported\",\"value\":\"/build\"},"
   "\"TMP\":{\"type\":\"exported\",\"value\":\"/build\"},"
   "\"NIX_BUILD_TOP\":{\"type\":\"exported\",\"value\":\"/build\"},"
   "\"NIX_ENFORCE_PURITY\":{\"type\":\"exported\",\"value\":\"1\"},"
   "\"HOME\":{\"type\":\"exported\",\"value\":\"/homeless-shelter\"},"
   "\"SHELL\":{\"type\":\"exported\",\"value\":\"/nix/store/bbb/bin/bash\"},"
   "\"out\":{\"type\":\"exported\",\"value\":\"/nix/store/ccc-shell-env\"},"
   "\"outputs\":{\"type\":\"exported\",\"value\":\"out\"},"
   "\"stdenv\":{\"type\":\"exported\",\"value\":\"/nix/store/ddd-stdenv\"},"
   ;; Real devenv exports shellHook; it is filtered by name, not by type.
   "\"shellHook\":{\"type\":\"exported\",\"value\":\"echo hi\"},"
   ;; Genuinely non-exported, so the type gate keeps its coverage.
   "\"checkPhase\":{\"type\":\"var\",\"value\":\"runHook check\"},"
   ;; Variables a terminal shell does get, and the loader must keep.
   "\"SOURCE_DATE_EPOCH\":{\"type\":\"exported\",\"value\":\"315532800\"},"
   "\"IN_NIX_SHELL\":{\"type\":\"exported\",\"value\":\"impure\"},"
   "\"NIX_CFLAGS_COMPILE\":{\"type\":\"exported\",\"value\":\"-isystem /x\"},"
   "\"CC\":{\"type\":\"exported\",\"value\":\"gcc\"},"
   "\"RUST_SRC_PATH\":{\"type\":\"exported\",\"value\":\"/nix/store/eee-src\"},"
   "\"MY_APP_ENV\":{\"type\":\"exported\",\"value\":\"dev\"},"
   "\"BROKEN\":{\"type\":\"exported\",\"value\":null}"
   "}}")
  "Canned `devenv print-dev-env --json' output.")

(defun devenv-test--pair (pairs name)
  "Return NAME's applied value in PAIRS, or nil."
  (devenv-env--pair-value pairs name))

(ert-deftest devenv-test-env-parse-exported-only ()
  "Only exported string variables become NAME=VALUE pairs."
  (let ((pairs (devenv-env--parse devenv-test--env-json)))
    (should (member "PATH=/nix/store/aaa/bin:/usr/bin" pairs))
    (should (member "DEVENV_ROOT=/proj" pairs))
    ;; Ignored variable filtered out.
    (should-not (devenv-test--pair pairs "PS1"))
    ;; Non-exported and null-valued variables filtered out.
    (should-not (devenv-test--pair pairs "checkPhase"))
    (should-not (devenv-test--pair pairs "BROKEN"))))

(ert-deftest devenv-test-env-parse-drops-build-only-vars ()
  "Variables a real devenv shell never exports are never applied.
TMPDIR and friends name the build sandbox's /build, which does not
exist on the host: applying them breaks every tool that needs a temp
directory, rust-analyzer's proc-macro server included."
  (let ((pairs (devenv-env--parse devenv-test--env-json)))
    (dolist (name '("TMPDIR" "TMP" "NIX_BUILD_TOP" "NIX_ENFORCE_PURITY"
                    "HOME" "SHELL" "out" "outputs" "stdenv" "shellHook"
                    "PS1"))
      (should-not (devenv-test--pair pairs name)))))

(ert-deftest devenv-test-env-parse-keeps-toolchain-vars ()
  "Variables a terminal devenv shell does get survive the filter."
  (let ((pairs (devenv-env--parse devenv-test--env-json)))
    (dolist (name '("PATH" "DEVENV_ROOT" "SOURCE_DATE_EPOCH" "IN_NIX_SHELL"
                    "NIX_CFLAGS_COMPILE" "CC" "RUST_SRC_PATH" "MY_APP_ENV"))
      (should (devenv-test--pair pairs name)))))

(ert-deftest devenv-test-env-parse-custom-ignore ()
  "The IGNORED argument overrides every default ignore list."
  (let ((pairs (devenv-env--parse devenv-test--env-json '("PATH"))))
    (should-not (devenv-test--pair pairs "PATH"))
    (should (devenv-test--pair pairs "PS1"))))

(ert-deftest devenv-test-env-parse-extra-ignored ()
  "`devenv-env-ignored-variables' filters on top of the constant set."
  (let* ((devenv-env-ignored-variables '("MY_APP_ENV"))
         (pairs (devenv-env--parse devenv-test--env-json)))
    (should-not (devenv-test--pair pairs "MY_APP_ENV"))
    ;; The built-in set still applies.
    (should-not (devenv-test--pair pairs "TMPDIR"))
    (should (devenv-test--pair pairs "CC"))))

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

;;;; Environment layering (PATH must extend, not replace)

(ert-deftest devenv-test-env-layer-value ()
  "A layered value keeps its own entries first and the login ones after."
  (cl-letf (((default-value 'process-environment)
             '("PATH=/b:/usr/bin" "TMPDIR=/tmp")))
    ;; Project entries first, login tail preserved, /b deduped.
    (should (equal (devenv-env--layer-value "PATH" "/a:/b")
                   "/a:/b:/usr/bin"))
    ;; Empty components dropped.
    (should (equal (devenv-env--layer-value "PATH" "/a::")
                   "/a:/b:/usr/bin"))
    ;; No project value: just the inherited one.
    (should (equal (devenv-env--layer-value "PATH" "") "/b:/usr/bin"))
    ;; Unknown variable has no global side.
    (should (equal (devenv-env--layer-value "NOPE" "/a") "/a"))))

(ert-deftest devenv-test-env-layer-pairs ()
  "Only `devenv-env-layered-variables' are layered; others pass through."
  (cl-letf (((default-value 'process-environment)
             '("PATH=/usr/bin" "XDG_DATA_DIRS=/usr/share")))
    (let ((pairs (devenv-env--layer-pairs
                  '("PATH=/nix/bin"
                    "XDG_DATA_DIRS=/nix/share"
                    "FOO=bar"
                    ;; A value containing "=" must not be mangled.
                    "NIX_LDFLAGS=-L/x -Wl,-rpath=/y"
                    "UNSET_ME"))))
      (should (equal (devenv-env--pair-value pairs "PATH")
                     "/nix/bin:/usr/bin"))
      (should (equal (devenv-env--pair-value pairs "XDG_DATA_DIRS")
                     "/nix/share:/usr/share"))
      (should (member "FOO=bar" pairs))
      (should (member "NIX_LDFLAGS=-L/x -Wl,-rpath=/y" pairs))
      ;; Bare names (meaning "unset") survive untouched.
      (should (member "UNSET_ME" pairs)))))

(ert-deftest devenv-test-env-apply-layers-path ()
  "An applied PATH keeps the login entries reachable, `devenv' included."
  (cl-letf (((default-value 'process-environment)
             '("PATH=/login/bin" "TMPDIR=/tmp")))
    (with-temp-buffer
      (devenv-env--apply (current-buffer) '("PATH=/nix/store/aaa/bin"))
      (should (equal (getenv "PATH") "/nix/store/aaa/bin:/login/bin"))
      (should (member "/nix/store/aaa/bin/" exec-path))
      (should (member "/login/bin/" exec-path))
      (should (equal (car (last exec-path)) exec-directory)))))

(ert-deftest devenv-test-env-apply-is-idempotent ()
  "Re-applying the same pairs does not stack PATH entries."
  (cl-letf (((default-value 'process-environment) '("PATH=/login/bin")))
    (with-temp-buffer
      (devenv-env--apply (current-buffer) '("PATH=/nix/bin"))
      (let ((first (getenv "PATH")))
        (devenv-env--apply (current-buffer) '("PATH=/nix/bin"))
        (should (equal (getenv "PATH") first))))))

(ert-deftest devenv-test-env-apply-keeps-host-tmpdir ()
  "The host TMPDIR survives, so temp-dir-using tools keep working.
Regression test for rust-analyzer's proc-macro server dying on the
build sandbox's TMPDIR=/build."
  (cl-letf (((default-value 'process-environment)
             '("PATH=/login/bin" "TMPDIR=/tmp")))
    (with-temp-buffer
      (devenv-env--apply (current-buffer)
                         (devenv-env--parse devenv-test--env-json))
      (should (equal (getenv "TMPDIR") "/tmp"))
      (should-not (getenv "NIX_BUILD_TOP"))
      (should-not (getenv "out"))
      ;; The toolchain variables are still there.
      (should (equal (getenv "CC") "gcc")))))

;;;; Environment sourcing (print-dev-env in bash)

(ert-deftest devenv-test-env-script-with-hook ()
  "The shellHook eval is added when missing and never duplicated."
  (let ((with-eval (concat "export FOO=1\n" devenv-env--shell-hook-eval "\n")))
    (should (equal (devenv-env--script-with-hook with-eval) with-eval)))
  (let ((script (devenv-env--script-with-hook "export FOO=1")))
    (should (string-suffix-p (concat devenv-env--shell-hook-eval "\n") script))
    (should (equal 1 (cl-count-if
                      (lambda (line)
                        (equal line devenv-env--shell-hook-eval))
                      (split-string script "\n"))))))

(ert-deftest devenv-test-env-parse-dump ()
  "A NUL-separated `env -0' dump parses into name/value cells."
  (let ((vars (devenv-env--parse-dump
               (concat "PATH=/nix/bin\0"
                       "MULTI=line one\nline two\0"
                       "EQUALS=-Wl,-rpath=/y\0"
                       "NOVALUE=\0"
                       "junk-without-equals\0"))))
    (should (equal (cdr (assoc "PATH" vars)) "/nix/bin"))
    (should (equal (cdr (assoc "MULTI" vars)) "line one\nline two"))
    (should (equal (cdr (assoc "EQUALS" vars)) "-Wl,-rpath=/y"))
    (should (equal (cdr (assoc "NOVALUE" vars)) ""))
    (should-not (assoc "junk-without-equals" vars))))

(ert-deftest devenv-test-env-dump-delta ()
  "Only the difference against Emacs's own environment is applied."
  (cl-letf (((default-value 'process-environment)
             '("PATH=/login/bin" "TMPDIR=/tmp" "GONE=1" "SAME=keep")))
    (let ((pairs (devenv-env--dump-delta
                  '(("PATH" . "/nix/bin:/login/bin")
                    ("SAME" . "keep")
                    ("NEW" . "yes")
                    ("TMPDIR" . "/tmp")
                    ;; Shell bookkeeping, ignored in both directions.
                    ("SHLVL" . "2")
                    ("_" . "/nix/bin/env")
                    ("BASH_VERSION" . "5.3")
                    ("nix_saved_PATH" . "/login/bin")))))
      (should (member "PATH=/nix/bin:/login/bin" pairs))
      (should (member "NEW=yes" pairs))
      ;; Unchanged variables are not repeated.
      (should-not (member "SAME=keep" pairs))
      (should-not (member "TMPDIR=/tmp" pairs))
      ;; A variable the shell removed becomes a bare name, i.e. unset.
      (should (member "GONE" pairs))
      (dolist (name '("SHLVL" "_" "BASH_VERSION" "nix_saved_PATH"))
        (should-not (devenv-env--pair-value pairs name))))))

(ert-deftest devenv-test-env-dump-delta-drops-extra-env ()
  "`devenv-extra-env' is loader plumbing and never reaches the project.
It is handed to the sourcing shell, so the dump echoes it back; applying
it would export the quiet-mode flag to every tool the project runs."
  (cl-letf (((default-value 'process-environment) '("PATH=/login/bin")))
    (let* ((devenv-extra-env '("AI_AGENT=1"))
           (pairs (devenv-env--dump-delta '(("PATH" . "/nix/bin")
                                           ("AI_AGENT" . "1")))))
      (should-not (devenv-env--pair-value pairs "AI_AGENT"))
      (should-not (member "AI_AGENT" pairs))
      (should (member "PATH=/nix/bin" pairs)))))

(ert-deftest devenv-test-env-dump-delta-unset-applies ()
  "A bare name from the delta really unsets the variable in a buffer."
  (cl-letf (((default-value 'process-environment)
             '("PATH=/login/bin" "GONE=1")))
    (with-temp-buffer
      (devenv-env--apply (current-buffer)
                         (devenv-env--dump-delta '(("PATH" . "/nix/bin"))))
      (should-not (getenv "GONE")))))

;;;; Executable resolution

(ert-deftest devenv-test-ensure-executable-global-fallback ()
  "A buffer-local PATH cannot hide the devenv CLI.
The project's own profile need not contain devenv, so resolution falls
back to the global `exec-path' and returns an absolute path."
  (let* ((dir (make-temp-file "devenv-bin" t))
         (name "devenv-test-fake")
         (program (expand-file-name name dir)))
    (unwind-protect
        (progn
          (with-temp-file program (insert "#!/bin/sh\nexit 0\n"))
          (set-file-modes program #o755)
          (let ((devenv-executable name))
            ;; Reachable from this buffer: used as given.
            (let ((exec-path (list dir)))
              (should (equal (devenv--ensure-executable) name)))
            ;; Hidden by a buffer-local `exec-path' (what the devenv env
            ;; loader installs), still on the global one: absolute path.
            (let ((exec-path (list dir)))
              (with-temp-buffer
                (setq-local exec-path '("/devenv-test-nonexistent"))
                (should (equal (devenv--ensure-executable) program))))
            ;; Nowhere at all: a clean user error.
            (let ((exec-path '("/devenv-test-nonexistent")))
              (should-error (devenv--ensure-executable) :type 'user-error))))
      (delete-directory dir t))))

;;;; Environment fetch bookkeeping

(defvar devenv-test--eglot-calls 0
  "Stubbed `eglot-ensure' call count (see `devenv-test--with-env-state').")

(defmacro devenv-test--with-env-state (&rest body)
  "Run BODY with fresh loader state and a stubbed `eglot-ensure'.
The variable `devenv-test--eglot-calls' counts replayed calls."
  (declare (indent 0) (debug t))
  `(let ((devenv-env--pending (make-hash-table :test #'equal))
         (devenv--cache (make-hash-table :test #'equal))
         (devenv-env--eglot-replay nil)
         (devenv-test--eglot-calls 0))
     (cl-letf (((symbol-function 'eglot-ensure)
                (lambda () (cl-incf devenv-test--eglot-calls))))
       ,@body)))

(ert-deftest devenv-test-env-handle-pairs-failure-clears-pending ()
  "A failed load clears pending state instead of parking eglot forever.
Leaving the root pending keeps `devenv-env-loading-p' non-nil, which
silently disables eglot auto-start for the whole project."
  (devenv-test--with-env-state
    (let ((buffer (generate-new-buffer " *devenv-test*"))
          (root "/proj/"))
      (unwind-protect
          (progn
            (puthash root (list buffer) devenv-env--pending)
            (setq devenv-env--eglot-replay (list buffer))
            (devenv-env--handle-pairs root nil)
            (should-not (gethash root devenv-env--pending))
            (should-not devenv-env--eglot-replay)
            ;; Released, deliberately not connected with the global env.
            (should (equal devenv-test--eglot-calls 0))
            (should-not (gethash (cons root 'env) devenv--cache)))
        (kill-buffer buffer)))))

(ert-deftest devenv-test-env-handle-pairs-success-replays-eglot ()
  "A successful load applies the env, caches it, and replays eglot once."
  (devenv-test--with-env-state
    (let ((buffer (generate-new-buffer " *devenv-test*"))
          (root "/proj/"))
      (unwind-protect
          (progn
            (puthash root (list buffer) devenv-env--pending)
            (setq devenv-env--eglot-replay (list buffer))
            (devenv-env--handle-pairs root '("DEVENV_TEST_OK=1"))
            (should-not (gethash root devenv-env--pending))
            (should-not devenv-env--eglot-replay)
            (should (equal devenv-test--eglot-calls 1))
            (should (gethash (cons root 'env) devenv--cache))
            (with-current-buffer buffer
              (should (equal (getenv "DEVENV_TEST_OK") "1"))))
        (kill-buffer buffer)))))

(ert-deftest devenv-test-env-release-eglot-keeps-other-roots ()
  "Buffers parked for another still-pending root stay parked."
  (devenv-test--with-env-state
    (let ((mine (generate-new-buffer " *devenv-test-mine*"))
          (other (generate-new-buffer " *devenv-test-other*")))
      (unwind-protect
          (progn
            (setq devenv-env--eglot-replay (list mine other))
            (should (equal (devenv-env--release-eglot (list mine)) 1))
            (should (equal devenv-env--eglot-replay (list other))))
        (kill-buffer mine)
        (kill-buffer other)))))

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
  "Non-devenv Nix buffers get the captured fallback contact.
No Nix LSP is configured, so the fallback branch is reached even when
nixd happens to be on PATH (e.g. the CI sandbox)."
  (let ((devenv--eglot-fallback-contact '("nil"))
        (devenv-nix-lsp-servers nil))
    (with-temp-buffer
      ;; No file name: never a devenv.nix buffer.
      (should (equal (devenv--eglot-contact) '("nil"))))))

(ert-deftest devenv-test-eglot-contact-functional-fallback ()
  "A functional fallback (eglot-alternatives) is called through."
  (let ((devenv--eglot-fallback-contact
         (lambda (&optional _interactive) '("alt-server")))
        (devenv-nix-lsp-servers nil))
    (with-temp-buffer
      (should (equal (devenv--eglot-contact) '("alt-server"))))))

(ert-deftest devenv-test-eglot-contact-prefers-nix-lsp ()
  "An ordinary Nix buffer uses the resolved Nix LSP over the fallback."
  (let ((devenv--eglot-fallback-contact '("nil")))
    (cl-letf (((symbol-function 'devenv--nix-lsp-program)
               (lambda () "/usr/bin/nixd")))
      (with-temp-buffer
        (should (equal (devenv--eglot-contact) '("/usr/bin/nixd")))))))

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

;;;; Auto-activation trust (devenv allow / revoke / hook-should-activate)

(ert-deftest devenv-test-activation-state ()
  "Exit code and stdout of hook-should-activate map to trust states.
Exit 0 with the project path on stdout means trusted; exit 2
means the project exists but is not trusted; exit 0 with empty
output means no project; anything else (including a missing
subcommand on devenv 1.x) is `unsupported'."
  (should (eq (devenv--activation-state 0 "/home/u/proj\n") 'allowed))
  (should (eq (devenv--activation-state 2 "") 'blocked))
  (should (eq (devenv--activation-state 0 "") 'no-project))
  (should (eq (devenv--activation-state 0 "  \n") 'no-project))
  (should (eq (devenv--activation-state 1 "error: unrecognized subcommand")
              'unsupported))
  (should (eq (devenv--activation-state nil "") 'unsupported)))

(ert-deftest devenv-test-activation-state-permits-loading ()
  "Trusted and pre-trust-model devenv versions permit auto-loading."
  (should (devenv--activation-permits-p 'allowed))
  (should (devenv--activation-permits-p 'unsupported))
  (should-not (devenv--activation-permits-p 'blocked))
  (should-not (devenv--activation-permits-p 'no-project)))

;;;; Modeline status

(ert-deftest devenv-test-modeline-compute-state ()
  "Modeline state derives from root, active env, and trust state."
  ;; Not in a devenv project.
  (should (eq (devenv-modeline--compute-state nil nil nil) 'none))
  ;; Environment loaded (DEVENV_PROFILE visible in the buffer).
  (should (eq (devenv-modeline--compute-state "/p/" "/nix/store/x" nil)
              'active))
  ;; Project present, trust denied.
  (should (eq (devenv-modeline--compute-state "/p/" nil 'blocked)
              'blocked))
  ;; Project present, env not loaded (trusted, unknown, or no info).
  (should (eq (devenv-modeline--compute-state "/p/" nil 'allowed)
              'inactive))
  (should (eq (devenv-modeline--compute-state "/p/" nil nil) 'inactive)))

(ert-deftest devenv-test-modeline-format ()
  "Each modeline state renders its label; `none' renders empty."
  (should (equal (devenv-modeline--format 'none) ""))
  (should (string-match-p "devenv\\[on\\]" (devenv-modeline--format 'active)))
  (should (string-match-p "devenv\\[off\\]"
                          (devenv-modeline--format 'inactive)))
  (should (string-match-p "devenv\\[!\\]"
                          (devenv-modeline--format 'blocked)))
  ;; Non-empty segments carry a leading space so they never glue onto
  ;; the previous mode-line element.
  (dolist (state '(active inactive blocked))
    (should (string-prefix-p " " (devenv-modeline--format state)))))

(ert-deftest devenv-test-mcp-command-quoting ()
  "The MCP launch command shell-quotes both the root and the binary."
  (let ((devenv-executable "/opt/my tools/devenv"))
    (let ((cmd (devenv--mcp-command "/home/u/my proj/")))
      (should-not (string-match-p "/home/u/my proj" cmd))
      (should-not (string-match-p "/opt/my tools/devenv mcp" cmd))
      (should (string-suffix-p " mcp" cmd))
      (should (string-prefix-p "cd " cmd)))))

;;;; Invocation log formatting

(ert-deftest devenv-test-log-exit ()
  "Exit values map to a glyph, a face, and a label."
  (should (equal (devenv--log-exit 0) '("✓" devenv-log-success "0")))
  (should (equal (devenv--log-exit 1) '("✗" devenv-log-failure "1")))
  (should (equal (devenv--log-exit 'timeout)
                 '("✗" devenv-log-failure "timeout")))
  (should (equal (devenv--log-exit 'compile)
                 '("▶" devenv-log-info "dispatched"))))

(ert-deftest devenv-test-log-summarize ()
  "Summaries carry a human size and a line count that pluralizes."
  (should (string-match-p "1 line>" (devenv--log-summarize "one line")))
  (should (string-match-p "3 lines>" (devenv--log-summarize "a\nb\nc"))))

(ert-deftest devenv-test-log-pretty-json ()
  "Valid JSON is re-indented; anything else yields nil."
  (should (string-match-p "\n  \"a\": 1" (devenv--log-pretty-json "{\"a\":1}")))
  (should (null (devenv--log-pretty-json "garbage {"))))

(ert-deftest devenv-test-log-truncate ()
  "Over-cap output is cut on a line boundary with an elision note."
  (let* ((devenv-log-max-output 10)
         (output "line one\nline two\nline three\n")
         (cut (devenv--log-truncate output)))
    (should (< (length cut) (length output)))
    (should (string-match-p "truncated" cut))
    ;; The cut lands on the first newline, keeping only "line one".
    (should (string-prefix-p "line one\n" cut))
    (should-not (string-match-p "line three" cut))))

(ert-deftest devenv-test-log-renders-ansi-and-keeps-failures ()
  "A failed entry strips ANSI escapes but keeps the trace text."
  (let ((devenv-log-buffer-name " *devenv-test-log*"))
    (unwind-protect
        (progn
          (devenv--log "/p/" '("devenv" "-q" "info") 1
                       "\e[31m× boom\e[0m\ntrace line\n")
          (with-current-buffer devenv-log-buffer-name
            (let ((s (buffer-string)))
              (should-not (string-search "\e[31m" s))
              (should (string-search "× boom" s))
              (should (string-search "trace line" s))
              (should (string-search "✗ 1" s)))))
      (when (get-buffer devenv-log-buffer-name)
        (kill-buffer devenv-log-buffer-name)))))

(ert-deftest devenv-test-log-summarizes-large-success-with-expand ()
  "Large successful JSON is summarized behind an expand button.
Activating the button replaces the summary with pretty JSON."
  (let ((devenv-log-buffer-name " *devenv-test-log*")
        (devenv-log-max-output 100))
    (unwind-protect
        (progn
          (devenv--log "/p/" '("devenv" "-q" "print-dev-env" "--json") 0
                       (concat "{\"data\":\"" (make-string 200 ?x) "\",\"n\":7}"))
          (with-current-buffer devenv-log-buffer-name
            (should (string-search "<output:" (buffer-string)))
            (let ((btn (next-button (point-min))))
              (should btn)
              (button-activate btn))
            (let ((s (buffer-string)))
              (should-not (string-search "<output:" s))
              (should (string-search "\n  \"n\": 7" s)))))
      (when (get-buffer devenv-log-buffer-name)
        (kill-buffer devenv-log-buffer-name)))))

(provide 'devenv-test)
;;; devenv-test.el ends here
