;;; devenv.el --- devenv.sh integration: tasks, processes, env, LSP, MCP -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;; Author: Markus Jylhänkangas <markus@jylhis.com>
;; Maintainer: Markus Jylhänkangas <markus@jylhis.com>
;; URL: https://github.com/Jylhis/jotain
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (transient "0.7.4"))
;; Keywords: processes, tools, unix

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

;; Native Emacs integration for devenv <https://devenv.sh>, the
;; Nix-based developer environment tool.  Everything is driven through
;; the `devenv' CLI (2.x recommended; older versions degrade
;; gracefully):
;;
;;   `devenv'                  Transient entry point with all commands.
;;   `devenv-task-run'         Pick and run a task (`devenv tasks run').
;;   `devenv-script-run'       Pick and run a script from the config.
;;   `devenv-test'             `devenv test' in a compilation buffer.
;;   `devenv-build'            `devenv build' in a compilation buffer.
;;   `devenv-up' / `devenv-down'
;;                             Start/stop the project's processes.
;;   `devenv-processes'        Tabulated dashboard over the process
;;                             manager (start/stop/restart/logs).
;;   `devenv-info' / `devenv-search' / `devenv-eval'
;;                             Introspection of the environment.
;;   `devenv-reload'           Refresh the environment (delegates to
;;                             envrc when it manages the project) and
;;                             offer to reconnect eglot servers.
;;   `devenv-env-global-mode'  Native environment loader: reproduces a
;;                             terminal `devenv shell' and applies it
;;                             buffer-locally, envrc-style.
;;   `devenv-eglot-setup'      Route devenv.nix buffers to the bundled
;;                             `devenv lsp' server (a nixd preloaded
;;                             with the project's devenv options) while
;;                             other Nix buffers keep their server.
;;   `devenv-mcp-setup'        Register the project's `devenv mcp'
;;                             stdio server with mcp.el so gptel and
;;                             friends can call its tools.
;;
;; Environment loading: `devenv-env-global-mode' is the native loader.
;; It sources `devenv print-dev-env' in bash, shellHook included, and
;; applies the result buffer-locally, so a buffer sees what a terminal
;; `devenv shell' sees.  `devenv-env-loader' can switch to parsing the
;; JSON form instead, which runs no project code but misses everything
;; the shellHook sets.  Either way PATH and XDG_DATA_DIRS are layered
;; over the login values (`devenv-env-layered-variables') and the
;; derivation-only variables the JSON carries are dropped
;; (`devenv-env-never-applied-variables'): applied verbatim they point
;; TMPDIR at the build sandbox's /build, which breaks every tool that
;; needs a temp dir, rust-analyzer's proc-macro server included.
;;
;; With direnv, envrc.el plus `use devenv' in .envrc is an equally good
;; substrate: `devenv-env-defer-to-direnv' (default t) keeps the native
;; loader out of projects that have a .envrc, so the two can never
;; double-manage a buffer.  Buffer-local values propagate to temp
;; buffers through inheritenv when that package is installed (envrc
;; depends on it).
;;
;; All subprocess invocations set AI_AGENT=1 (see `devenv-extra-env'),
;; which puts devenv 2.1+ into quiet mode: the TUI is suppressed and
;; stdout stays machine-readable.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'json)
(require 'compile)
(require 'ansi-color)
(require 'tabulated-list)
(require 'transient)

;; Soft dependencies — only called when the feature is loaded.
(declare-function envrc-reload-all "envrc" ())
(declare-function eglot-reconnect "eglot" (server &optional interactive))
(declare-function eglot-current-server "eglot" ())
(declare-function eglot-managed-p "eglot" ())
(declare-function eglot-ensure "eglot" ())
(defvar eglot-server-programs)
(defvar mcp-hub-servers)
(defvar envrc-mode)

;; Defined later in this file; declared for forward references.
(defvar devenv-env-global-mode)
(defvar devenv-env-mode)

;;;; Customization

(defgroup devenv nil
  "Integration with devenv.sh developer environments."
  :group 'tools
  :prefix "devenv-")

(defcustom devenv-executable "devenv"
  "Name or path of the devenv executable."
  :type 'string)

(defcustom devenv-extra-env '("AI_AGENT=1")
  "Environment strings prepended to every devenv subprocess.
The default AI_AGENT=1 switches devenv 2.1+ into quiet mode so
the progress TUI never interleaves with machine-readable output."
  :type '(repeat string))

(defcustom devenv-global-arguments '("-q")
  "Arguments inserted after the executable in every invocation."
  :type '(repeat string))

(defcustom devenv-cache-ttl 30
  "Seconds to keep cached introspection results (tasks, scripts)."
  :type 'natnum)

(defcustom devenv-command-timeout 60
  "Seconds to wait for a synchronous devenv command before giving up."
  :type 'natnum)

(defcustom devenv-up-detached nil
  "When non-nil, `devenv-up' starts processes detached (devenv up -d).
Otherwise processes run attached in a live output buffer."
  :type 'boolean)

(defcustom devenv-env-loader 'shell
  "How the native loader obtains a project's environment.

The symbol `shell' sources `devenv print-dev-env' in bash and dumps
the resulting environment.  This runs the project's shellHook, so
hook-only variables (LOCALE_ARCHIVE, MANPATH, PATH additions such as
$CARGO_HOME/bin, anything `enterShell' exports) are included, exactly
as in a terminal shell.  Only projects trusted for auto-activation
\(`devenv-allow') are sourced.

The symbol `json' parses `devenv print-dev-env --json' and filters it
down to what a shell would keep.  No project code runs, but the
shellHook's effects are missing.  This is also the fallback whenever
sourcing is unavailable or fails."
  :type '(choice (const :tag "Source the dev-env in bash" shell)
                 (const :tag "Parse the JSON dev-env" json)))

(defconst devenv-env-never-applied-variables
  '(;; nix's `ignoreVars' (src/nix/develop.cc): dropped from every
    ;; `nix develop' shell, so devenv's own projection never has them.
    ;; The TMP* family and NIX_BUILD_TOP are actively harmful — they
    ;; name the build sandbox's /build, which does not exist on the
    ;; host, and rust-analyzer's proc-macro server dies creating its
    ;; temp dir under it.
    "BASHOPTS" "HOME" "NIX_BUILD_TOP" "NIX_ENFORCE_PURITY" "NIX_LOG_FD"
    "NIX_REMOTE" "PPID" "SHELL" "SHELLOPTS" "SSL_CERT_FILE" "TEMP"
    "TEMPDIR" "TERM" "TMP" "TMPDIR" "TZ" "UID"
    ;; Derivation-only stdenv variables that devenv's shellHook
    ;; `unset's before it hands the shell over.
    "HOST_PATH" "NIX_BUILD_CORES" "__structuredAttrs" "buildInputs"
    "buildPhase" "builder" "depsBuildBuild" "depsBuildBuildPropagated"
    "depsBuildTarget" "depsBuildTargetPropagated" "depsHostHost"
    "depsHostHostPropagated" "depsTargetTarget"
    "depsTargetTargetPropagated" "dontAddDisableDepTrack" "doCheck"
    "doInstallCheck" "nativeBuildInputs" "out" "outputs" "patches"
    "phases" "preferLocalBuild" "propagatedBuildInputs"
    "propagatedNativeBuildInputs" "shell" "shellHook" "stdenv"
    "strictDeps"
    ;; Shell bookkeeping that means nothing in Emacs.
    "PS1" "SHLVL" "PWD" "OLDPWD")
  "Variables `devenv print-dev-env' reports that a shell never exports.
The JSON form is the raw derivation build environment: a terminal
devenv shell sees it only after nix drops its `ignoreVars' and the
shellHook unsets the stdenv build variables.  Applying the unfiltered
set is what makes Emacs and the terminal diverge.  Add project- or
user-specific extras with `devenv-env-ignored-variables'.")

(defcustom devenv-env-ignored-variables nil
  "Extra variables from `devenv print-dev-env' never applied to buffers.
Filtered on top of `devenv-env-never-applied-variables', which already
covers everything a real devenv shell drops."
  :type '(repeat string))

(defcustom devenv-env-shell-ignored-variables
  '("\\`_"
    "\\`BASH_" "\\`COMP_" "\\`READLINE_" "\\`PROMPT"
    "\\`STARSHIP_" "\\`__fish" "\\`DIRENV_" "\\`nix_saved_"
    "\\`\\(SHELL\\|SHELLOPTS\\|BASHOPTS\\|HISTCMD\\|HISTFILE\\)\\'"
    "\\`\\(PS[1-4]\\|MAILCHECK\\|COLUMNS\\|LINES\\|LINENO\\)\\'"
    "\\`\\(RANDOM\\|SRANDOM\\|SECONDS\\)\\'"
    "\\`\\(EPOCHSECONDS\\|EPOCHREALTIME\\)\\'")
  "Regexps for variables the `shell' loader ignores when diffing.
Mirrors devenv's own `__devenv_ignored_var' (see the shell rcfile it
generates): interactive-shell bookkeeping that changes on its own and
must never be pinned into a buffer's environment.  Names in
`devenv-env-never-applied-variables' are ignored as well."
  :type '(repeat regexp))

(defcustom devenv-env-layered-variables '("PATH" "XDG_DATA_DIRS")
  "Colon-path variables extended with, not replaced by, the global value.
A terminal devenv shell prepends the project's entries to the ones it
inherited (see `nix_saved_PATH' in the script devenv generates).
Replacing them instead hides every tool outside the project profile,
the devenv CLI itself included."
  :type '(repeat string))

(defcustom devenv-env-lighter " devenv"
  "Mode-line lighter for `devenv-env-mode'."
  :type 'string)

(defcustom devenv-env-defer-to-direnv t
  "When non-nil, `devenv-env-global-mode' skips direnv-managed projects.
The native loader then activates only in devenv projects that have no
`.envrc' above them and no active `envrc-mode', leaving the environment
to direnv where it is in charge.  Set to nil to let the native loader
own the environment for every trusted devenv project regardless of a
`.envrc' (use this when direnv/envrc is disabled)."
  :type 'boolean)

(defcustom devenv-processes-log-lines 200
  "Number of log lines fetched by `devenv-processes-logs'."
  :type 'natnum)

(defcustom devenv-log-max-output 4000
  "Byte cap for the output shown per entry in `devenv-log-buffer-name'.
Output above this is summarized (for a successful command) or
truncated on a line boundary (for a failed one)."
  :type 'natnum)

;;;; Project root and executable discovery

(defun devenv-project-root (&optional dir)
  "Return the devenv project root at or above DIR, else nil.
The root is the closest directory containing devenv.nix.  DIR
defaults to `default-directory'.  Remote directories are not
supported and return nil."
  (let ((dir (or dir default-directory)))
    (unless (file-remote-p dir)
      (when-let* ((root (locate-dominating-file dir "devenv.nix")))
        (expand-file-name root)))))

(defun devenv--ensure-executable ()
  "Return the program to spawn for devenv, or signal a `user-error'.
`devenv-executable' is returned as-is when the current `exec-path'
resolves it, else its absolute location on the global `exec-path': a
project's devenv profile need not contain the devenv CLI itself, so a
buffer-local PATH must never be able to hide it."
  (or (and (executable-find devenv-executable) devenv-executable)
      (let ((exec-path (default-value 'exec-path)))
        (executable-find devenv-executable))
      (user-error "Cannot find `%s' on PATH; install devenv or set \
`devenv-executable'" devenv-executable)))

(defun devenv--root-or-error (&optional dir)
  "Return the devenv project root covering DIR or signal `user-error'."
  (or (devenv-project-root dir)
      (user-error "No devenv.nix found above %s"
                  (abbreviate-file-name (or dir default-directory)))))

;;;; Logging

(defvar devenv-log-buffer-name "*devenv log*"
  "Name of the buffer recording every devenv CLI invocation.")

(defface devenv-log-timestamp '((t :inherit shadow))
  "Face for the timestamp, separator, and elision notes in the log.")

(defface devenv-log-directory '((t :inherit font-lock-string-face))
  "Face for the project directory on a log entry's header.")

(defface devenv-log-command '((t :inherit font-lock-function-name-face))
  "Face for the `$ command' line of a log entry.")

(defface devenv-log-success '((t :inherit success))
  "Face for the exit line of a successful log entry.")

(defface devenv-log-failure '((t :inherit error))
  "Face for the exit line of a failed or timed-out log entry.")

(defface devenv-log-info '((t :inherit warning))
  "Face for the exit line of a still-running (dispatched) entry.")

(defun devenv--log-exit (exit)
  "Return a (GLYPH FACE LABEL) list describing EXIT for the header."
  (cond ((eql exit 0)       (list "✓" 'devenv-log-success (format "%s" exit)))
        ((eq exit 'compile) (list "▶" 'devenv-log-info "dispatched"))
        ((eq exit 'timeout) (list "✗" 'devenv-log-failure "timeout"))
        (t                  (list "✗" 'devenv-log-failure (format "%s" exit)))))

(defun devenv--log-summarize (output)
  "Return a one-line summary string describing OUTPUT."
  (let ((lines (1+ (cl-count ?\n (string-remove-suffix "\n" output)))))
    (format "<output: %s, %d line%s>"
            (file-size-human-readable (string-bytes output))
            lines (if (= lines 1) "" "s"))))

(defun devenv--log-truncate (output)
  "Return OUTPUT cut on a line boundary near `devenv-log-max-output'.
A shadow-faced note reports how many bytes were elided."
  (let* ((cap devenv-log-max-output)
         (nl (cl-position ?\n output :from-end t :end (min cap (length output))))
         (cut (or nl cap))
         (elided (- (string-bytes output) (string-bytes (substring output 0 cut)))))
    (concat (substring output 0 cut) "\n"
            (propertize (format "[+%s truncated]"
                                (file-size-human-readable elided))
                        'face 'devenv-log-timestamp))))

(defun devenv--log-pretty-json (output)
  "Return OUTPUT re-indented as JSON, or nil when it is not JSON."
  (ignore-errors
    (with-temp-buffer
      (insert (ansi-color-filter-apply output))
      (json-pretty-print-buffer)
      (buffer-string))))

(defun devenv--log-insert-body (text)
  "Insert TEXT as a log entry body, rendering ANSI colors."
  (let ((start (point)))
    (insert text)
    (unless (string-suffix-p "\n" text) (insert "\n"))
    (ansi-color-apply-on-region start (point))))

(defun devenv--log-insert-output (exit output)
  "Insert OUTPUT for a log entry whose command exited EXIT.
Successful large output is summarized behind an expand button that
pretty-prints JSON on demand; failed output is kept and truncated."
  (when (and output (not (string-empty-p output)))
    (if (and (eql exit 0) (> (string-bytes output) devenv-log-max-output))
        (let ((from (copy-marker (point))))
          (insert (propertize (devenv--log-summarize output)
                              'face 'devenv-log-timestamp)
                  " ")
          (insert-text-button
           "[expand]"
           'action (lambda (button)
                     (let ((inhibit-read-only t)
                           (at (button-get button 'devenv-from)))
                       (delete-region at (save-excursion
                                           (goto-char at)
                                           (forward-line 1)
                                           (point)))
                       (goto-char at)
                       (devenv--log-insert-body
                        (or (devenv--log-pretty-json output) output))))
           'devenv-from from
           'help-echo "Show the full output"
           'follow-link t)
          (insert "\n"))
      (devenv--log-insert-body
       (if (> (string-bytes output) devenv-log-max-output)
           (devenv--log-truncate output)
         output)))))

(defun devenv--log (root argv exit output)
  "Append an invocation record to `devenv-log-buffer-name'.
ROOT is the project directory, ARGV the full command list, EXIT
the numeric exit status (or a symbol while still running), and
OUTPUT the captured text (summarized or truncated for display)."
  (with-current-buffer (get-buffer-create devenv-log-buffer-name)
    (unless (derived-mode-p 'special-mode)
      (special-mode))
    (let ((inhibit-read-only t))
      (pcase-let ((`(,glyph ,face ,label) (devenv--log-exit exit)))
        (goto-char (point-max))
        (insert (propertize (concat (make-string 60 ?─) "\n")
                            'face 'devenv-log-timestamp)
                (propertize (format "[%s] " (format-time-string "%F %T"))
                            'face 'devenv-log-timestamp)
                (propertize (concat (abbreviate-file-name root) "\n")
                            'face 'devenv-log-directory)
                (propertize (format "$ %s\n" (mapconcat #'identity argv " "))
                            'face 'devenv-log-command)
                (propertize (format "%s %s\n" glyph label) 'face face))
        (devenv--log-insert-output exit output)))))

(defun devenv-show-log ()
  "Display the devenv invocation log buffer."
  (interactive)
  (display-buffer (get-buffer-create devenv-log-buffer-name)))

;;;; Subprocess plumbing

(defun devenv--command (args &optional global-args program)
  "Return the full argv for a devenv invocation with ARGS.
GLOBAL-ARGS are extra global flags (e.g. from the transient)
inserted between `devenv-global-arguments' and ARGS.  PROGRAM
overrides `devenv-executable' (see `devenv--ensure-executable')."
  (cons (or program devenv-executable)
        (append devenv-global-arguments global-args args)))

(defun devenv--shell-command (argv)
  "Return ARGV quoted for the shell as a single command string."
  (mapconcat #'shell-quote-argument argv " "))

(defun devenv--first-line (string)
  "Return the first non-empty line of STRING, or the empty string."
  (or (seq-find (lambda (line) (not (string-blank-p line)))
                (split-string (or string "") "\n"))
      ""))

(defun devenv--call (root &rest args)
  "Run devenv with ARGS synchronously in ROOT; return stdout.
Waits at most `devenv-command-timeout' seconds.  A non-zero exit
signals a `user-error' carrying the first stderr line."
  (let* ((program (devenv--ensure-executable))
         (default-directory root)
         (process-environment (append devenv-extra-env process-environment))
         (argv (devenv--command args nil program))
         (stdout (generate-new-buffer " *devenv-stdout*"))
         (stderr (generate-new-buffer " *devenv-stderr*")))
    (unwind-protect
        (let ((proc (make-process
                     :name "devenv" :buffer stdout :stderr stderr
                     :command argv :noquery t :connection-type 'pipe
                     :sentinel #'ignore)))
          ;; The pipe process wrapping STDERR must not prompt either.
          (when-let* ((err-proc (get-buffer-process stderr)))
            (set-process-query-on-exit-flag err-proc nil))
          (with-timeout (devenv-command-timeout
                         (delete-process proc)
                         (devenv--log root argv 'timeout nil)
                         (user-error "devenv timed out after %ss: %s"
                                     devenv-command-timeout
                                     (devenv--shell-command argv)))
            (while (process-live-p proc)
              (accept-process-output proc 0.1)))
          ;; Let the stderr pipe drain before reading it.
          (accept-process-output nil 0.05)
          (let ((exit (process-exit-status proc))
                (out (with-current-buffer stdout (buffer-string)))
                (err (with-current-buffer stderr (buffer-string))))
            (devenv--log root argv exit (if (string-empty-p err) out err))
            (unless (zerop exit)
              (user-error "devenv failed (%d): %s" exit
                          (devenv--first-line err)))
            out))
      (kill-buffer stdout)
      (kill-buffer stderr))))

(defun devenv--json-parse (string)
  "Parse JSON STRING with the conventions used across this library.
Objects become alists, arrays become lists, null/false become
nil.  Signals on malformed input."
  (json-parse-string string
                     :object-type 'alist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun devenv--call-json (root &rest args)
  "Run devenv with ARGS in ROOT and parse the stdout as JSON."
  (let ((out (apply #'devenv--call root args)))
    (condition-case err
        (devenv--json-parse out)
      (error (user-error "devenv returned unparseable JSON (%s): %s"
                         (error-message-string err)
                         (devenv--first-line out))))))

(defun devenv--run (root args callback &optional name)
  "Run devenv with ARGS asynchronously in ROOT.
CALLBACK receives (EXIT-CODE OUTPUT) when the process finishes.
NAME labels the process.  If the executable cannot be resolved or
`make-process' itself fails, CALLBACK still runs (with exit code
127) so callers never deadlock waiting for a result."
  (let* ((default-directory root)
         (process-environment (append devenv-extra-env process-environment))
         (buffer (generate-new-buffer
                  (format " *devenv-%s*" (or name "async")))))
    (condition-case err
        ;; Resolving the executable can signal, so it stays inside the
        ;; guard: a caller left waiting for CALLBACK would keep its
        ;; project marked as "environment loading" forever.
        (let ((argv (devenv--command args nil (devenv--ensure-executable))))
          (make-process
           :name (format "devenv-%s" (or name "async"))
           :buffer buffer :command argv :noquery t :connection-type 'pipe
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (let* ((buf (process-buffer proc))
                      (exit (process-exit-status proc))
                      (output (if (buffer-live-p buf)
                                  (with-current-buffer buf (buffer-string))
                                "")))
                 (devenv--log root argv exit output)
                 (when (buffer-live-p buf) (kill-buffer buf))
                 (funcall callback exit output))))))
      (error
       (let ((description (error-message-string err)))
         (devenv--log root (devenv--command args) 127 description)
         (when (buffer-live-p buffer) (kill-buffer buffer))
         (message "devenv: %s" description)
         (funcall callback 127 description))))))

(defvar devenv-compilation-error-regexp-alist-alist
  '((devenv-nix-trace
     "\\bat \\(/[^ :\n]+\\.nix\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)
    (devenv-nix-error "^error: " nil nil nil 2))
  "Error matchers for `devenv-compilation-mode' (Nix eval traces).")

(defvar devenv-compilation-error-regexp-alist
  '(devenv-nix-trace devenv-nix-error)
  "Active error matchers for `devenv-compilation-mode'.")

(define-compilation-mode devenv-compilation-mode "devenv"
  "Compilation mode for devenv commands with Nix trace matching.")

(defun devenv--compile (root args &optional global-args)
  "Run devenv ARGS in ROOT through `compilation-start'.
GLOBAL-ARGS are global CLI flags inserted before the subcommand.
Return the compilation buffer."
  (let* ((program (devenv--ensure-executable))
         (default-directory root)
         (compilation-environment devenv-extra-env)
         (argv (devenv--command args global-args program))
         (command (devenv--shell-command argv)))
    (devenv--log root argv 'compile nil)
    (compilation-start
     command #'devenv-compilation-mode
     (lambda (_mode)
       (format "*devenv %s: %s*"
               (car args)
               (file-name-nondirectory (directory-file-name root)))))))

;;;; Cache

(defvar devenv--cache (make-hash-table :test #'equal)
  "Cache of introspection results keyed by (ROOT . KEY).
Values are (TIMESTAMP . VALUE) conses; see `devenv-cache-ttl'.")

(defconst devenv-env--cache-ttl 3600
  "Seconds to keep a fetched environment or trust state.
Deliberately much longer than `devenv-cache-ttl': the shell env
only changes when the config does (and `devenv-reload'
invalidates it explicitly), while trust only changes via
`devenv-allow' and `devenv-revoke', which both invalidate their
cache entry.")

(defun devenv--cache-fresh-p (entry &optional now)
  "Return non-nil when cache ENTRY is younger than `devenv-cache-ttl'.
ENTRY is a (TIMESTAMP . VALUE) cons; NOW defaults to the current
time and exists for tests."
  (and entry
       (< (- (or now (float-time)) (car entry)) devenv-cache-ttl)))

(defun devenv--cached (root key fetch-fn)
  "Return the cached value for (ROOT . KEY), refreshing via FETCH-FN."
  (let* ((cache-key (cons root key))
         (entry (gethash cache-key devenv--cache)))
    (if (devenv--cache-fresh-p entry)
        (cdr entry)
      (let ((value (funcall fetch-fn)))
        (puthash cache-key (cons (float-time) value) devenv--cache)
        value))))

(defun devenv--cache-invalidate (&optional root)
  "Drop cached results for ROOT, or all roots when ROOT is nil."
  (if (null root)
      (clrhash devenv--cache)
    (let (stale)
      (maphash (lambda (key _value)
                 (when (equal (car key) root)
                   (push key stale)))
               devenv--cache)
      (dolist (key stale)
        (remhash key devenv--cache)))))

(defun devenv-version (root)
  "Return the devenv version string active in ROOT, or nil."
  (devenv--cached
   root 'version
   (lambda ()
     (condition-case nil
         (let ((out (devenv--call root "version")))
           (when (string-match "devenv \\([0-9][0-9.]*\\)" out)
             (match-string 1 out)))
       (error nil)))))

;;;; Auto-activation trust (devenv allow / revoke)

;; devenv 2.1+ gates auto-activation behind a trust database
;; (~/.local/share/devenv/allowed, managed by `devenv allow' and
;; `devenv revoke').  The shell hook consults it via the internal
;; `devenv hook-should-activate' subcommand: exit 0 with the project
;; path on stdout when trusted, exit 2 when a project exists but is
;; not trusted, exit 0 with no output when there is no project.  The
;; native environment loader below honours the same contract so Emacs
;; never evaluates an untrusted devenv.nix automatically.

(defun devenv--activation-state (exit output)
  "Map hook-should-activate EXIT code and OUTPUT to a trust state.
Returns `allowed', `blocked', `no-project', or `unsupported' (a
devenv without the trust model, e.g. 1.x — treated as permitted
to preserve that version's behaviour)."
  (cond
   ((and (eql exit 0) (not (string-blank-p (or output "")))) 'allowed)
   ((eql exit 2) 'blocked)
   ((eql exit 0) 'no-project)
   (t 'unsupported)))

(defun devenv--activation-permits-p (state)
  "Return non-nil when trust STATE permits automatic env loading."
  (memq state '(allowed unsupported)))

(defun devenv--trust-state (root)
  "Return ROOT's auto-activation trust state (cached).
See `devenv--activation-state' for the possible values."
  ;; Trust only changes via `devenv-allow'/`devenv-revoke', which both
  ;; invalidate this cache explicitly — cache it for the long TTL so
  ;; the synchronous `hook-should-activate' subprocess is not re-paid
  ;; every `devenv-cache-ttl' seconds on the find-file path.
  (let ((devenv-cache-ttl devenv-env--cache-ttl))
    (devenv--cached
     root 'trust
     (lambda ()
       (if (not (executable-find devenv-executable))
           'unsupported
         (let ((default-directory root)
               (process-environment (append devenv-extra-env
                                            process-environment)))
           (with-temp-buffer
             (let ((exit (ignore-errors
                           (apply #'call-process devenv-executable
                                  nil '(t nil) nil
                                  (append devenv-global-arguments
                                          '("hook-should-activate"))))))
               (devenv--activation-state exit (buffer-string))))))))))

;;;###autoload
(defun devenv-allow ()
  "Trust the current project for devenv auto-activation.
Runs `devenv allow' (recording the project in devenv's trust
database), then activates the native environment loader in the
project's buffers when `devenv-env-global-mode' is enabled."
  (interactive)
  (let ((root (devenv--root-or-error)))
    (devenv--call root "allow")
    (devenv--cache-invalidate root)
    (when devenv-env-global-mode
      (dolist (buffer (devenv--project-buffers root))
        (with-current-buffer buffer
          (unless devenv-env-mode
            (devenv-env--turn-on)))))
    (devenv-modeline--refresh-root root)
    (message "devenv: allowed %s" (abbreviate-file-name root))))

;;;###autoload
(defun devenv-revoke ()
  "Revoke devenv auto-activation trust for the current project.
Runs `devenv revoke' and drops the native environment from any
buffer `devenv-env-mode' was managing."
  (interactive)
  (let ((root (devenv--root-or-error)))
    (devenv--call root "revoke")
    (devenv--cache-invalidate root)
    (dolist (buffer (devenv-env--buffers root))
      (with-current-buffer buffer
        (devenv-env-mode -1)))
    (devenv-modeline--refresh-root root)
    (message "devenv: revoked %s" (abbreviate-file-name root))))

;;;; Introspection: eval, info, search

(defun devenv--eval-attr (root attr)
  "Evaluate config ATTR in ROOT via `devenv eval'; return parsed JSON.
When the output is a single-key object keyed by ATTR itself the
value is unwrapped, so callers always get the attribute's value."
  (let ((parsed (devenv--call-json root "eval" attr)))
    (if (and (consp parsed)
             (null (cdr parsed))
             (consp (car parsed))
             (equal (symbol-name (caar parsed)) attr))
        (cdar parsed)
      parsed)))

(defvar devenv-eval-history nil
  "Minibuffer history for `devenv-eval'.")

;;;###autoload
(defun devenv-eval (attr)
  "Evaluate devenv config attribute ATTR and show it as pretty JSON."
  (interactive
   (list (read-string "devenv eval attribute: " nil 'devenv-eval-history)))
  (let* ((root (devenv--root-or-error))
         (out (devenv--call root "eval" attr))
         (buffer (get-buffer-create (format "*devenv eval: %s*" attr))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert out)
        (ignore-errors (json-pretty-print-buffer))
        (if (fboundp 'js-json-mode) (js-json-mode) (fundamental-mode))
        (view-mode 1)))
    (display-buffer buffer)))

(defun devenv--show-output (buffer-name output)
  "Display OUTPUT (ANSI-colored) in a fresh buffer named BUFFER-NAME."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (ansi-color-apply output))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buffer)))

;;;###autoload
(defun devenv-info ()
  "Show `devenv info' for the current project."
  (interactive)
  (let ((root (devenv--root-or-error)))
    (devenv--show-output
     (format "*devenv info: %s*"
             (file-name-nondirectory (directory-file-name root)))
     (devenv--call root "info"))))

;;;###autoload
(defun devenv-search (query)
  "Search nixpkgs packages and devenv options for QUERY."
  (interactive "sdevenv search: ")
  (let ((root (devenv--root-or-error)))
    (devenv--show-output (format "*devenv search: %s*" query)
                         (devenv--call root "search" query))))

;;;; Tasks

(defun devenv--tasks-parse (parsed)
  "Normalize PARSED `devenv tasks list --json' output.
Return an alist of (NAME . DESCRIPTION) with NAME a string and
DESCRIPTION a string or nil.  Accepts either a list of task
objects with `name'/`description' fields or an object mapping
task names to their definitions."
  (cond
   ;; List of task objects: ({name: "a:b", description: …} …)
   ((and (listp parsed)
         (cl-every #'listp parsed)
         (cl-some (lambda (entry) (and (listp entry) (assq 'name entry)))
                  parsed))
    (mapcar (lambda (entry)
              (cons (format "%s" (alist-get 'name entry))
                    (alist-get 'description entry)))
            parsed))
   ;; Object keyed by task name: ((a:b . (…)) …)
   ((and (consp parsed) (consp (car parsed)) (symbolp (caar parsed)))
    (mapcar (lambda (entry)
              (cons (symbol-name (car entry))
                    (when (listp (cdr entry))
                      (alist-get 'description (cdr entry)))))
            parsed))
   (t nil)))

(defun devenv--tasks (root)
  "Return the (NAME . DESCRIPTION) task alist for ROOT (cached)."
  (devenv--cached
   root 'tasks
   (lambda ()
     (devenv--tasks-parse
      (devenv--call-json root "tasks" "list" "--json")))))

(defconst devenv-task-run-modes '("before" "single" "after" "all")
  "Valid values for the -m flag of `devenv tasks run'.")

;;;###autoload
(defun devenv-task-run (task &optional mode)
  "Run TASK via `devenv tasks run' in a compilation buffer.
Interactively, complete over the project's tasks; with a prefix
argument also prompt for the dependency MODE (devenv 2.1 default
is `before', which runs the task's dependencies first)."
  (interactive
   (let* ((root (devenv--root-or-error))
          (tasks (or (devenv--tasks root)
                     (user-error "No tasks defined in this devenv \
configuration")))
          (completion-extra-properties
           (list :annotation-function
                 (lambda (name)
                   (when-let* ((desc (cdr (assoc name tasks))))
                     (concat "  " desc))))))
     (list (completing-read "Run task: " tasks nil t)
           (when current-prefix-arg
             (completing-read "Dependency mode: " devenv-task-run-modes
                              nil t nil nil "before")))))
  (devenv--compile (devenv--root-or-error)
                   (append (list "tasks" "run" task)
                           (when mode (list "-m" mode)))))

;;;; Scripts

(defun devenv--scripts (root)
  "Return the (NAME . DESCRIPTION) script alist for ROOT (cached)."
  (devenv--cached
   root 'scripts
   (lambda ()
     (let ((parsed (ignore-errors (devenv--eval-attr root "scripts"))))
       (when (and (consp parsed) (consp (car parsed)))
         (mapcar (lambda (entry)
                   (cons (symbol-name (car entry))
                         (when (listp (cdr entry))
                           (let ((desc (alist-get 'description
                                                  (cdr entry))))
                             (unless (member desc '(nil "")) desc)))))
                 parsed))))))

;;;###autoload
(defun devenv-script-run (script)
  "Run SCRIPT (defined in devenv.nix) in a compilation buffer.
When the buffer's environment already carries DEVENV_PROFILE
\(direnv/envrc has loaded the shell), the script is on PATH and
runs directly; otherwise it goes through `devenv shell'."
  (interactive
   (let* ((root (devenv--root-or-error))
          (scripts (or (devenv--scripts root)
                       (user-error "No scripts defined in this devenv \
configuration")))
          (completion-extra-properties
           (list :annotation-function
                 (lambda (name)
                   (when-let* ((desc (cdr (assoc name scripts))))
                     (concat "  " desc))))))
     (list (completing-read "Run script: " scripts nil t))))
  (let ((root (devenv--root-or-error)))
    (if (getenv "DEVENV_PROFILE")
        (let ((default-directory root))
          (compilation-start (shell-quote-argument script)
                             #'devenv-compilation-mode))
      (devenv--compile root (list "shell" script)))))

;;;; Build, test, maintenance

;;;###autoload
(defun devenv-test (&optional args)
  "Run `devenv test' in a compilation buffer.
ARGS are extra global CLI flags (supplied by the transient)."
  (interactive (list (transient-args 'devenv)))
  (devenv--compile (devenv--root-or-error) '("test") args))

;;;###autoload
(defun devenv-build (&optional attrs args)
  "Run `devenv build' in a compilation buffer.
ATTRS is a space-separated string of attributes to build (empty
builds the whole shell).  ARGS are extra global CLI flags."
  (interactive (list (read-string "Attributes (empty for all): ")
                     (transient-args 'devenv)))
  (devenv--compile (devenv--root-or-error)
                   (cons "build" (split-string (or attrs "") nil t))
                   args))

;;;###autoload
(defun devenv-update ()
  "Run `devenv update' (refresh devenv.lock) in a compilation buffer."
  (interactive)
  (devenv--compile (devenv--root-or-error) '("update")))

;;;###autoload
(defun devenv-gc ()
  "Run `devenv gc' (delete unused environment generations)."
  (interactive)
  (devenv--compile (devenv--root-or-error) '("gc")))

;;;; Processes: up/down and log buffers

(define-derived-mode devenv-log-mode special-mode "devenv-log"
  "Major mode for live devenv process output buffers."
  (setq-local window-point-insertion-type t))

(defun devenv--insert-filter (proc string)
  "Insert STRING into PROC's buffer with ANSI colors applied."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (ansi-color-apply string)))))))

(defun devenv--up-buffer-name (root)
  "Return the attached `devenv up' buffer name for ROOT."
  (format "*devenv up: %s*"
          (file-name-nondirectory (directory-file-name root))))

;;;###autoload
(defun devenv-up (&optional select)
  "Start the project's processes with `devenv up'.
With prefix argument SELECT, choose a subset of the defined
processes.  Runs detached when `devenv-up-detached' is non-nil,
otherwise attached in a live `devenv-log-mode' buffer."
  (interactive "P")
  (let* ((root (devenv--root-or-error))
         (procs (when select
                  (let ((defined (devenv--processes-defined root)))
                    (unless defined
                      (user-error "No processes defined in devenv.nix"))
                    (completing-read-multiple "Processes: " defined nil t))))
         (args (append '("up") procs)))
    (if devenv-up-detached
        (devenv--run root (append args '("-d"))
                     (lambda (exit _output)
                       (if (zerop exit)
                           (message "devenv up: started (detached)")
                         (message "devenv up failed; see %s"
                                  devenv-log-buffer-name)))
                     "up")
      (let* ((default-directory root)
             (process-environment (append devenv-extra-env
                                          process-environment))
             (buffer (get-buffer-create (devenv--up-buffer-name root))))
        (when (get-buffer-process buffer)
          (user-error "devenv up already running in %s"
                      (buffer-name buffer)))
        (with-current-buffer buffer
          (let ((inhibit-read-only t)) (erase-buffer))
          (devenv-log-mode))
        (make-process
         :name "devenv-up" :buffer buffer
         :command (devenv--command args)
         :noquery t :connection-type 'pipe
         :filter #'devenv--insert-filter
         :sentinel (lambda (proc event)
                     (devenv--insert-filter
                      proc (format "\n[devenv up %s]"
                                   (string-trim event)))))
        (display-buffer buffer)))))

;;;###autoload
(defun devenv-down ()
  "Stop the project's running processes with `devenv down'."
  (interactive)
  (let ((root (devenv--root-or-error)))
    ;; Interrupt an attached `devenv up' first so it can shut down.
    (when-let* ((buffer (get-buffer (devenv--up-buffer-name root)))
                (proc (get-buffer-process buffer)))
      (interrupt-process proc))
    (devenv--run root '("down")
                 (lambda (exit _output)
                   (if (zerop exit)
                       (message "devenv down: processes stopped")
                     (message "devenv down failed; see %s"
                              devenv-log-buffer-name))
                   (devenv-processes--maybe-refresh root))
                 "down")))

(defun devenv--processes-defined (root)
  "Return the names of processes defined in ROOT's devenv.nix."
  (devenv--cached
   root 'processes-def
   (lambda ()
     (let ((parsed (ignore-errors (devenv--eval-attr root "processes"))))
       (when (and (consp parsed) (consp (car parsed)))
         (mapcar (lambda (entry) (symbol-name (car entry))) parsed))))))

;;;###autoload
(defun devenv-processes-logs (name)
  "Show recent log output of managed process NAME."
  (interactive
   (list (completing-read
          "Process logs: "
          (or (devenv--processes-defined (devenv--root-or-error)) '())
          nil nil)))
  (let* ((root (devenv--root-or-error))
         (out (devenv--call root "processes" "logs" name
                            "-n" (number-to-string
                                  devenv-processes-log-lines)))
         (buffer (get-buffer-create (format "*devenv logs: %s*" name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (ansi-color-apply out))
        (goto-char (point-max)))
      (devenv-log-mode))
    (display-buffer buffer)))

;;;; Process dashboard

(defvar-local devenv-processes--root nil
  "Project root shown by this `devenv-processes-mode' buffer.")

(defun devenv-processes--parse-json (parsed)
  "Normalize PARSED `devenv processes list --json' output.
Return a list of (NAME STATUS DETAIL) string triples.  Accepts
a list of process objects or an object keyed by process name."
  (cl-flet ((field (alist key)
              (let ((value (alist-get key alist)))
                (if value (format "%s" value) ""))))
    (cond
     ((and (listp parsed)
           (cl-every #'listp parsed)
           (cl-some (lambda (entry) (and (listp entry) (assq 'name entry)))
                    parsed))
      (mapcar (lambda (entry)
                (list (field entry 'name)
                      (field entry 'status)
                      (string-trim
                       (concat (field entry 'pid) " "
                               (field entry 'exit_code)))))
              parsed))
     ((and (consp parsed) (consp (car parsed)) (symbolp (caar parsed)))
      (mapcar (lambda (entry)
                (let ((props (if (listp (cdr entry)) (cdr entry) nil)))
                  (list (symbol-name (car entry))
                        (field props 'status)
                        (string-trim
                         (concat (field props 'pid) " "
                                 (field props 'exit_code))))))
              parsed))
     (t nil))))

(defun devenv-processes--parse-table (output)
  "Parse plain-text `devenv processes list' OUTPUT into triples.
Columns are split on runs of two or more spaces; header and rule
lines are skipped.  Return (NAME STATUS DETAIL) string triples."
  (let (rows)
    (dolist (line (split-string (ansi-color-filter-apply output) "\n" t))
      (let ((cols (split-string (string-trim line) "[ \t][ \t]+" t)))
        (when (and cols
                   (not (string-match-p "\\`\\(NAME\\'\\|[─═-]+\\'\\)"
                                        (car cols))))
          (push (list (nth 0 cols)
                      (or (nth 1 cols) "")
                      (mapconcat #'identity (cddr cols) " "))
                rows))))
    (nreverse rows)))

(defun devenv-processes--entries (triples)
  "Convert (NAME STATUS DETAIL) TRIPLES to `tabulated-list-entries'."
  (mapcar (lambda (row)
            (list (nth 0 row)
                  (vector (nth 0 row) (nth 1 row) (nth 2 row))))
          triples))

(defun devenv-processes--display (buffer triples)
  "Fill dashboard BUFFER with TRIPLES and redraw it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq tabulated-list-entries (devenv-processes--entries triples))
      (tabulated-list-print t)
      (unless triples
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (insert "No running processes — press u (devenv up) to \
start them.")))))))

(defun devenv-processes--refresh ()
  "Asynchronously refresh the current dashboard buffer."
  (let ((buffer (current-buffer))
        (root devenv-processes--root))
    (devenv--run
     root '("processes" "list" "--json")
     (lambda (exit output)
       (if (zerop exit)
           (devenv-processes--display
            buffer
            (devenv-processes--parse-json
             (ignore-errors (devenv--json-parse output))))
         ;; Older devenv without --json: fall back to the plain table.
         (devenv--run
          root '("processes" "list")
          (lambda (exit2 output2)
            (devenv-processes--display
             buffer (when (zerop exit2)
                      (devenv-processes--parse-table output2))))
          "processes-list")))
     "processes-list")))

(defun devenv-processes--maybe-refresh (root)
  "Refresh the dashboard buffer for ROOT when one exists."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'devenv-processes-mode)
                 (equal devenv-processes--root root))
        (devenv-processes--refresh)))))

(defun devenv-processes--name-at-point ()
  "Return the process name on the current dashboard line."
  (or (tabulated-list-get-id)
      (user-error "No process on this line")))

(defun devenv-processes--act (verb)
  "Run `devenv processes VERB' on the process at point, then refresh."
  (let ((name (devenv-processes--name-at-point))
        (buffer (current-buffer))
        (root devenv-processes--root))
    (message "devenv processes %s %s…" verb name)
    (devenv--run root (list "processes" verb name)
                 (lambda (exit _output)
                   (message "devenv processes %s %s: %s" verb name
                            (if (zerop exit) "ok" "failed"))
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (devenv-processes--refresh))))
                 verb)))

(defun devenv-processes-start ()
  "Start the process at point."
  (interactive nil devenv-processes-mode)
  (devenv-processes--act "start"))

(defun devenv-processes-stop ()
  "Stop the process at point."
  (interactive nil devenv-processes-mode)
  (devenv-processes--act "stop"))

(defun devenv-processes-restart ()
  "Restart the process at point."
  (interactive nil devenv-processes-mode)
  (devenv-processes--act "restart"))

(defun devenv-processes-show-logs ()
  "Show logs for the process at point."
  (interactive nil devenv-processes-mode)
  (let ((default-directory devenv-processes--root))
    (devenv-processes-logs (devenv-processes--name-at-point))))

(defun devenv-processes-revert (&optional _ignore-auto _noconfirm)
  "Refresh the dashboard; suitable for `revert-buffer-function'."
  (interactive nil devenv-processes-mode)
  (devenv-processes--refresh))

(defvar-keymap devenv-processes-mode-map
  :parent tabulated-list-mode-map
  :doc "Keymap for `devenv-processes-mode'."
  "s" #'devenv-processes-start
  "k" #'devenv-processes-stop
  "r" #'devenv-processes-restart
  "l" #'devenv-processes-show-logs
  "RET" #'devenv-processes-show-logs
  "u" #'devenv-up
  "d" #'devenv-down)

(define-derived-mode devenv-processes-mode tabulated-list-mode
  "devenv-processes"
  "Dashboard over the devenv process manager.
Observes processes managed by `devenv up'; keys start, stop,
restart, and tail individual processes."
  (setq tabulated-list-format [("Process" 28 t)
                               ("Status" 14 t)
                               ("Detail" 0 nil)])
  (setq tabulated-list-padding 1)
  (setq-local revert-buffer-function #'devenv-processes-revert)
  (tabulated-list-init-header))

;;;###autoload
(defun devenv-processes ()
  "Open the process dashboard for the current devenv project."
  (interactive)
  (let* ((root (devenv--root-or-error))
         (buffer (get-buffer-create
                  (format "*devenv processes: %s*"
                          (file-name-nondirectory
                           (directory-file-name root))))))
    (with-current-buffer buffer
      (devenv-processes-mode)
      (setq devenv-processes--root root)
      (devenv-processes--refresh))
    (pop-to-buffer buffer)))

;;;; Environment reload

(defun devenv--project-buffers (root)
  "Return live buffers whose `default-directory' sits under ROOT."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer
                  (and default-directory
                       (not (file-remote-p default-directory))
                       (file-in-directory-p default-directory root))))
              (buffer-list)))

(defun devenv--offer-eglot-reconnect (root)
  "Offer to reconnect eglot servers serving buffers under ROOT.
Eglot snapshots the environment when a server starts, so after
an environment change reconnecting is the only way a language
server picks up new PATH entries and variables."
  (when (featurep 'eglot)
    (let (servers)
      (dolist (buffer (devenv--project-buffers root))
        (with-current-buffer buffer
          (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
            (when-let* ((server (eglot-current-server)))
              (cl-pushnew server servers)))))
      (when (and servers
                 (y-or-n-p (format "Reconnect %d eglot server%s with \
the new environment? "
                                   (length servers)
                                   (if (cdr servers) "s" ""))))
        (dolist (server servers)
          (eglot-reconnect server))))))

;;;###autoload
(defun devenv-reload ()
  "Reload the devenv environment for the current project.
Invalidates cached introspection, then refreshes buffer-local
environments: through envrc when it manages this project (the
recommended direnv substrate), or through `devenv-env-mode'
buffers when the native loader is active.  Finally offers to
reconnect any eglot servers so they see the new environment."
  (interactive)
  (let ((root (devenv--root-or-error)))
    (devenv--cache-invalidate root)
    (cond
     ;; `envrc-reload-all' can be fboundp via its autoload before the
     ;; package is loaded, so also require the mode variable to exist.
     ((and (fboundp 'envrc-reload-all)
           (boundp 'envrc-mode)
           (seq-some (lambda (buffer)
                       (buffer-local-value 'envrc-mode buffer))
                     (devenv--project-buffers root)))
      (message "devenv: refreshing environment via envrc…")
      (envrc-reload-all))
     ((devenv-env--buffers root)
      (message "devenv: refreshing native environment…")
      (devenv-env--refresh root))
     (t
      (message "devenv: introspection cache cleared")))
    (devenv-modeline--refresh-root root)
    (devenv--offer-eglot-reconnect root)))

;;;; Native environment loader

(defvar devenv-env--pending (make-hash-table :test #'equal)
  "Roots with an environment fetch in flight, mapped to buffers.")

(defvar devenv-env--eglot-replay nil
  "Buffers whose deferred `eglot-ensure' replays after env load.")

(defmacro devenv-env--with-global-env (&rest body)
  "Run BODY with Emacs's global environment, ignoring buffer-local values.
The loader must probe and source from a pristine environment: a reload
triggered from a buffer that already has the project env applied would
otherwise stack the project's PATH on top of itself and mis-diff the
result."
  (declare (indent 0) (debug t))
  `(let ((process-environment (default-value 'process-environment))
         (exec-path (default-value 'exec-path)))
     ,@body))

(defun devenv-env--ignored-variables ()
  "Return the variable names the loader never applies to a buffer."
  (append devenv-env-never-applied-variables devenv-env-ignored-variables))

(defun devenv-env--parse (json-string &optional ignored)
  "Parse `devenv print-dev-env --json' JSON-STRING output.
Return a list of \"NAME=VALUE\" strings for every exported
variable, skipping names in IGNORED, which overrides the default
of `devenv-env--ignored-variables'."
  (let* ((ignored (or ignored (devenv-env--ignored-variables)))
         (parsed (devenv--json-parse json-string))
         (variables (alist-get 'variables parsed))
         pairs)
    (dolist (entry variables)
      (let* ((name (symbol-name (car entry)))
             (props (cdr entry))
             (type (alist-get 'type props))
             (value (alist-get 'value props)))
        (when (and (equal type "exported")
                   (stringp value)
                   (not (member name ignored)))
          (push (concat name "=" value) pairs))))
    (nreverse pairs)))

(defun devenv-env--split-pair (pair)
  "Split PAIR into a (NAME . VALUE) cons, or nil when it has no \"=\".
A pair without \"=\" is a bare name, which Emacs reads as \"unset\"."
  (when-let* ((idx (string-search "=" pair)))
    (cons (substring pair 0 idx) (substring pair (1+ idx)))))

(defun devenv-env--pair-value (pairs name)
  "Return the value of NAME in the \"NAME=VALUE\" list PAIRS, or nil."
  (let ((prefix (concat name "=")))
    (when-let* ((pair (seq-find (lambda (p) (string-prefix-p prefix p))
                                pairs)))
      (substring pair (length prefix)))))

(defun devenv-env--layer-value (name value)
  "Return NAME's VALUE layered over the global environment's value.
VALUE's entries come first, then the ones Emacs inherited, with empty
and duplicate components dropped and the order otherwise preserved.
The global side is read from the default `process-environment', never
the buffer-local one, so re-applying an environment is idempotent."
  (let ((global (getenv-internal name (default-value 'process-environment))))
    (mapconcat #'identity
               (delete-dups
                (append (split-string (or value "") path-separator t)
                        (split-string (or global "") path-separator t)))
               path-separator)))

(defun devenv-env--layer-pairs (pairs)
  "Return PAIRS with `devenv-env-layered-variables' layered, others as-is."
  (mapcar (lambda (pair)
            (let ((cell (devenv-env--split-pair pair)))
              (if (and cell (member (car cell) devenv-env-layered-variables))
                  (concat (car cell) "="
                          (devenv-env--layer-value (car cell) (cdr cell)))
                pair)))
          pairs))

(defun devenv-env--path-from-pairs (pairs)
  "Return the `exec-path' list encoded in PAIRS' PATH entry, or nil.
`exec-directory' stays at the tail, envrc-style, so Emacs's own helper
programs remain reachable.  PAIRS is expected to be layered already
\(see `devenv-env--layer-pairs'), which is what keeps `exec-path' and
the PATH subprocesses see in step."
  (when-let* ((value (devenv-env--pair-value pairs "PATH")))
    (delete-dups
     (append (delq nil (parse-colon-path value)) (list exec-directory)))))

(defun devenv-env--apply (buffer pairs)
  "Apply environment PAIRS buffer-locally to BUFFER, envrc-style.
Layering happens here rather than in the cache, so `exec-path' and the
PATH subprocesses see are always derived from the same string and a
change to Emacs's own PATH is picked up on the next application."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((pairs (devenv-env--layer-pairs pairs)))
        (setq-local process-environment
                    (append pairs (default-value 'process-environment)))
        (when-let* ((path (devenv-env--path-from-pairs pairs)))
          (setq-local exec-path path)))
      (devenv-modeline--update buffer 'no-probe))))

(defun devenv-env--cached-pairs (root)
  "Return cached environment pairs for ROOT, or nil."
  (let ((entry (gethash (cons root 'env) devenv--cache))
        (devenv-cache-ttl devenv-env--cache-ttl))
    (when (devenv--cache-fresh-p entry)
      (cdr entry))))

(defun devenv-env--release-eglot (buffers &optional replay)
  "Unpark BUFFERS from `devenv-env--eglot-replay'; return how many were left.
With REPLAY non-nil each one's deferred `eglot-ensure' runs, guarded so
a single failure cannot strand the rest.  Buffers parked for another,
still-pending root stay parked.  The return value counts the buffers
released without connecting, i.e. the ones now missing their server."
  (let ((parked devenv-env--eglot-replay)
        (released 0))
    (setq devenv-env--eglot-replay nil)
    (dolist (buffer parked)
      (cond
       ((not (buffer-live-p buffer)))
       ((not (memq buffer buffers))
        (push buffer devenv-env--eglot-replay))
       (replay
        (with-demoted-errors "devenv: deferred eglot-ensure failed: %S"
          (with-current-buffer buffer
            (when (fboundp 'eglot-ensure) (eglot-ensure)))))
       (t (cl-incf released))))
    released))

(defun devenv-env--handle-pairs (root pairs)
  "Apply PAIRS to ROOT's pending buffers and release deferred eglot calls.
PAIRS nil means the fetch failed.  ROOT is dropped from
`devenv-env--pending' and its buffers are released from
`devenv-env--eglot-replay' on every path, failures included: leaving
either in place would keep `devenv-env-loading-p' non-nil forever and
so disable eglot auto-start for the whole project.  A failed load
deliberately does not connect eglot with the global environment, which
would be the wrong toolchain for a project whose server only exists
inside devenv."
  (let ((buffers (gethash root devenv-env--pending)))
    (remhash root devenv-env--pending)
    (unwind-protect
        (when pairs
          (puthash (cons root 'env) (cons (float-time) pairs) devenv--cache)
          (dolist (buffer buffers)
            (devenv-env--apply buffer pairs))
          (message "devenv: environment loaded for %s"
                   (abbreviate-file-name root)))
      (let ((released (devenv-env--release-eglot buffers (and pairs t))))
        (unless pairs
          (message "devenv: loading environment for %s failed%s; see %s"
                   (abbreviate-file-name root)
                   (if (> released 0)
                       (format " (%d buffer%s without LSP, M-x eglot to retry)"
                               released (if (= released 1) "" "s"))
                     "")
                   devenv-log-buffer-name))))))

(defconst devenv-env--shell-hook-eval "eval \"${shellHook:-}\""
  "The line devenv appends to its dev-env script to run the shellHook.")

(defconst devenv-env--source-script "{ . \"$1\"; } >&2; env -0"
  "Bash snippet that sources the dev-env script and dumps the result.
The hook's own output goes to stderr, where it lands in
`devenv-log-buffer-name', so stdout stays a clean `env -0' dump.")

(defun devenv-env--script-with-hook (script)
  "Return SCRIPT, made to evaluate its shellHook exactly once.
devenv's own dev-env script ends with the eval; a plain nix projection
does not, and would leave every hook-set variable missing."
  (if (string-search devenv-env--shell-hook-eval script)
      script
    (concat script "\n" devenv-env--shell-hook-eval "\n")))

(defun devenv-env--parse-dump (dump)
  "Parse NUL-separated `env -0' DUMP into an alist of (NAME . VALUE)."
  (delq nil (mapcar #'devenv-env--split-pair (split-string dump "\0" t))))

(defun devenv-env--shell-ignored-p (name)
  "Return non-nil when NAME must not be taken from a sourced shell."
  (or (member name (devenv-env--ignored-variables))
      (seq-some (lambda (regexp) (string-match-p regexp name))
                devenv-env-shell-ignored-variables)))

(defun devenv-env--dump-delta (vars)
  "Return VARS as environment pairs relative to Emacs's global environment.
VARS is a (NAME . VALUE) alist from a shell that inherited that
environment, so only the difference has to be applied: unchanged
variables are dropped, new or changed ones become \"NAME=VALUE\", and
variables the shell removed become a bare \"NAME\", which Emacs reads
as unset.  Names matching `devenv-env-shell-ignored-variables' or the
never-applied set are ignored in both directions.  The baseline
includes `devenv-extra-env', which the loader passes to the shell but
must not hand on to the project's own subprocesses."
  (let* ((base (append devenv-extra-env
                       (default-value 'process-environment)))
         (vars (seq-remove (lambda (cell)
                             (devenv-env--shell-ignored-p (car cell)))
                           vars))
         pairs)
    (dolist (cell vars)
      (unless (equal (cdr cell) (getenv-internal (car cell) base))
        (push (concat (car cell) "=" (cdr cell)) pairs)))
    (dolist (entry base)
      (when-let* ((cell (devenv-env--split-pair entry))
                  (name (car cell))
                  ((not (devenv-env--shell-ignored-p name)))
                  ((not (assoc name vars))))
        (push name pairs)))
    (delete-dups (nreverse pairs))))

(defun devenv-env--source-shell (root script callback)
  "Source SCRIPT in bash under ROOT and pass its env dump to CALLBACK.
CALLBACK receives (EXIT-CODE DUMP); a non-zero exit means the dump is
unusable.  The child gets no stdin and is killed after
`devenv-command-timeout' seconds, so a hook that waits for input
cannot park the loader forever."
  (let* ((file (make-temp-file "devenv-env" nil ".sh"
                               (devenv-env--script-with-hook script)))
         (default-directory root)
         (process-environment (append devenv-extra-env
                                      (default-value 'process-environment)))
         (bash (or (let ((exec-path (default-value 'exec-path)))
                     (executable-find "bash"))
                   "bash"))
         (argv (list bash "--noprofile" "--norc" "-c"
                     devenv-env--source-script "devenv-env" file))
         (stdout (generate-new-buffer " *devenv-env-dump*"))
         (stderr (generate-new-buffer " *devenv-env-hook*"))
         (finish (lambda (exit dump hook)
                   (devenv--log root argv exit hook)
                   (ignore-errors (delete-file file))
                   (when (buffer-live-p stdout) (kill-buffer stdout))
                   (when (buffer-live-p stderr) (kill-buffer stderr))
                   (funcall callback exit dump)))
         timer)
    (condition-case err
        (let ((proc (make-process
                     :name "devenv-env-source"
                     :buffer stdout :stderr stderr
                     :command argv :noquery t :connection-type 'pipe
                     :sentinel
                     (lambda (proc _event)
                       (when (memq (process-status proc) '(exit signal))
                         (when timer (cancel-timer timer))
                         ;; Let the stderr pipe drain before reading it.
                         (accept-process-output nil 0.05)
                         (funcall finish
                                  (process-exit-status proc)
                                  (with-current-buffer stdout (buffer-string))
                                  (with-current-buffer stderr
                                    (buffer-string))))))))
          ;; Neither the process nor its stderr pipe may prompt on exit.
          (set-process-query-on-exit-flag proc nil)
          (when-let* ((err-proc (get-buffer-process stderr)))
            (set-process-query-on-exit-flag err-proc nil))
          (process-send-eof proc)
          (setq timer (run-at-time devenv-command-timeout nil
                                   (lambda ()
                                     (when (process-live-p proc)
                                       (delete-process proc)))))
          proc)
      (error
       (funcall finish 127 "" (error-message-string err))))))

(defun devenv-env--shell-loader-p (root)
  "Return non-nil when ROOT's environment may be loaded by sourcing bash.
Sourcing evaluates the project's shellHook, so it is limited to
projects trusted for auto-activation (`devenv-allow')."
  (and (eq devenv-env-loader 'shell)
       (let ((exec-path (default-value 'exec-path)))
         (and (executable-find "bash") t))
       (devenv--activation-permits-p (devenv--trust-state root))))

(defun devenv-env--fetch-json (root &optional reason)
  "Fetch ROOT's environment from `print-dev-env --json' and apply it.
REASON, when non-nil, is reported as why sourcing was not used."
  (when reason
    (message "devenv: %s; using print-dev-env --json" reason))
  (devenv-env--with-global-env
    (devenv--run
     root '("print-dev-env" "--json")
     (lambda (exit output)
       (devenv-env--handle-pairs
        root (and (zerop exit)
                  (condition-case nil
                      (devenv-env--parse output)
                    (error nil)))))
     "print-dev-env")))

(defun devenv-env--fetch-shell (root)
  "Fetch ROOT's environment by sourcing `devenv print-dev-env' in bash.
Either step failing falls back to `devenv-env--fetch-json', so a
project is never left without an environment."
  (devenv-env--with-global-env
    (devenv--run
     root '("print-dev-env")
     (lambda (exit script)
       (if (or (not (zerop exit)) (string-blank-p script))
           ;; Exit 127 means the CLI never ran, so the JSON form would
           ;; fail identically: report once instead of retrying.
           (if (eql exit 127)
               (devenv-env--handle-pairs root nil)
             (devenv-env--fetch-json root "print-dev-env failed"))
         ;; Anything unexpected here (no temp dir, no bash) must still
         ;; end in a fetch, or the root stays pending forever.
         (condition-case err
             (devenv-env--source-shell
              root script
              (lambda (exit dump)
                (let* ((vars (and (zerop exit) (devenv-env--parse-dump dump)))
                       (pairs (and vars (devenv-env--dump-delta vars))))
                  (if pairs
                      (devenv-env--handle-pairs root pairs)
                    (devenv-env--fetch-json
                     root "sourcing the dev-env script failed")))))
           (error (devenv-env--fetch-json
                   root (format "sourcing the dev-env script failed (%s)"
                                (error-message-string err)))))))
     "print-dev-env")))

(defun devenv-env--fetch (root)
  "Fetch ROOT's environment asynchronously and apply it when done.
All buffers registered under ROOT in `devenv-env--pending' get the
result; deferred `eglot-ensure' calls are replayed.  A trusted project
is loaded by sourcing the printed dev-env script so its shellHook runs
\(see `devenv-env-loader'); otherwise the JSON form is parsed."
  (if (devenv-env--with-global-env (devenv-env--shell-loader-p root))
      (devenv-env--fetch-shell root)
    (devenv-env--fetch-json root)))

(defun devenv-env--buffers (root)
  "Return live buffers under ROOT that have `devenv-env-mode' on."
  (seq-filter (lambda (buffer)
                (buffer-local-value 'devenv-env-mode buffer))
              (devenv--project-buffers root)))

(defun devenv-env--refresh (root)
  "Re-fetch ROOT's environment and re-apply it to its buffers."
  (remhash (cons root 'env) devenv--cache)
  (dolist (buffer (devenv-env--buffers root))
    (push buffer (gethash root devenv-env--pending)))
  (devenv-env--fetch root))

;;;###autoload
(define-minor-mode devenv-env-mode
  "Apply the project's devenv environment buffer-locally.
Fetches the environment asynchronously (cached per project, see
`devenv-env-loader') and sets `process-environment' and `exec-path'
locally in this buffer, in the style of envrc.  The result mirrors a
terminal `devenv shell': PATH is layered over the login one and the
derivation-only variables the JSON form carries are dropped.  Where
direnv is in charge, envrc + `use devenv' does the same job; see
`devenv-env-defer-to-direnv'."
  :lighter devenv-env-lighter
  (if devenv-env-mode
      (let ((root (devenv-project-root)))
        (if (null root)
            (setq devenv-env-mode nil)
          (let ((pairs (devenv-env--cached-pairs root)))
            (if pairs
                (devenv-env--apply (current-buffer) pairs)
              (let ((pending (gethash root devenv-env--pending)))
                (push (current-buffer)
                      (gethash root devenv-env--pending))
                (unless pending
                  (devenv-env--fetch root)))))))
    (kill-local-variable 'process-environment)
    (kill-local-variable 'exec-path)))

(defun devenv-env--turn-on ()
  "Enable `devenv-env-mode' when this buffer should manage its env.
The buffer must sit inside a devenv project with the devenv binary
available, and the project must be trusted for auto-activation
\(`devenv allow'; see `devenv-allow').  When `devenv-env-defer-to-direnv'
is non-nil the buffer must also not be under direnv (no .envrc above, no
active `envrc-mode'), so direnv keeps ownership where it is configured.
Never activates in the minibuffer or remote buffers."
  (let ((root (and (not (minibufferp))
                   (not (file-remote-p default-directory))
                   (executable-find devenv-executable)
                   (devenv-project-root))))
    (when (and root
               (or (not devenv-env-defer-to-direnv)
                   (and (not (locate-dominating-file default-directory ".envrc"))
                        (not (bound-and-true-p envrc-mode))))
               (devenv--activation-permits-p (devenv--trust-state root)))
      (devenv-env-mode 1))))

(defun devenv-env-loading-p (&optional buffer)
  "Non-nil when BUFFER's devenv environment fetch is still in flight.
BUFFER defaults to the current buffer.  Used by callers (e.g. eglot
auto-start) to hold off decisions that depend on the buffer-local
`exec-path' until the async `devenv print-dev-env' has landed."
  (with-current-buffer (or buffer (current-buffer))
    (and (bound-and-true-p devenv-env-mode)
         (when-let* ((root (devenv-project-root)))
           (and (gethash root devenv-env--pending) t)))))

(defun devenv-env--around-eglot-ensure (orig-fun &rest args)
  "Defer ORIG-FUN (`eglot-ensure', ARGS) while the env is loading.
Eglot snapshots the environment at connect time; connecting
before the async fetch lands would give the server the global
environment.  Deferred buffers replay once the env is applied."
  (let ((root (and devenv-env-mode (devenv-project-root))))
    (if (and root (gethash root devenv-env--pending))
        (cl-pushnew (current-buffer) devenv-env--eglot-replay)
      (apply orig-fun args))))

;;;###autoload
(define-globalized-minor-mode devenv-env-global-mode devenv-env-mode
  devenv-env--turn-on
  :group 'devenv
  (if devenv-env-global-mode
      (advice-add 'eglot-ensure :around
                  #'devenv-env--around-eglot-ensure)
    (advice-remove 'eglot-ensure #'devenv-env--around-eglot-ensure)))

;;;; Modeline status

(defface devenv-modeline-active-face
  '((t :inherit success))
  "Face for the modeline segment when the devenv env is active.")

(defface devenv-modeline-inactive-face
  '((t :inherit shadow))
  "Face for the modeline segment when the devenv env is not loaded.")

(defface devenv-modeline-blocked-face
  '((t :inherit warning))
  "Face for the modeline segment when auto-activation is blocked.")

(defvar-local devenv-modeline--state nil
  "Cached devenv modeline state for this buffer.
Nil when not yet computed; otherwise `none' (not a devenv
project), `active', `inactive', or `blocked'.")

(defvar devenv-modeline--map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'devenv)
    map)
  "Keymap for the devenv modeline segment; mouse-1 opens the menu.")

(defun devenv-modeline--compute-state (root active trust-state)
  "Return the modeline state for a buffer.
ROOT is the devenv project root or nil, ACTIVE non-nil when the
buffer's environment already carries the devenv shell, and
TRUST-STATE the cached `devenv--activation-state' value (or
nil when unknown)."
  (cond ((null root) 'none)
        (active 'active)
        ((eq trust-state 'blocked) 'blocked)
        (t 'inactive)))

(defun devenv-modeline--format (state)
  "Return the mode-line string for modeline STATE."
  (pcase state
    ('active
     (propertize " devenv[on]"
                 'face 'devenv-modeline-active-face
                 'help-echo "devenv environment active\nmouse-1: devenv menu"
                 'mouse-face 'mode-line-highlight
                 'local-map devenv-modeline--map))
    ('inactive
     (propertize " devenv[off]"
                 'face 'devenv-modeline-inactive-face
                 'help-echo "devenv project, environment not loaded\n\
mouse-1: devenv menu"
                 'mouse-face 'mode-line-highlight
                 'local-map devenv-modeline--map))
    ('blocked
     (propertize " devenv[!]"
                 'face 'devenv-modeline-blocked-face
                 'help-echo "devenv project not trusted — M-x devenv-allow\n\
mouse-1: devenv menu"
                 'mouse-face 'mode-line-highlight
                 'local-map devenv-modeline--map))
    (_ "")))

(defun devenv-modeline--cached-trust (root)
  "Return ROOT's trust state from the cache, or nil when unknown.
Never runs devenv: modeline code must stay subprocess-free on
the synchronous path."
  (cdr (gethash (cons root 'trust) devenv--cache)))

(defvar devenv-modeline--probing (make-hash-table :test #'equal)
  "Roots whose trust state is being resolved in the background.")

(defun devenv-modeline--probe-trust (root)
  "Resolve ROOT's trust state asynchronously, then refresh buffers."
  (unless (gethash root devenv-modeline--probing)
    (puthash root t devenv-modeline--probing)
    (devenv--run root '("hook-should-activate")
                 (lambda (exit output)
                   (remhash root devenv-modeline--probing)
                   (puthash (cons root 'trust)
                            (cons (float-time)
                                  (devenv--activation-state exit output))
                            devenv--cache)
                   (devenv-modeline--refresh-root root))
                 "trust-probe")))

(defun devenv-modeline--update (&optional buffer no-probe)
  "Recompute the devenv modeline state for BUFFER.
Unless NO-PROBE, kick an asynchronous trust probe when the state
is unknown so blocked projects eventually show devenv[!]."
  (with-current-buffer (or buffer (current-buffer))
    (let ((root (devenv-project-root)))
      (setq devenv-modeline--state
            (devenv-modeline--compute-state
             root
             (and root (getenv "DEVENV_PROFILE"))
             (and root (devenv-modeline--cached-trust root))))
      (when (and root (not no-probe)
                 (eq devenv-modeline--state 'inactive)
                 (null (devenv-modeline--cached-trust root))
                 (executable-find devenv-executable))
        (devenv-modeline--probe-trust root))
      devenv-modeline--state)))

(defun devenv-modeline--refresh-root (root)
  "Recompute the modeline state of every buffer under ROOT."
  (when (bound-and-true-p devenv-modeline-mode)
    (dolist (buffer (devenv--project-buffers root))
      (devenv-modeline--update buffer 'no-probe))
    (force-mode-line-update t)))

(defun devenv-modeline--segment ()
  "Return the devenv status string for the current buffer.
Computes the state lazily on first render; the redisplay path
never runs subprocesses (probes happen from `find-file-hook')."
  (unless devenv-modeline--state
    (devenv-modeline--update nil 'no-probe))
  (devenv-modeline--format devenv-modeline--state))

(defun devenv-modeline--on-find-file ()
  "Compute this buffer's modeline state, probing trust if needed."
  (devenv-modeline--update))

(defconst devenv-modeline--construct
  '(devenv-modeline-mode (:eval (devenv-modeline--segment)))
  "Mode-line construct injected into `mode-line-misc-info'.")

;;;###autoload
(define-minor-mode devenv-modeline-mode
  "Show the devenv environment status in the mode line.
Displays devenv[on] when the buffer's environment carries the
devenv shell (loaded by envrc/direnv or `devenv-env-mode'),
devenv[off] in a devenv project whose environment is not loaded,
and devenv[!] when auto-activation trust has been denied (run
\\[devenv-allow] to trust the project).  Outside devenv projects
the segment is empty."
  :global t
  (if devenv-modeline-mode
      (progn
        (add-to-list 'mode-line-misc-info devenv-modeline--construct t)
        (add-hook 'find-file-hook #'devenv-modeline--on-find-file)
        (add-hook 'dired-mode-hook #'devenv-modeline--on-find-file)
        ;; Invalidate so already-open buffers recompute lazily.
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (setq devenv-modeline--state nil))))
    (setq mode-line-misc-info
          (delete devenv-modeline--construct mode-line-misc-info))
    (remove-hook 'find-file-hook #'devenv-modeline--on-find-file)
    (remove-hook 'dired-mode-hook #'devenv-modeline--on-find-file))
  (force-mode-line-update t))

;;;; Eglot: serve devenv.nix with `devenv lsp'

(defvar devenv--eglot-fallback-contact '("nil")
  "Contact used for Nix buffers that are not devenv.nix files.
Captured from `eglot-server-programs' by `devenv-eglot-setup' so
ordinary Nix buffers keep whatever server they had before.  Only
reached when neither the project nor this editor ships a Nix LSP.")

(defcustom devenv-nix-lsp-servers '("nixd" "nil")
  "Nix language servers to try for an ordinary Nix buffer, best first.
Resolved to an absolute path at eglot connect time by
`devenv--nix-lsp-program', preferring the project's environment
before the server shipped with this editor."
  :type '(repeat string)
  :group 'devenv)

(defun devenv--nix-lsp-program ()
  "Return the absolute path of a Nix LSP to use here, or nil.
Search the buffer-local `exec-path' first, so a server provided by
the project's devenv wins; then fall back to Emacs's global
`exec-path', i.e. the server shipped with this editor.  Returning an
absolute path lets the `eglot-ensure' auto-start gate in
`init-prog.el' find it even though the shipped server is not on the
buffer-local devenv PATH."
  (or (seq-some #'executable-find devenv-nix-lsp-servers)
      (let ((exec-path (default-value 'exec-path)))
        (seq-some #'executable-find devenv-nix-lsp-servers))))

(defun devenv--devenv-nix-file-p (file)
  "Return non-nil when FILE is a devenv config inside a devenv project.
Matches devenv.nix and devenv.local.nix by basename, then checks
that a devenv project root covers the file."
  (and (stringp file)
       (string-match-p "\\`devenv\\(\\.local\\)?\\.nix\\'"
                       (file-name-nondirectory file))
       (devenv-project-root (file-name-directory (expand-file-name file)))
       t))

(defun devenv--eglot-contact (&optional interactive _project)
  "Return the eglot contact for the current Nix buffer.
Buffers visiting devenv.nix / devenv.local.nix get the bundled
`devenv lsp' server (a nixd preconfigured with the project's devenv
options).  Every other Nix buffer prefers a Nix LSP from the project's
environment and otherwise uses the one shipped with this editor (see
`devenv--nix-lsp-program' and `devenv-nix-lsp-servers').  Only when
neither exists does it delegate to the previously registered contact;
INTERACTIVE is passed through when that fallback is itself a contact
function, as produced by `eglot-alternatives'."
  (cond
   ;; devenv.nix / devenv.local.nix: the project's own `devenv lsp' (a
   ;; nixd preloaded with the project's devenv options).
   ((and (devenv--devenv-nix-file-p (buffer-file-name))
         (executable-find devenv-executable))
    (list devenv-executable "lsp"))
   ;; Any other Nix buffer: the project's Nix LSP if its env provides one,
   ;; else the one shipped with this editor.
   ((when-let* ((prog (devenv--nix-lsp-program))) (list prog)))
   ;; Neither: defer to whatever eglot had registered (may report that no
   ;; server is available).
   (t
    (let ((fallback devenv--eglot-fallback-contact))
      (if (functionp fallback)
          (funcall fallback interactive)
        fallback)))))

(defun devenv--eglot-entry-covers-nix-p (key)
  "Return non-nil when `eglot-server-programs' KEY covers Nix modes."
  (cl-flet ((mode-sym (item) (if (consp item) (car item) item)))
    (cond
     ((symbolp key) (memq key '(nix-mode nix-ts-mode)))
     ((listp key) (cl-some (lambda (item)
                             (memq (mode-sym item)
                                   '(nix-mode nix-ts-mode)))
                           key))
     (t nil))))

;;;###autoload
(defun devenv-eglot-setup ()
  "Route devenv.nix buffers to `devenv lsp' in `eglot-server-programs'.
Idempotent: captures the currently registered Nix contact as the
fallback for ordinary Nix buffers, then installs a routing entry
at the front of the list.  Call after eglot is loaded."
  (interactive)
  (require 'eglot)
  (let ((existing (cl-find-if
                   (lambda (entry)
                     (and (devenv--eglot-entry-covers-nix-p (car entry))
                          (not (eq (cdr entry) #'devenv--eglot-contact))))
                   eglot-server-programs)))
    (when existing
      (setq devenv--eglot-fallback-contact (cdr existing))))
  (unless (cl-rassoc #'devenv--eglot-contact eglot-server-programs)
    (push (cons '(nix-mode nix-ts-mode) #'devenv--eglot-contact)
          eglot-server-programs))
  eglot-server-programs)

;;;; MCP: register `devenv mcp' with mcp.el

(defun devenv--mcp-server-name (root)
  "Return the mcp.el server name used for ROOT."
  (format "devenv-%s"
          (file-name-nondirectory (directory-file-name root))))

(defun devenv--mcp-command (root)
  "Return the sh -c command string that starts `devenv mcp' in ROOT."
  (format "cd %s && exec %s mcp"
          (shell-quote-argument root)
          (shell-quote-argument devenv-executable)))

;;;###autoload
(defun devenv-mcp-setup ()
  "Register the current project's `devenv mcp' server with mcp.el.
Adds a stdio entry to `mcp-hub-servers' named after the project
\(e.g. \"devenv-myproject\") that starts `devenv mcp' in the
project root.  Its tools — nixpkgs package search, devenv option
search, and process control — then become available to MCP
clients: start the server from the `mcp-hub' dashboard, then
`gptel-mcp-connect' exposes the tools to gptel."
  (interactive)
  (unless (require 'mcp nil t)
    (user-error "Package mcp.el is not installed"))
  (let* ((root (devenv--root-or-error))
         (name (devenv--mcp-server-name root))
         (spec (list :command "sh"
                     :args (list "-c" (devenv--mcp-command root)))))
    (setf (alist-get name mcp-hub-servers nil nil #'equal) spec)
    (message "devenv: registered MCP server %S — start it from M-x \
mcp-hub, then M-x gptel-mcp-connect exposes its tools to gptel"
             name)))

;;;###autoload
(defun devenv-mcp-remove ()
  "Remove the current project's `devenv mcp' entry from mcp.el."
  (interactive)
  (unless (boundp 'mcp-hub-servers)
    (user-error "Package mcp.el is not loaded"))
  (let ((name (devenv--mcp-server-name (devenv--root-or-error))))
    (setf (alist-get name mcp-hub-servers nil 'remove #'equal) nil)
    (message "devenv: removed MCP server %S" name)))

;;;; Transient entry point

;;;###autoload (autoload 'devenv "devenv" nil t)
(transient-define-prefix devenv ()
  "Devenv commands for the current project."
  ["Options"
   ("-c" "Clean eval (ignore external env)" "--clean")
   ("-r" "Refresh eval cache" "--refresh-eval-cache")]
  ["Run"
   [("t" "Task…" devenv-task-run)
    ("s" "Script…" devenv-script-run)]
   [("T" "Test (enterTest)" devenv-test)
    ("b" "Build…" devenv-build)]]
  ["Processes"
   [("u" "Up (start all)" devenv-up)
    ("d" "Down (stop all)" devenv-down)]
   [("p" "Dashboard" devenv-processes)
    ("l" "Logs…" devenv-processes-logs)]]
  ["Introspect"
   [("i" "Info" devenv-info)
    ("/" "Search packages/options…" devenv-search)]
   [(":" "Eval attribute…" devenv-eval)
    ("$" "Show invocation log" devenv-show-log)]]
  ["Environment"
   [("r" "Reload environment" devenv-reload)
    ("e" "Native env loader (global)" devenv-env-global-mode)
    ("m" "Modeline status (global)" devenv-modeline-mode)]
   [("a" "Allow auto-activation" devenv-allow)
    ("x" "Revoke auto-activation" devenv-revoke)
    ("M" "Register MCP server" devenv-mcp-setup)]]
  ["Maintenance"
   [("U" "Update inputs (devenv.lock)" devenv-update)
    ("G" "Garbage-collect generations" devenv-gc)]]
  (interactive)
  (devenv--root-or-error)
  (transient-setup 'devenv))

(provide 'devenv)
;;; devenv.el ends here
