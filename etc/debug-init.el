;;; debug-init.el --- Full-debug launch harness for Jotain  -*- lexical-binding: t; -*-

;; Loaded via `emacs --load' by the `just run-built-debug-log' recipe, AFTER
;; the normal init has run.  It turns on Emacs's debugging facilities and
;; mirrors *Messages*, *Warnings*, *Backtrace*, the native-comp/compile logs,
;; and a backtrace for every error into a per-session directory (var/debug/,
;; gitignored via var/).  The recipe passes that directory in
;; JOTAIN_DEBUG_DIR.
;;
;; This file is intentionally OUTSIDE lisp/ and test/: it is not part of the
;; loaded configuration, so the use-package scanner (nix/use-package.nix) and
;; the elisp-compile/elisp-lint checks never see it.

(require 'backtrace)

(defvar jotain-debug-directory
  (or (getenv "JOTAIN_DEBUG_DIR")
      (expand-file-name "var/debug/session" user-emacs-directory))
  "Directory the debug harness writes its logs into.")

(defun jotain-debug--file (name)
  "Return NAME under `jotain-debug-directory', ensuring the directory exists."
  (make-directory jotain-debug-directory t)
  (expand-file-name name jotain-debug-directory))

(defvar jotain-debug--errors-file (jotain-debug--file "errors.log")
  "File that accumulates a backtrace for every error that reaches the debugger.")

(defun jotain-debug--append (file text)
  "Append TEXT to FILE, creating it if necessary.  Never signals."
  (ignore-errors
    (let ((coding-system-for-write 'utf-8))
      (write-region text nil file 'append 'silent))))

(defun jotain-debug--dump-buffer (buffer file)
  "Overwrite FILE with BUFFER's contents when it exists and is non-empty."
  (ignore-errors
    (when-let* ((buf (get-buffer buffer)))
      (with-current-buffer buf
        (when (> (buffer-size) 0)
          (let ((coding-system-for-write 'utf-8))
            (write-region (point-min) (point-max) file nil 'silent)))))))

(defun jotain-debug-dump-now ()
  "Flush *Messages*, *Warnings*, *Backtrace* and the compile logs to disk.
Runs automatically on exit; call it interactively to snapshot the
current state without quitting (handy before a suspected crash)."
  (interactive)
  (jotain-debug--dump-buffer "*Messages*"   (jotain-debug--file "messages.log"))
  (jotain-debug--dump-buffer "*Warnings*"   (jotain-debug--file "warnings.log"))
  (jotain-debug--dump-buffer "*Backtrace*"  (jotain-debug--file "backtrace.log"))
  (jotain-debug--dump-buffer "*Compile-Log*" (jotain-debug--file "compile-log.log"))
  (jotain-debug--dump-buffer "*Async-native-compile-log*"
                             (jotain-debug--file "native-comp.log"))
  (when (called-interactively-p 'interactive)
    (message "jotain-debug: logs flushed to %s" jotain-debug-directory)))

(defun jotain-debug--log-backtrace (&rest _)
  "Append the current backtrace to `jotain-debug--errors-file'.
Advises `debug' so every error is logged even when the interactive
debugger is dismissed with `q'."
  (ignore-errors
    (jotain-debug--append
     jotain-debug--errors-file
     (concat "\n;; " (format-time-string "%FT%T%z") "\n"
             (backtrace-to-string
              (backtrace-get-frames #'jotain-debug--log-backtrace))
             "\n"))))

;; --- Turn the debugging facilities on -----------------------------------

(setq debug-on-error t                 ; interactive backtrace on any error
      message-log-max t                ; keep every message in *Messages*
      backtrace-line-length 0          ; never truncate backtrace lines
      warning-minimum-log-level :debug); log warnings of every severity
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors t))

(advice-add 'debug :before #'jotain-debug--log-backtrace)
(add-hook 'kill-emacs-hook #'jotain-debug-dump-now)

;; Full eglot/jsonrpc traffic once eglot loads, for LSP debugging.
(with-eval-after-load 'eglot
  (setopt eglot-events-buffer-config '(:size 2000000 :format full)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "jotain-debug: full debugging on; logs → %s (M-x jotain-debug-dump-now to flush)"
                     jotain-debug-directory)))
