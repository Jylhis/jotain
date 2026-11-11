;;; debug-init.el --- Full-debug launch harness for the configuration  -*- lexical-binding: t; -*-

;; Loaded by `just run-built-debug-log' after the normal init.  Turns on
;; Emacs's debugging facilities and mirrors *Messages*, *Warnings*,
;; *Backtrace*, the compile logs, and a backtrace per error into the
;; session directory the recipe passes in EMACS_CONFIG_DEBUG_DIR (under
;; var/debug/).
;;
;; Kept outside lisp/ and test/ so the use-package scanner and the
;; elisp-compile/elisp-lint checks never see it.

(require 'backtrace)

(defvar my-debug-directory
  (or (getenv "EMACS_CONFIG_DEBUG_DIR")
      (expand-file-name "var/debug/session" user-emacs-directory))
  "Directory the debug harness writes its logs into.")

(defun my-debug--file (name)
  "Return NAME under `my-debug-directory', ensuring the directory exists."
  (make-directory my-debug-directory t)
  (expand-file-name name my-debug-directory))

(defvar my-debug--errors-file (my-debug--file "errors.log")
  "File that accumulates a backtrace for every error that reaches the debugger.")

(defun my-debug--append (file text)
  "Append TEXT to FILE, creating it if necessary.  Never signals."
  (ignore-errors
    (let ((coding-system-for-write 'utf-8))
      (write-region text nil file 'append 'silent))))

(defun my-debug--dump-buffer (buffer file)
  "Overwrite FILE with BUFFER's contents when it exists and is non-empty."
  (ignore-errors
    (when-let* ((buf (get-buffer buffer)))
      (with-current-buffer buf
        (when (> (buffer-size) 0)
          (let ((coding-system-for-write 'utf-8))
            (write-region (point-min) (point-max) file nil 'silent)))))))

(defun my-debug-dump-now ()
  "Flush *Messages*, *Warnings*, *Backtrace* and the compile logs to disk.
Runs automatically on exit; call it interactively to snapshot the
current state without quitting (handy before a suspected crash)."
  (interactive)
  (my-debug--dump-buffer "*Messages*"   (my-debug--file "messages.log"))
  (my-debug--dump-buffer "*Warnings*"   (my-debug--file "warnings.log"))
  (my-debug--dump-buffer "*Backtrace*"  (my-debug--file "backtrace.log"))
  (my-debug--dump-buffer "*Compile-Log*" (my-debug--file "compile-log.log"))
  (my-debug--dump-buffer "*Async-native-compile-log*"
                         (my-debug--file "native-comp.log"))
  (when (called-interactively-p 'interactive)
    (message "my-debug: logs flushed to %s" my-debug-directory)))

(defun my-debug--log-backtrace (&rest _)
  "Append the current backtrace to `my-debug--errors-file'.
Advises `debug' so every error is logged even when the interactive
debugger is dismissed with `q'."
  (ignore-errors
    (my-debug--append
     my-debug--errors-file
     (concat "\n;; " (format-time-string "%FT%T%z") "\n"
             (backtrace-to-string
              (backtrace-get-frames #'my-debug--log-backtrace))
             "\n"))))

;;; Turn the debugging facilities on

(setopt debug-on-error t                 ; interactive backtrace on any error
        message-log-max t                ; keep every message in *Messages*
        backtrace-line-length 0          ; never truncate backtrace lines
        warning-minimum-log-level :debug); log warnings of every severity
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors t))

(advice-add 'debug :before #'my-debug--log-backtrace)
(add-hook 'kill-emacs-hook #'my-debug-dump-now)

;; Full eglot/jsonrpc traffic once eglot loads, for LSP debugging.
(with-eval-after-load 'eglot
  (setopt eglot-events-buffer-config '(:size 2000000 :format full)))

(defun my-debug--announce ()
  "Tell the user where the debug logs go."
  (message "my-debug: full debugging on; logs → %s (M-x my-debug-dump-now to flush)"
           my-debug-directory))

(add-hook 'emacs-startup-hook #'my-debug--announce)

(provide 'debug-init)
;;; debug-init.el ends here
