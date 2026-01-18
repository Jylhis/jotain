;;; test-app-launcher.el --- Tests for app-launcher.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'app-launcher)
(require 'cl-lib)

;; Duplicate the legacy logic for benchmarking
(defun app-launcher--format-command-legacy (exec)
  (let (result)
    (dolist (chunk (split-string exec " ") result)
      (unless (or (equal chunk "%U")
                  (equal chunk "%F")
                  (equal chunk "%u")
                  (equal chunk "%f"))
        (setq result (concat result chunk " "))))))

(ert-deftest test-app-launcher/format-command-legacy ()
  "Verify the legacy implementation behavior."
  :tags '(fast benchmark)
  (should (equal (app-launcher--format-command-legacy "foo %F bar %u baz")
                 "foo bar baz ")))

(ert-deftest test-app-launcher/benchmark ()
  "Benchmark the command formatting."
  :tags '(fast benchmark)
  (let ((exec "command --arg1 value1 %U --arg2 value2 %F --long-argument %u --another-one %f")
        (n 50000))
    (message "Benchmarking legacy implementation (%d iterations)..." n)
    (let ((time (benchmark-run n (app-launcher--format-command-legacy exec))))
      (message "Legacy: %.4fs" (car time)))

    (when (fboundp 'app-launcher--format-command)
      (message "Benchmarking new implementation (%d iterations)..." n)
      (let ((time (benchmark-run n (app-launcher--format-command exec))))
        (message "New:    %.4fs" (car time))
        ;; Check correctness (allowing for trimmed trailing space)
        (should (string-prefix-p (string-trim-right (app-launcher--format-command exec))
                                 (string-trim-right (app-launcher--format-command-legacy exec))))))))
