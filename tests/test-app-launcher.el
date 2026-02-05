;;; test-app-launcher.el --- Tests for app-launcher -*- lexical-binding: t; -*-

(require 'ert)
(require 'app-launcher)
(require 'cl-lib)

(ert-deftest test-app-launcher-command-generation ()
  "Test that the command generation logic works correctly and handles field codes."
  :tags '(fast)
  (let ((app-launcher--cache (make-hash-table :test #'equal))
        (executed-command nil))

    ;; Mock cache entry
    (puthash "TestApp"
             '((exec . "test-command %U --arg %F fixed-arg")
               (file . "/tmp/test.desktop"))
             app-launcher--cache)

    ;; Mock call-process-shell-command
    (cl-letf (((symbol-function 'call-process-shell-command)
               (lambda (command &rest args)
                 (setq executed-command command))))

      (app-launcher--action-function-default "TestApp")

      ;; Verify the command matches expectation
      ;; The original implementation produces "test-command --arg fixed-arg "
      ;; The new implementation (using mapconcat) will produce "test-command --arg fixed-arg"
      (should (string-prefix-p "test-command --arg fixed-arg" executed-command)))))

(ert-deftest benchmark-app-launcher-string-concat ()
  "Benchmark the string concatenation logic."
  :tags '(fast)
  (let ((exec-string "command arg1 arg2 %U arg3 %F arg4")
        (iterations 10000))
    (message "Benchmarking string concatenation with %d iterations..." iterations)

    (let ((start-time (float-time)))
      (dotimes (_ iterations)
        (let (result)
          (dolist (chunk (split-string exec-string " ") result)
            (unless (or (equal chunk "%U")
                        (equal chunk "%F")
                        (equal chunk "%u")
                        (equal chunk "%f"))
              (setq result (concat result chunk " "))))))
      (message "Baseline time: %f seconds" (- (float-time) start-time)))

    ;; Optimized version 1 (cl-remove-if)
    (let ((start-time (float-time)))
      (dotimes (_ iterations)
        (mapconcat #'identity
                   (cl-remove-if (lambda (chunk)
                                   (member chunk '("%U" "%F" "%u" "%f")))
                                 (split-string exec-string " "))
                   " "))
      (message "Optimized (cl-remove-if) time: %f seconds" (- (float-time) start-time)))

    ;; Optimized version 2 (cl-loop collect)
    (let ((start-time (float-time)))
      (dotimes (_ iterations)
        (mapconcat #'identity
                   (cl-loop for chunk in (split-string exec-string " ")
                            unless (member chunk '("%U" "%F" "%u" "%f"))
                            collect chunk)
                   " "))
      (message "Optimized (cl-loop) time: %f seconds" (- (float-time) start-time)))))
