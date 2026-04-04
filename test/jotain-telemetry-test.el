;;; jotain-telemetry-test.el --- Tests for jotain-telemetry -*- lexical-binding:t; -*-

;;; Commentary:
;; ERT test suite for the PostHog telemetry module.

;;; Code:

(require 'ert)
(require 'jotain-telemetry)

;;;; State isolation

(defmacro jotain-telemetry-test--with-clean-state (&rest body)
  "Run BODY with all telemetry state variables locally bound to clean defaults."
  (declare (indent 0) (debug t))
  `(let ((jotain-telemetry-enabled t)
         (jotain-telemetry-api-key "phc_test_key")
         (jotain-telemetry-host "https://test.posthog.invalid")
         (jotain-telemetry-debug nil)
         (jotain-telemetry--queue nil)
         (jotain-telemetry--session-id "test-session-id")
         (jotain-telemetry--distinct-id "test-distinct-id")
         (jotain-telemetry--session-start (float-time))
         (jotain-telemetry--command-counts (make-hash-table :test #'eq))
         (jotain-telemetry--command-total 0)
         (jotain-telemetry--mode-times (make-hash-table :test #'eq))
         (jotain-telemetry--current-mode nil)
         (jotain-telemetry--last-mode-change nil)
         (jotain-telemetry--flush-timer nil)
         (jotain-telemetry--last-error-time 0)
         (jotain-telemetry--buffers-created 0)
         (jotain-telemetry--known-buffer-count 0))
     ,@body))

(defmacro jotain-telemetry-test--with-env (key value &rest body)
  "Run BODY with environment variable KEY set to VALUE, restoring afterward."
  (declare (indent 2) (debug t))
  (let ((orig (gensym "orig")))
    `(let ((,orig (getenv ,key)))
       (unwind-protect
           (progn (setenv ,key ,value) ,@body)
         (setenv ,key ,orig)))))

;;;; Group A — Pure functions

(ert-deftest jotain-telemetry-test-uuid-format ()
  "UUID has v4 format: 8-4-4-4-12 hex digits with correct version/variant bits."
  (let ((uuid (jotain-telemetry--generate-uuid)))
    (should (stringp uuid))
    (should (= (length uuid) 36))
    (should (string-match-p
             "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}$"
             uuid))))

(ert-deftest jotain-telemetry-test-uuid-uniqueness ()
  "Two generated UUIDs should differ."
  (should-not (string= (jotain-telemetry--generate-uuid)
                        (jotain-telemetry--generate-uuid))))

(ert-deftest jotain-telemetry-test-api-key-from-custom ()
  "Returns the customizable API key when env var is unset."
  (jotain-telemetry-test--with-env "POSTHOG_API_KEY" nil
    (let ((jotain-telemetry-api-key "custom-key"))
      (should (equal (jotain-telemetry--api-key) "custom-key")))))

(ert-deftest jotain-telemetry-test-api-key-from-env ()
  "Environment variable POSTHOG_API_KEY takes precedence."
  (jotain-telemetry-test--with-env "POSTHOG_API_KEY" "env-key"
    (let ((jotain-telemetry-api-key "custom-key"))
      (should (equal (jotain-telemetry--api-key) "env-key")))))

(ert-deftest jotain-telemetry-test-api-key-nil ()
  "Returns nil when neither env var nor custom is set."
  (jotain-telemetry-test--with-env "POSTHOG_API_KEY" nil
    (let ((jotain-telemetry-api-key nil))
      (should-not (jotain-telemetry--api-key)))))

(ert-deftest jotain-telemetry-test-active-p-true ()
  "Active when enabled and API key is present."
  (jotain-telemetry-test--with-env "POSTHOG_API_KEY" nil
    (let ((jotain-telemetry-enabled t)
          (jotain-telemetry-api-key "key"))
      (should (jotain-telemetry--active-p)))))

(ert-deftest jotain-telemetry-test-active-p-disabled ()
  "Inactive when disabled."
  (jotain-telemetry-test--with-env "POSTHOG_API_KEY" nil
    (let ((jotain-telemetry-enabled nil)
          (jotain-telemetry-api-key "key"))
      (should-not (jotain-telemetry--active-p)))))

(ert-deftest jotain-telemetry-test-active-p-no-key ()
  "Inactive when no API key is configured."
  (jotain-telemetry-test--with-env "POSTHOG_API_KEY" nil
    (let ((jotain-telemetry-enabled t)
          (jotain-telemetry-api-key nil))
      (should-not (jotain-telemetry--active-p)))))

(ert-deftest jotain-telemetry-test-iso8601-format ()
  "Timestamp matches ISO 8601 pattern."
  (let ((ts (jotain-telemetry--iso8601)))
    (should (stringp ts))
    (should (string-match-p
             "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}[+-][0-9]\\{4\\}$"
             ts))))

;;;; Group B — State mutation

(ert-deftest jotain-telemetry-test-enqueue-pushes-event ()
  "Enqueue adds an event with correct structure to the queue."
  (jotain-telemetry-test--with-clean-state
    (jotain-telemetry--enqueue "test_event" '((foo . "bar")))
    (should (= (length jotain-telemetry--queue) 1))
    (let ((event (car jotain-telemetry--queue)))
      (should (equal (alist-get 'event event) "test_event"))
      (should (equal (alist-get 'distinct_id event) "test-distinct-id"))
      (let ((props (alist-get 'properties event)))
        (should (equal (alist-get '$session_id props) "test-session-id"))
        (should (equal (alist-get 'foo props) "bar"))))))

(ert-deftest jotain-telemetry-test-enqueue-noop-when-inactive ()
  "Enqueue does nothing when telemetry is inactive."
  (jotain-telemetry-test--with-clean-state
    (let ((jotain-telemetry-enabled nil))
      (jotain-telemetry--enqueue "test_event" '((foo . "bar")))
      (should-not jotain-telemetry--queue))))

(ert-deftest jotain-telemetry-test-track-command-increments ()
  "track-command increments hash-table count and total."
  (jotain-telemetry-test--with-clean-state
    (let ((this-command 'save-buffer))
      (jotain-telemetry--track-command)
      (jotain-telemetry--track-command)
      (jotain-telemetry--track-command))
    (should (= (gethash 'save-buffer jotain-telemetry--command-counts 0) 3))
    (should (= jotain-telemetry--command-total 3))))

(ert-deftest jotain-telemetry-test-track-command-ignores-nil ()
  "track-command does nothing when this-command is nil."
  (jotain-telemetry-test--with-clean-state
    (let ((this-command nil))
      (jotain-telemetry--track-command))
    (should (= (hash-table-count jotain-telemetry--command-counts) 0))
    (should (= jotain-telemetry--command-total 0))))

(ert-deftest jotain-telemetry-test-flush-command-counts-top-20 ()
  "flush-command-counts enqueues at most 20 commands sorted by count."
  (jotain-telemetry-test--with-clean-state
    (dotimes (i 25)
      (puthash (intern (format "cmd-%02d" i)) (1+ i) jotain-telemetry--command-counts))
    (jotain-telemetry--flush-command-counts)
    (should (= (length jotain-telemetry--queue) 1))
    (let* ((event (car jotain-telemetry--queue))
           (props (alist-get 'properties event))
           (cmds (alist-get 'commands props)))
      (should (= (length cmds) 20))
      ;; First entry should have the highest count (25)
      (should (= (cdar cmds) 25)))
    (should (= (hash-table-count jotain-telemetry--command-counts) 0))))

(ert-deftest jotain-telemetry-test-flush-command-counts-noop-when-empty ()
  "flush-command-counts does not enqueue when hash-table is empty."
  (jotain-telemetry-test--with-clean-state
    (jotain-telemetry--flush-command-counts)
    (should-not jotain-telemetry--queue)))

(ert-deftest jotain-telemetry-test-flush-command-counts-json-serializable ()
  "Regression: flushed command_frequency event must be json-serializable."
  (jotain-telemetry-test--with-clean-state
    (puthash 'next-line 58 jotain-telemetry--command-counts)
    (puthash 'previous-line 31 jotain-telemetry--command-counts)
    (puthash 'save-buffer 5 jotain-telemetry--command-counts)
    (jotain-telemetry--flush-command-counts)
    (should (= (length jotain-telemetry--queue) 1))
    ;; This is the call that failed before the fix:
    (let ((json-str (json-serialize (car jotain-telemetry--queue))))
      (should (stringp json-str)))))

(ert-deftest jotain-telemetry-test-track-mode-change-records-elapsed ()
  "track-mode-change accumulates time for the previous mode."
  (jotain-telemetry-test--with-clean-state
    (setq jotain-telemetry--current-mode 'emacs-lisp-mode
          jotain-telemetry--last-mode-change (- (float-time) 60))
    (let ((major-mode 'org-mode))
      (jotain-telemetry--track-mode-change))
    (let ((elapsed (gethash 'emacs-lisp-mode jotain-telemetry--mode-times 0)))
      (should (> elapsed 58))
      (should (< elapsed 62)))
    (should (eq jotain-telemetry--current-mode 'org-mode))))

(ert-deftest jotain-telemetry-test-track-mode-change-first-call ()
  "First call sets current-mode and last-mode-change without error."
  (jotain-telemetry-test--with-clean-state
    (let ((major-mode 'text-mode))
      (jotain-telemetry--track-mode-change))
    (should (eq jotain-telemetry--current-mode 'text-mode))
    (should (numberp jotain-telemetry--last-mode-change))
    (should (= (hash-table-count jotain-telemetry--mode-times) 0))))

(ert-deftest jotain-telemetry-test-flush-mode-times-enqueues-per-mode ()
  "flush-mode-times creates one event per mode with non-zero time."
  (jotain-telemetry-test--with-clean-state
    (puthash 'emacs-lisp-mode 120.0 jotain-telemetry--mode-times)
    (puthash 'org-mode 45.0 jotain-telemetry--mode-times)
    ;; Set last-mode-change 30s in the past so text-mode gets deterministic non-zero time.
    (setq jotain-telemetry--current-mode 'text-mode
          jotain-telemetry--last-mode-change (- (float-time) 30))
    (jotain-telemetry--flush-mode-times)
    (let ((mode-events (seq-filter
                        (lambda (ev) (equal (alist-get 'event ev) "major_mode_time"))
                        jotain-telemetry--queue)))
      (should (= (length mode-events) 3)))
    (should (= (hash-table-count jotain-telemetry--mode-times) 0))))

(ert-deftest jotain-telemetry-test-flush-mode-times-skips-zero ()
  "flush-mode-times does not enqueue modes with zero seconds."
  (jotain-telemetry-test--with-clean-state
    (puthash 'some-mode 0.0 jotain-telemetry--mode-times)
    (setq jotain-telemetry--current-mode nil
          jotain-telemetry--last-mode-change nil)
    (jotain-telemetry--flush-mode-times)
    (should-not jotain-telemetry--queue)))

(ert-deftest jotain-telemetry-test-track-error-enqueues ()
  "track-error enqueues an error_occurred event."
  (jotain-telemetry-test--with-clean-state
    (let ((this-command 'next-line)
          (major-mode 'emacs-lisp-mode))
      (jotain-telemetry--track-error '(end-of-buffer "End of buffer") "ctx" nil))
    (should (= (length jotain-telemetry--queue) 1))
    (let* ((event (car jotain-telemetry--queue))
           (props (alist-get 'properties event)))
      (should (equal (alist-get 'event event) "error_occurred"))
      (should (equal (alist-get 'error_type props) "end-of-buffer"))
      (should (equal (alist-get 'command props) "next-line")))))

(ert-deftest jotain-telemetry-test-track-error-debounce ()
  "track-error suppresses events within 10 seconds of the last."
  (jotain-telemetry-test--with-clean-state
    ;; Recent error — should be suppressed
    (setq jotain-telemetry--last-error-time (float-time))
    (jotain-telemetry--track-error '(error "oops") "" nil)
    (should-not jotain-telemetry--queue)
    ;; Old error — should fire
    (setq jotain-telemetry--last-error-time (- (float-time) 11))
    (jotain-telemetry--track-error '(error "oops") "" nil)
    (should (= (length jotain-telemetry--queue) 1))))

(ert-deftest jotain-telemetry-test-track-buffer-create-increments ()
  "track-buffer-create detects new buffers."
  (jotain-telemetry-test--with-clean-state
    (let ((actual (length (buffer-list))))
      (setq jotain-telemetry--known-buffer-count (- actual 3))
      (jotain-telemetry--track-buffer-create)
      (should (= jotain-telemetry--buffers-created 3))
      (should (= jotain-telemetry--known-buffer-count actual)))))

(ert-deftest jotain-telemetry-test-track-buffer-create-no-change ()
  "track-buffer-create does nothing when buffer count is unchanged."
  (jotain-telemetry-test--with-clean-state
    (setq jotain-telemetry--known-buffer-count (length (buffer-list)))
    (jotain-telemetry--track-buffer-create)
    (should (= jotain-telemetry--buffers-created 0))))

;;;; Group C — Full payload serialization (integration)

(ert-deftest jotain-telemetry-test-full-batch-json-serializable ()
  "Integration: a complete batch payload with all event types can be serialized."
  (jotain-telemetry-test--with-clean-state
    ;; Populate command counts
    (puthash 'next-line 58 jotain-telemetry--command-counts)
    (puthash 'previous-line 31 jotain-telemetry--command-counts)
    (setq jotain-telemetry--command-total 89)
    ;; Populate mode times
    (puthash 'emacs-lisp-mode 120.0 jotain-telemetry--mode-times)
    (puthash 'org-mode 45.0 jotain-telemetry--mode-times)
    (setq jotain-telemetry--current-mode 'text-mode
          jotain-telemetry--last-mode-change (float-time))
    ;; Enqueue a plain event too
    (jotain-telemetry--enqueue "test_event" '((foo . "bar")))
    ;; Flush aggregated data into queue
    (jotain-telemetry--flush-command-counts)
    (jotain-telemetry--flush-mode-times)
    ;; Build the exact payload structure that send-batch uses
    (let* ((events (nreverse jotain-telemetry--queue))
           (payload `((api_key . ,(jotain-telemetry--api-key))
                      (batch . ,(vconcat events))))
           (json-str (json-serialize payload)))
      (should (stringp json-str))
      (should (> (length json-str) 0)))))

;;;; Group D — Debug logging

(ert-deftest jotain-telemetry-test-debug-log-writes-buffer ()
  "debug-log writes to *jotain-telemetry* buffer when debug is enabled."
  (jotain-telemetry-test--with-clean-state
    (let ((jotain-telemetry-debug t))
      (unwind-protect
          (progn
            (jotain-telemetry--debug-log "test_event" '((key . "val")))
            (let ((buf (get-buffer "*jotain-telemetry*")))
              (should buf)
              (should (> (buffer-size buf) 0))
              (with-current-buffer buf
                (should (string-match-p "test_event" (buffer-string))))))
        (when-let* ((buf (get-buffer "*jotain-telemetry*")))
          (kill-buffer buf))))))

(ert-deftest jotain-telemetry-test-debug-log-noop-when-disabled ()
  "debug-log does not create a buffer when debug is disabled."
  (jotain-telemetry-test--with-clean-state
    (let ((jotain-telemetry-debug nil))
      ;; Kill pre-existing buffer if any
      (when-let* ((buf (get-buffer "*jotain-telemetry*")))
        (kill-buffer buf))
      (jotain-telemetry--debug-log "test_event" '((key . "val")))
      (should-not (get-buffer "*jotain-telemetry*")))))

;;;; Group E — Edge cases

(ert-deftest jotain-telemetry-test-track-command-ignores-non-symbol ()
  "track-command ignores non-symbol this-command (e.g. lambda)."
  (jotain-telemetry-test--with-clean-state
    (let ((this-command (lambda () nil)))
      (jotain-telemetry--track-command))
    (should (= (hash-table-count jotain-telemetry--command-counts) 0))
    (should (= jotain-telemetry--command-total 0))))

(ert-deftest jotain-telemetry-test-track-error-non-symbol-command ()
  "track-error uses \"unknown\" when this-command is not a symbol."
  (jotain-telemetry-test--with-clean-state
    (let ((this-command (lambda () nil))
          (major-mode 'fundamental-mode))
      (jotain-telemetry--track-error '(error "oops") "" nil))
    (should (= (length jotain-telemetry--queue) 1))
    (let* ((props (alist-get 'properties (car jotain-telemetry--queue))))
      (should (equal (alist-get 'command props) "unknown")))))

(ert-deftest jotain-telemetry-test-track-error-at-boundary ()
  "track-error suppresses when within 10 seconds (strict > check)."
  (jotain-telemetry-test--with-clean-state
    ;; 9.99s ago — just under the 10s threshold, should be suppressed
    (setq jotain-telemetry--last-error-time (- (float-time) 9.99))
    (jotain-telemetry--track-error '(error "oops") "" nil)
    (should-not jotain-telemetry--queue)))

(ert-deftest jotain-telemetry-test-track-buffer-create-handles-decrease ()
  "track-buffer-create does not decrement when buffers are killed."
  (jotain-telemetry-test--with-clean-state
    (setq jotain-telemetry--known-buffer-count (+ (length (buffer-list)) 5))
    (jotain-telemetry--track-buffer-create)
    (should (= jotain-telemetry--buffers-created 0))
    ;; known-buffer-count should update to actual count
    (should (= jotain-telemetry--known-buffer-count (length (buffer-list))))))

(ert-deftest jotain-telemetry-test-enqueue-preserves-order ()
  "Events pushed to queue are in reverse insertion order."
  (jotain-telemetry-test--with-clean-state
    (jotain-telemetry--enqueue "first" nil)
    (jotain-telemetry--enqueue "second" nil)
    (jotain-telemetry--enqueue "third" nil)
    (should (= (length jotain-telemetry--queue) 3))
    ;; push prepends, so most recent is first
    (should (equal (alist-get 'event (nth 0 jotain-telemetry--queue)) "third"))
    (should (equal (alist-get 'event (nth 1 jotain-telemetry--queue)) "second"))
    (should (equal (alist-get 'event (nth 2 jotain-telemetry--queue)) "first"))))

(ert-deftest jotain-telemetry-test-flush-command-counts-exactly-20 ()
  "flush-command-counts includes all commands when exactly 20."
  (jotain-telemetry-test--with-clean-state
    (dotimes (i 20)
      (puthash (intern (format "cmd-%02d" i)) (1+ i) jotain-telemetry--command-counts))
    (jotain-telemetry--flush-command-counts)
    (let* ((event (car jotain-telemetry--queue))
           (cmds (alist-get 'commands (alist-get 'properties event))))
      (should (= (length cmds) 20)))))

(ert-deftest jotain-telemetry-test-flush-command-counts-single ()
  "flush-command-counts works with a single command."
  (jotain-telemetry-test--with-clean-state
    (puthash 'self-insert-command 42 jotain-telemetry--command-counts)
    (jotain-telemetry--flush-command-counts)
    (let* ((event (car jotain-telemetry--queue))
           (cmds (alist-get 'commands (alist-get 'properties event))))
      (should (= (length cmds) 1))
      (should (equal (caar cmds) 'self-insert-command))
      (should (= (cdar cmds) 42)))))

(ert-deftest jotain-telemetry-test-debug-log-truncates-large-buffer ()
  "debug-log truncates when buffer exceeds max size."
  (jotain-telemetry-test--with-clean-state
    (let ((jotain-telemetry-debug t)
          (jotain-telemetry--debug-buffer-max 200))
      (unwind-protect
          (progn
            ;; Write enough to exceed the small max
            (dotimes (_ 20)
              (jotain-telemetry--debug-log "big_event"
                                           '((data . "padding-padding-padding"))))
            (let ((buf (get-buffer "*jotain-telemetry*")))
              (should buf)
              (should (<= (buffer-size buf)
                          (+ jotain-telemetry--debug-buffer-max 100)))))
        (when-let* ((buf (get-buffer "*jotain-telemetry*")))
          (kill-buffer buf))))))

(ert-deftest jotain-telemetry-test-enqueue-with-debug-enabled ()
  "enqueue triggers debug logging when debug is on."
  (jotain-telemetry-test--with-clean-state
    (let ((jotain-telemetry-debug t))
      (unwind-protect
          (progn
            (jotain-telemetry--enqueue "debug_test" '((key . "val")))
            (should (= (length jotain-telemetry--queue) 1))
            (let ((buf (get-buffer "*jotain-telemetry*")))
              (should buf)
              (with-current-buffer buf
                (should (string-match-p "debug_test" (buffer-string))))))
        (when-let* ((buf (get-buffer "*jotain-telemetry*")))
          (kill-buffer buf))))))

;;;; Group F — Distinct ID persistence

(ert-deftest jotain-telemetry-test-get-distinct-id-creates-file ()
  "get-distinct-id creates a new ID file when none exists."
  (let* ((tmpdir (make-temp-file "jotain-test-" t))
         (jotain-telemetry--id-file (expand-file-name "id" tmpdir)))
    (unwind-protect
        (let ((id (jotain-telemetry--get-distinct-id)))
          (should (stringp id))
          (should (= (length id) 36))
          (should (file-exists-p jotain-telemetry--id-file))
          ;; File content matches returned ID
          (should (equal id (string-trim
                             (with-temp-buffer
                               (insert-file-contents jotain-telemetry--id-file)
                               (buffer-string))))))
      (delete-directory tmpdir t))))

(ert-deftest jotain-telemetry-test-get-distinct-id-reads-existing ()
  "get-distinct-id returns existing ID from file."
  (let* ((tmpdir (make-temp-file "jotain-test-" t))
         (jotain-telemetry--id-file (expand-file-name "id" tmpdir))
         (expected "aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee"))
    (unwind-protect
        (progn
          (with-temp-file jotain-telemetry--id-file
            (insert expected))
          (should (equal (jotain-telemetry--get-distinct-id) expected)))
      (delete-directory tmpdir t))))

(ert-deftest jotain-telemetry-test-get-distinct-id-whitespace-only-file ()
  "get-distinct-id generates new ID when file has only whitespace."
  (let* ((tmpdir (make-temp-file "jotain-test-" t))
         (jotain-telemetry--id-file (expand-file-name "id" tmpdir)))
    (unwind-protect
        (progn
          (with-temp-file jotain-telemetry--id-file
            (insert "   \n  "))
          (let ((id (jotain-telemetry--get-distinct-id)))
            (should (stringp id))
            (should (= (length id) 36))
            ;; File should now have the new ID
            (should (equal id (string-trim
                               (with-temp-buffer
                                 (insert-file-contents jotain-telemetry--id-file)
                                 (buffer-string)))))))
      (delete-directory tmpdir t))))

;;;; Group G — Lifecycle

(ert-deftest jotain-telemetry-test-mode-refuses-without-key ()
  "Enabling mode without API key sets mode back to nil."
  (jotain-telemetry-test--with-env "POSTHOG_API_KEY" nil
    (let ((jotain-telemetry-api-key nil)
          (jotain-telemetry-mode nil))
      (jotain-telemetry-mode 1)
      (should-not jotain-telemetry-mode))))

(ert-deftest jotain-telemetry-test-teardown-preserves-foreign-error-function ()
  "teardown does not overwrite a non-telemetry command-error-function."
  (jotain-telemetry-test--with-clean-state
    (let ((custom-fn (lambda (data context caller) nil)))
      (setq command-error-function custom-fn)
      (jotain-telemetry--teardown)
      (should (eq command-error-function custom-fn)))))

(provide 'jotain-telemetry-test)
;;; jotain-telemetry-test.el ends here
