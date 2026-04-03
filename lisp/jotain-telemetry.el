;;; jotain-telemetry.el --- PostHog telemetry for Jotain Emacs -*- lexical-binding:t; -*-

;; Author: Markus Jylhänkangas
;; URL: https://github.com/jylhis/jotain

;;; Commentary:

;; Tracks Emacs usage patterns via PostHog analytics.
;; Events are batched in memory and flushed periodically.
;;
;; Configuration:
;;   Set POSTHOG_API_KEY environment variable, or customize
;;   `jotain-telemetry-api-key'.  Telemetry is silently disabled
;;   if no API key is found.

;;; Code:

(require 'url)
(require 'json)

;;;; Customization

(defgroup jotain-telemetry nil
  "PostHog telemetry for Jotain Emacs."
  :group 'jotain
  :prefix "jotain-telemetry-")

(defcustom jotain-telemetry-enabled t
  "When non-nil, telemetry collection is active.
Telemetry is still silently disabled if no API key is configured."
  :type 'boolean)

(defcustom jotain-telemetry-api-key "phc_IKKkL2DWB3zpFreX1quIuHNI7MBnaUOkA8HUpTP5taX"
  "PostHog project API key (write-only).
The environment variable POSTHOG_API_KEY takes precedence."
  :type '(choice (const :tag "None" nil) string))

(defcustom jotain-telemetry-host "https://eu.i.posthog.com"
  "PostHog instance URL."
  :type 'string)

(defcustom jotain-telemetry-flush-interval 300
  "Seconds between automatic event flushes."
  :type 'integer)

(defcustom jotain-telemetry-debug nil
  "When non-nil, log telemetry events to the *jotain-telemetry* buffer."
  :type 'boolean)

;;;; Internal variables

(defvar jotain-telemetry--queue nil
  "List of event alists awaiting flush.")

(defvar jotain-telemetry--session-id nil
  "UUID for the current Emacs session.")

(defvar jotain-telemetry--distinct-id nil
  "Persistent UUID identifying this user.")

(defvar jotain-telemetry--session-start nil
  "Float-time when the session started.")

(defvar jotain-telemetry--command-counts nil
  "Hash-table mapping command symbols to execution counts.")

(defvar jotain-telemetry--command-total 0
  "Total commands executed this flush period.")

(defvar jotain-telemetry--mode-times nil
  "Hash-table mapping major-mode symbols to accumulated seconds.")

(defvar jotain-telemetry--current-mode nil
  "The major mode recorded at the last mode change.")

(defvar jotain-telemetry--last-mode-change nil
  "Float-time of the last major mode change.")

(defvar jotain-telemetry--flush-timer nil
  "Timer for periodic flush.")

(defvar jotain-telemetry--last-error-time 0
  "Float-time of the last error event, for debouncing.")

(defvar jotain-telemetry--buffers-created 0
  "Count of buffers created during this session.")

(defvar jotain-telemetry--known-buffer-count 0
  "Buffer count at last check, used to detect new buffer creation.")

(defvar jotain-telemetry--id-file
  (expand-file-name "posthog-id" (expand-file-name "jotain" (or (getenv "XDG_CONFIG_HOME")
                                                                 "~/.config")))
  "File storing the persistent distinct ID.")

;;;; UUID generation

(defun jotain-telemetry--generate-uuid ()
  "Generate a version 4 UUID string."
  (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior #x4000 (logand #x0fff (random 65536)))
          (logior #x8000 (logand #x3fff (random 65536)))
          (random 65536) (random 65536) (random 65536)))

(defun jotain-telemetry--get-distinct-id ()
  "Load or create the persistent distinct ID."
  (let ((existing (and (file-exists-p jotain-telemetry--id-file)
                       (string-trim
                        (with-temp-buffer
                          (insert-file-contents jotain-telemetry--id-file)
                          (buffer-string))))))
    (if (and existing (not (string-empty-p existing)))
        existing
      (let ((id (jotain-telemetry--generate-uuid))
            (dir (file-name-directory jotain-telemetry--id-file)))
        (unless (file-directory-p dir)
          (make-directory dir t))
        (with-temp-file jotain-telemetry--id-file
          (insert id))
        id))))

;;;; API key resolution

(defun jotain-telemetry--api-key ()
  "Return the PostHog API key, or nil if unconfigured."
  (or (getenv "POSTHOG_API_KEY")
      jotain-telemetry-api-key))

(defun jotain-telemetry--active-p ()
  "Return non-nil if telemetry should be collected."
  (and jotain-telemetry-enabled
       (jotain-telemetry--api-key)))

;;;; Timestamp

(defun jotain-telemetry--iso8601 ()
  "Return current time as ISO 8601 string."
  (format-time-string "%FT%T%z" nil t))

;;;; Debug logging

(defvar jotain-telemetry--debug-buffer-max 100000
  "Maximum size in characters for the debug log buffer.")

(defun jotain-telemetry--debug-log (event-name properties)
  "Log EVENT-NAME with PROPERTIES to the *jotain-telemetry* buffer."
  (when jotain-telemetry-debug
    (with-current-buffer (get-buffer-create "*jotain-telemetry*")
      (goto-char (point-max))
      (insert (format "[%s] %s\n" (jotain-telemetry--iso8601) event-name))
      (dolist (prop properties)
        (insert (format "  %s: %S\n" (car prop) (cdr prop))))
      (insert "\n")
      (when (> (buffer-size) jotain-telemetry--debug-buffer-max)
        (delete-region (point-min)
                       (- (buffer-size) (/ jotain-telemetry--debug-buffer-max 2)))))))

;;;; Event queue

(defun jotain-telemetry--enqueue (event-name properties)
  "Add an event with EVENT-NAME and PROPERTIES alist to the queue."
  (when (jotain-telemetry--active-p)
    (jotain-telemetry--debug-log event-name properties)
    (push `((event . ,event-name)
            (distinct_id . ,jotain-telemetry--distinct-id)
            (timestamp . ,(jotain-telemetry--iso8601))
            (properties . ,(append
                            `(($session_id . ,jotain-telemetry--session-id))
                            properties)))
          jotain-telemetry--queue)))

;;;; HTTP flush

(defun jotain-telemetry--send-batch (synchronous)
  "Drain the queue and POST events to PostHog.
When SYNCHRONOUS is non-nil, block until the request completes."
  (when (and (jotain-telemetry--active-p) jotain-telemetry--queue)
    (jotain-telemetry--flush-mode-times)
    (jotain-telemetry--flush-command-counts)
    (let* ((events (nreverse jotain-telemetry--queue))
           (api-key (jotain-telemetry--api-key))
           (payload (json-serialize
                     `((api_key . ,api-key)
                       (batch . ,(vconcat events)))))
           (url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/json")))
           (url-request-data (encode-coding-string payload 'utf-8))
           (endpoint (concat jotain-telemetry-host "/batch/")))
      (setq jotain-telemetry--queue nil)
      (if synchronous
          (ignore-errors
            (let ((buf (url-retrieve-synchronously endpoint t nil 5)))
              (when (buffer-live-p buf)
                (kill-buffer buf))))
        (url-retrieve endpoint
                      (lambda (status)
                        (when-let* ((err (plist-get status :error)))
                          (message "jotain-telemetry: flush error %S" err))
                        (when (buffer-live-p (current-buffer))
                          (kill-buffer (current-buffer))))
                      nil t t)))))

(defun jotain-telemetry-flush ()
  "Send queued events to PostHog.  Safe to call interactively."
  (interactive)
  (jotain-telemetry--send-batch nil))

;;;; Hook functions

;; --- Session ---

(defun jotain-telemetry--session-start ()
  "Record session start event."
  (jotain-telemetry--enqueue
   "session_start"
   `((emacs_version . ,emacs-version)
     (system_type . ,(symbol-name system-type))
     (window_system . ,(symbol-name (or window-system 'tty))))))

(defun jotain-telemetry--session-end ()
  "Record session end and flush synchronously."
  (jotain-telemetry--enqueue
   "session_end"
   `((duration_seconds . ,(round (- (float-time) jotain-telemetry--session-start)))
     (buffers_created . ,jotain-telemetry--buffers-created)
     (commands_executed . ,jotain-telemetry--command-total)))
  (jotain-telemetry--send-batch t))

;; --- Command tracking ---

(defun jotain-telemetry--track-command ()
  "Increment counter for `this-command'."
  (when (and this-command (symbolp this-command))
    (cl-incf (gethash this-command jotain-telemetry--command-counts 0))
    (cl-incf jotain-telemetry--command-total)))

(defun jotain-telemetry--flush-command-counts ()
  "Enqueue command frequency event and reset counters."
  (when (and jotain-telemetry--command-counts
             (> (hash-table-count jotain-telemetry--command-counts) 0))
    (let ((sorted nil))
      (maphash (lambda (cmd count) (push (cons (symbol-name cmd) count) sorted))
               jotain-telemetry--command-counts)
      (setq sorted (seq-take (sort sorted (lambda (a b) (> (cdr a) (cdr b)))) 20))
      (jotain-telemetry--enqueue
       "command_frequency"
       `((commands . ,sorted)
         (total . ,jotain-telemetry--command-total))))
    (clrhash jotain-telemetry--command-counts)))

;; --- Major mode tracking ---

(defun jotain-telemetry--track-mode-change ()
  "Record time spent in the previous major mode."
  (let ((now (float-time)))
    (when (and jotain-telemetry--current-mode jotain-telemetry--last-mode-change)
      (let ((elapsed (- now jotain-telemetry--last-mode-change)))
        (cl-incf (gethash jotain-telemetry--current-mode
                          jotain-telemetry--mode-times 0.0)
                 elapsed)))
    (setq jotain-telemetry--current-mode major-mode
          jotain-telemetry--last-mode-change now)))

(defun jotain-telemetry--flush-mode-times ()
  "Enqueue major_mode_time events and reset accumulator."
  ;; Account for time in the current mode up to now.
  (when (and jotain-telemetry--current-mode jotain-telemetry--last-mode-change)
    (let ((now (float-time)))
      (cl-incf (gethash jotain-telemetry--current-mode
                        jotain-telemetry--mode-times 0.0)
               (- now jotain-telemetry--last-mode-change))
      (setq jotain-telemetry--last-mode-change now)))
  (when (and jotain-telemetry--mode-times
             (> (hash-table-count jotain-telemetry--mode-times) 0))
    (maphash (lambda (mode secs)
               (when (> secs 0)
                 (jotain-telemetry--enqueue
                  "major_mode_time"
                  `((mode . ,(symbol-name mode))
                    (seconds . ,(round secs))))))
             jotain-telemetry--mode-times)
    (clrhash jotain-telemetry--mode-times)))

;; --- Package tracking ---

(defun jotain-telemetry--report-packages ()
  "Enqueue a package_loaded event with currently loaded packages."
  (when (jotain-telemetry--active-p)
    (let ((pkgs (mapcar #'symbol-name
                        (or (bound-and-true-p package-activated-list)
                            (delq nil (mapcar (lambda (f)
                                                (when (symbolp f) f))
                                              features))))))
      (jotain-telemetry--enqueue
       "packages_loaded"
       `((packages . ,(vconcat pkgs))
         (count . ,(length pkgs)))))))

;; --- Error tracking ---

(defun jotain-telemetry--track-error (data context caller)
  "Track an error event.  DATA is the error data, CONTEXT and CALLER are from Emacs.
Debounced to at most once per 10 seconds."
  (let ((now (float-time)))
    (when (> (- now jotain-telemetry--last-error-time) 10)
      (setq jotain-telemetry--last-error-time now)
      (jotain-telemetry--enqueue
       "error_occurred"
       `((error_type . ,(symbol-name (car data)))
         (error_message . ,(error-message-string data))
         (command . ,(if (and this-command (symbolp this-command))
                         (symbol-name this-command)
                       "unknown"))
         (major_mode . ,(symbol-name major-mode))
         (context . ,(format "%s" context)))))))

;; --- Buffer tracking ---

(defun jotain-telemetry--track-buffer-create ()
  "Increment buffer creation counter when a new buffer actually appears."
  (let ((current (length (buffer-list))))
    (when (> current jotain-telemetry--known-buffer-count)
      (cl-incf jotain-telemetry--buffers-created
               (- current jotain-telemetry--known-buffer-count)))
    (setq jotain-telemetry--known-buffer-count current)))

;;;; Minor mode

(defun jotain-telemetry--setup ()
  "Initialize telemetry state and hooks."
  (setq jotain-telemetry--session-id (jotain-telemetry--generate-uuid)
        jotain-telemetry--distinct-id (jotain-telemetry--get-distinct-id)
        jotain-telemetry--session-start (float-time)
        jotain-telemetry--command-counts (make-hash-table :test #'eq)
        jotain-telemetry--mode-times (make-hash-table :test #'eq)
        jotain-telemetry--command-total 0
        jotain-telemetry--buffers-created 0
        jotain-telemetry--known-buffer-count (length (buffer-list))
        jotain-telemetry--current-mode major-mode
        jotain-telemetry--last-mode-change (float-time)
        jotain-telemetry--last-error-time 0
        jotain-telemetry--queue nil)
  ;; Hooks
  (add-hook 'kill-emacs-hook #'jotain-telemetry--session-end)
  (add-hook 'post-command-hook #'jotain-telemetry--track-command)
  (add-hook 'after-change-major-mode-hook #'jotain-telemetry--track-mode-change)
  (add-hook 'buffer-list-update-hook #'jotain-telemetry--track-buffer-create)
  (setq command-error-function #'jotain-telemetry--track-error)
  ;; Periodic flush timer
  (setq jotain-telemetry--flush-timer
        (run-with-timer jotain-telemetry-flush-interval
                        jotain-telemetry-flush-interval
                        #'jotain-telemetry-flush))
  ;; Record session start
  (jotain-telemetry--session-start)
  ;; Report packages after init settles
  (run-with-idle-timer 10 nil #'jotain-telemetry--report-packages)
  (message "jotain-telemetry: active (distinct_id: %s)" jotain-telemetry--distinct-id))

(defun jotain-telemetry--teardown ()
  "Remove hooks and cancel timers."
  (remove-hook 'kill-emacs-hook #'jotain-telemetry--session-end)
  (remove-hook 'post-command-hook #'jotain-telemetry--track-command)
  (remove-hook 'after-change-major-mode-hook #'jotain-telemetry--track-mode-change)
  (remove-hook 'buffer-list-update-hook #'jotain-telemetry--track-buffer-create)
  (when (eq command-error-function #'jotain-telemetry--track-error)
    (setq command-error-function #'command-error-default-function))
  (when jotain-telemetry--flush-timer
    (cancel-timer jotain-telemetry--flush-timer)
    (setq jotain-telemetry--flush-timer nil))
  (message "jotain-telemetry: disabled"))

;;;###autoload
(define-minor-mode jotain-telemetry-mode
  "Global minor mode for PostHog telemetry."
  :global t
  :lighter " PHog"
  (if jotain-telemetry-mode
      (if (jotain-telemetry--active-p)
          (jotain-telemetry--setup)
        (message "jotain-telemetry: no API key configured, staying disabled")
        (setq jotain-telemetry-mode nil))
    (jotain-telemetry--teardown)))

(provide 'jotain-telemetry)
;;; jotain-telemetry.el ends here
