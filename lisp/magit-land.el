;;; magit-land.el --- Summary buffer for landing a task branch -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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

;; `magit-land' opens a buffer for the current branch against its base
;; (default origin/main).  It lists one line per commit and, under each,
;; the kinds of check result in refs/notes/checks and the text of the
;; note in refs/notes/review.  Keys:
;;
;;   L  run `git-land' in a compilation buffer (C-u L reviews the held change of
;;      the branch, asks `yes-or-no-p', then approves that set and that patch id in a
;;      compilation buffer; the review is the gates, the files, the diffstat and
;;      the full diff, see below)
;;   S  show the check results of the commit at point (`git-results show')
;;   D  remove the worktree with `wt remove', after a y-or-n-p
;;   g  refresh
;;
;; The library also adds a "Land queue" section to the magit status buffer
;; (`magit-land-insert-queue').  It comes from one call of
;; `git land status --all --json': the lane sets with their step and
;; elapsed time, the held and blocked sets, then the branches that are not
;; queued.  When the daemon is down or too old, the section is one line
;; that says why.  `&' on a set, an item or a branch opens
;; `magit-land-dispatch' (add, approve, reject, remove, retry, bump, show,
;; log, pause, resume).
;;
;; A review of a held set (`magit-land-approve' and `C-u L') runs
;; `git land approve SET' with no input.  Its stdout is the full diff of the
;; held change, and the sha256 of that diff is the patch id.  Its stderr holds
;; the gates, the changed files, the diffstat and the line "patch: ID".
;; The buffer `*magit-land review*' shows the summary and then the diff in
;; `diff-mode'.  The diff is untrusted text from an agent branch: it is read
;; as bytes, inserted as literal text and never evaluated.  The review stops
;; when the sha256 of the bytes is not the patch id.  After a yes, the
;; approval runs `git land approve --yes --patch ID SET'.
;;
;; When the daemon stopped itself because main moved outside the queue, the
;; queue section shows the reason.  `magit-land-resume' then shows the move
;; (`git land resume' with no input), asks whether to trust that main, and
;; resumes with `--yes --main SHA' for the commit that the status showed.
;;
;; The library needs `magit-section' and `transient', and magit for the status
;; section; init-review.el loads it when magit has loaded.  `git-land',
;; `git-results' and `wt' are called when a key asks for them.

;;; Code:

(require 'compile)
(require 'diff-mode)
(require 'subr-x)
(require 'cl-lib)
(require 'seq)
(require 'magit-section)
(require 'transient)

(declare-function magit-refresh "magit-mode")
(defvar magit-status-sections-hook)

(defgroup magit-land nil
  "Summary buffer for landing a task branch."
  :group 'tools)

(defcustom magit-land-base "origin/main"
  "The ref the branch lands on."
  :type 'string)

(defcustom magit-land-land-command "git-land"
  "The program that lands the branch."
  :type 'string)

(defcustom magit-land-results-command "git-results"
  "The program that shows stored check results."
  :type 'string)

(defcustom magit-land-wt-command "wt"
  "The worktrunk program."
  :type 'string)

(defcustom magit-land-status-section t
  "Whether the magit status buffer shows the land queue."
  :type 'boolean)

(defcustom magit-land-log-count 20
  "How many sets `magit-land-log' asks for."
  :type 'integer)

(defvar-local magit-land--root nil
  "The top-level directory of the repository the buffer shows.")

;;;; The queue section of the magit status buffer

(defun magit-land--call (root &rest args)
  "Run `magit-land-land-command' with ARGS in ROOT.
Return (EXIT STDOUT STDERR).  EXIT is -1 when the program did not start."
  (let ((default-directory root)
        (errfile (make-temp-file "magit-land-err")))
    (unwind-protect
        (with-temp-buffer
          (let* ((err nil)
                 (code (condition-case e
                           (apply #'process-file magit-land-land-command
                                  nil (list t errfile) nil args)
                         (error (setq err (error-message-string e)) -1))))
            (list code (buffer-string)
                  (or err (with-temp-buffer
                            (insert-file-contents errfile)
                            (buffer-string))))))
      (delete-file errfile))))

(defun magit-land--call-raw (root &rest args)
  "Run `magit-land-land-command' with ARGS in ROOT, without decoding.
Return (EXIT STDOUT STDERR).  STDOUT is a unibyte string of the exact bytes
that the program wrote.  STDERR is decoded as UTF-8.  EXIT is -1 when the
program did not start."
  (let ((default-directory root)
        (errfile (make-temp-file "magit-land-err"))
        (coding-system-for-read 'no-conversion)
        (coding-system-for-write 'no-conversion))
    (unwind-protect
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (let* ((err nil)
                 (code (condition-case e
                           (apply #'process-file magit-land-land-command
                                  nil (list t errfile) nil args)
                         (error (setq err (error-message-string e)) -1)))
                 (out (buffer-string)))
            (erase-buffer)
            (insert-file-contents-literally errfile)
            (list code out (or err (decode-coding-string (buffer-string) 'utf-8-unix t)))))
      (delete-file errfile))))

(defun magit-land--first-line (&rest texts)
  "Return the first non-empty line of TEXTS, or \"no output\"."
  (or (seq-some (lambda (text)
                  (seq-find (lambda (l) (not (string-empty-p l)))
                            (mapcar #'string-trim (split-string (or text "") "\n"))))
                texts)
      "no output"))

(defun magit-land--parse (text)
  "Parse the JSON object in TEXT into an alist, or return nil."
  (condition-case nil
      (let ((obj (json-parse-string text :object-type 'alist :array-type 'list
                                    :null-object nil :false-object nil)))
        (and (consp obj) (consp (car obj)) (symbolp (caar obj)) obj))
    (error nil)))

(defun magit-land--fetch (root)
  "Return (ok . STATUS) or (error . REASON) for the queue of ROOT."
  (pcase-let ((`(,code ,out ,err) (magit-land--call root "status" "--all" "--json")))
    (cond
     ((/= code 0)
      (cons 'error (format "%s exited %d: %s" magit-land-land-command code
                           (magit-land--first-line err out))))
     ((let ((st (magit-land--parse out)))
        (and st (assq 'main st) (cons 'ok st))))
     (t (cons 'error (format "%s sent no queue status (old daemon?)"
                             magit-land-land-command))))))

(defun magit-land--now ()
  "Return the current time in seconds."
  (float-time))

(defun magit-land--short (sha)
  "Return the first 7 characters of SHA."
  (substring (or sha "") 0 (min 7 (length (or sha "")))))

(defun magit-land--duration (secs)
  "Format SECS as 45s, 2m10s or 1h05m."
  (let ((s (max 0 (round secs))))
    (cond ((< s 60) (format "%ds" s))
          ((< s 3600) (format "%dm%02ds" (/ s 60) (% s 60)))
          (t (format "%dh%02dm" (/ s 3600) (/ (% s 3600) 60))))))

(defun magit-land--plural (n)
  "Return \"N commit\" or \"N commits\"."
  (format "%d commit%s" n (if (= n 1) "" "s")))

(defun magit-land--state-word (state reason)
  "Return STATE, with REASON when the state is returned."
  (if (and (equal state "returned") (not (member reason '(nil ""))))
      (concat state " " reason)
    state))

(defun magit-land--state-face (state)
  "Return the face for the set or item STATE."
  (pcase state
    ("landed" 'success)
    ("returned" 'error)
    ((or "held" "blocked") 'warning)
    ((or "rebasing" "checking" "landing") 'bold)))

(defun magit-land--state-text (state reason)
  "Return the state word of STATE and REASON with its face."
  (let ((face (magit-land--state-face state))
        (word (magit-land--state-word state reason)))
    (if face (propertize word 'face face) word)))

(defun magit-land--lane-state (st)
  "Return the state of the lane in status ST."
  (let ((lane (alist-get 'lane st)))
    (cond ((alist-get 'paused st) "paused")
          ((null lane) "empty")
          ((seq-some (lambda (s)
                       (member (alist-get 'state s) '("rebasing" "checking" "landing")))
                     lane)
           "running")
          (t "idle"))))

(defun magit-land--set-detail (set now)
  "Return what the worker does with SET at time NOW."
  (let ((step (alist-get 'step set))
        (since (alist-get 'step_since set)))
    (cond ((and step since)
           (format "%s (%s)" step (magit-land--duration (- now since))))
          (step step)
          ((cdr (alist-get 'items set)) "stack")
          (t (or (alist-get 'detail set) "")))))

(defun magit-land--parked-note (set now)
  "Return the note after the head of the parked SET at time NOW."
  (let ((id (alist-get 'id set)))
    (if (equal (alist-get 'state set) "blocked")
        (string-trim
         (concat (alist-get 'detail set) "  "
                 (let ((at (alist-get 'retry_at set)))
                   (cond ((null at) "")
                         ((<= at now) "retrying now")
                         (t (concat "retry in " (magit-land--duration (- at now))))))))
      (format "%s, waiting for: git land approve %d"
              (if (alist-get 'checked set) "green" "not checked") id))))

(defun magit-land--set-head (set now &optional place)
  "Return the heading line of SET at time NOW.
PLACE is the 1-based lane position, or nil for a parked set."
  (let* ((state (alist-get 'state set))
         (gates (alist-get 'gates set))
         (word (magit-land--state-text state (alist-get 'reason set))))
    (if place
        (format "%d #%d %s  %s" place (alist-get 'id set) word
                (magit-land--set-detail set now))
      (format "#%d %s%s  %s" (alist-get 'id set) word
              (if (and (equal state "held") gates)
                  (concat " " (string-join gates ", "))
                "")
              (magit-land--parked-note set now)))))

(defun magit-land--item-note (item)
  "Return the note after the branch name of ITEM."
  (let ((state (alist-get 'state item)))
    (cond ((equal state "landed")
           (concat "landed " (magit-land--short (alist-get 'landed item))))
          ((equal state "returned")
           (magit-land--state-word state (alist-get 'reason item)))
          (t (magit-land--plural (or (alist-get 'commits item) 0))))))

(defun magit-land--width (st)
  "Return the width of the branch column for status ST."
  (1+ (apply #'max 26
             (mapcar (lambda (it) (length (alist-get 'branch it)))
                     (mapcan (lambda (s) (copy-sequence (alist-get 'items s)))
                             (append (alist-get 'lane st) (alist-get 'parked st)))))))

(defun magit-land--insert-set (set st now &optional place)
  "Insert the section of SET of status ST at time NOW.
PLACE is the lane position, or nil for a parked set."
  (magit-insert-section (land-set (alist-get 'id set))
    (magit-insert-heading (magit-land--set-head set now place))
    (dolist (it (alist-get 'items set))
      (magit-insert-section (land-item (alist-get 'branch it))
        (insert "  "
                (propertize (string-pad (alist-get 'branch it) (magit-land--width st))
                            'face 'magit-branch-local)
                (let ((state (alist-get 'state it)))
                  (if (member state '("landed" "returned"))
                      (propertize (magit-land--item-note it)
                                  'face (magit-land--state-face state))
                    (magit-land--item-note it)))
                "\n")))))

(defun magit-land--insert-parked (st now)
  "Insert the held and blocked sets of status ST at time NOW."
  (dolist (state '("held" "blocked"))
    (when-let* ((sets (seq-filter (lambda (s) (equal (alist-get 'state s) state))
                                  (alist-get 'parked st))))
      (magit-insert-section (land-parked state)
        (magit-insert-heading (format "%s (%d)" (capitalize state) (length sets)))
        (dolist (s sets)
          (magit-land--insert-set s st now))))))

(defun magit-land--insert-unqueued (st)
  "Insert the branches of status ST that are not queued."
  (when-let* ((bs (alist-get 'unqueued st)))
    (let ((nw (apply #'max (mapcar (lambda (b) (length (alist-get 'name b))) bs)))
          (cw (apply #'max (mapcar (lambda (b) (length (magit-land--plural
                                                        (alist-get 'commits b))))
                                   bs))))
      (magit-insert-section (land-unqueued)
        (magit-insert-heading (format "Not queued (%d)" (length bs)))
        (dolist (b bs)
          (magit-insert-section (land-branch (alist-get 'name b))
            (insert "  "
                    (propertize (string-pad (alist-get 'name b) nw)
                                'face 'magit-branch-local)
                    "  " (propertize (magit-land--short (alist-get 'sha b))
                                     'face 'magit-hash)
                    "  " (string-pad (magit-land--plural (alist-get 'commits b)) cw)
                    "  " (alist-get 'state b) "\n")))))))

(defun magit-land--plain (text)
  "Return TEXT without control characters."
  (replace-regexp-in-string "[[:cntrl:]\u0080-\u009f]" "" text))

(defun magit-land--insert-stopped (st)
  "Insert the lines that say why the daemon of status ST stopped itself."
  (when-let* ((reason (alist-get 'pause_reason st))
              ((not (string-empty-p reason))))
    (insert (propertize (concat "Stopped: " (magit-land--plain reason)) 'face 'warning)
            "\n"
            "  resume with "
            (propertize "P" 'face 'help-key-binding)
            " in the menu (&), after a review of the move\n")))

(defun magit-land--insert-status (st)
  "Insert the queue sections for the status ST."
  (let ((now (magit-land--now)))
    (magit-insert-section (land)
      (magit-insert-heading
        (format "Land queue  main %s  lane: %s"
                (magit-land--short (alist-get 'main st)) (magit-land--lane-state st)))
      (magit-land--insert-stopped st)
      (let ((place 0))
        (dolist (s (alist-get 'lane st))
          (magit-land--insert-set s st now (cl-incf place))))
      (magit-land--insert-parked st now)
      (magit-land--insert-unqueued st))))

;;;###autoload
(defun magit-land-insert-queue ()
  "Insert the land queue into the magit status buffer.
Any failure becomes a single line instead of an error."
  (condition-case err
      (let ((res (magit-land--fetch default-directory)))
        (if (eq (car res) 'ok)
            (magit-land--insert-status (cdr res))
          (magit-land--insert-unavailable (cdr res))))
    (error (magit-land--insert-unavailable (error-message-string err))))
  (insert "\n"))

(defun magit-land--insert-unavailable (reason)
  "Insert a one-line section that says why there is no queue: REASON."
  (magit-insert-section (land)
    (magit-insert-heading
      (format "Land queue unavailable: %s"
              (replace-regexp-in-string "[\n\t ]+" " " reason)))))

;;;###autoload
(with-eval-after-load 'magit-status
  (when magit-land-status-section
    (add-hook 'magit-status-sections-hook #'magit-land-insert-queue t)))

;;;; Commands on the section at point

(defun magit-land--target ()
  "Return the plist (:kind KIND :ref REF :set ID) for the section at point.
KIND is `set', `item' or `branch'.  REF is a set id string or a branch
name, ID the set id (nil for an unqueued branch)."
  (when-let* ((s (magit-current-section)))
    (pcase (oref s type)
      ('land-set (list :kind 'set :ref (number-to-string (oref s value))
                       :set (number-to-string (oref s value))))
      ('land-item (list :kind 'item :ref (oref s value)
                        :set (number-to-string (oref (oref s parent) value))))
      ('land-branch (list :kind 'branch :ref (oref s value))))))

(defun magit-land--need (&rest kinds)
  "Return the target at point, which must be of one of KINDS."
  (let ((target (magit-land--target)))
    (unless (and target (memq (plist-get target :kind) kinds))
      (user-error "Put point on %s" (string-join (mapcar #'symbol-name kinds) " or ")))
    target))

(defun magit-land--dir ()
  "Return the repository directory for a call."
  (or magit-land--root default-directory))

(defun magit-land--refresh ()
  "Refresh the magit buffer, if this is one."
  (when (and (derived-mode-p 'magit-mode) (fboundp 'magit-refresh))
    (magit-refresh)))

(defun magit-land--run (&rest args)
  "Run git-land with ARGS, say its answer and refresh.  Signal when it fails."
  (pcase-let ((`(,code ,out ,err) (apply #'magit-land--call (magit-land--dir) args)))
    (magit-land--refresh)
    (unless (zerop code)
      (user-error "git land %s failed: %s" (car args) (magit-land--first-line err out)))
    (message "git land %s: %s" (car args) (magit-land--first-line out))))

(defun magit-land--show-text (name text)
  "Show TEXT as literal text in the read-only buffer NAME."
  (with-current-buffer (get-buffer-create name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer))))

(defun magit-land--text-buffer (name args)
  "Show the output of git-land ARGS in the buffer NAME."
  (pcase-let ((`(,code ,out ,err) (apply #'magit-land--call (magit-land--dir) args)))
    (magit-land--show-text name (if (zerop code)
                                    out
                                  (format "git land %s failed: %s\n" (car args)
                                          (magit-land--first-line err out))))))

(defun magit-land-add ()
  "Queue the unqueued branch at point and return at once."
  (interactive)
  (magit-land--run "add" "--detach" (plist-get (magit-land--need 'branch) :ref)))

(defun magit-land--held-set (ref)
  "Return the held set that REF (a set id or a branch) names, or signal."
  (let* ((res (magit-land--call (magit-land--dir) "show" "--json" ref))
         (set (and (zerop (car res))
                   (alist-get 'set (magit-land--parse (cadr res))))))
    (unless set
      (user-error "Cannot read set %s: %s" ref
                  (magit-land--first-line (nth 2 res) (nth 1 res))))
    (unless (equal (alist-get 'state set) "held")
      (user-error "%s has no held set (%s)" ref (alist-get 'state set)))
    set))

(defvar magit-land-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'diff-hunk-next)
    (define-key map "p" #'diff-hunk-prev)
    (define-key map "N" #'diff-file-next)
    (define-key map "P" #'diff-file-prev)
    (define-key map "}" #'diff-file-next)
    (define-key map "{" #'diff-file-prev)
    (define-key map (kbd "TAB") #'diff-hunk-next)
    (define-key map (kbd "<backtab>") #'diff-hunk-prev)
    (define-key map " " #'scroll-up-command)
    (define-key map (kbd "DEL") #'scroll-down-command)
    (define-key map "<" #'beginning-of-buffer)
    (define-key map ">" #'end-of-buffer)
    (define-key map "q" #'quit-window)
    (define-key map [remap read-only-mode] #'ignore)
    (define-key map [remap toggle-read-only] #'ignore)
    (define-key map [remap next-error] #'ignore)
    (define-key map [remap previous-error] #'ignore)
    map)
  "Keymap of `magit-land-review-mode', an allow-list with no parent.
The map of `diff-mode' has commands that visit, apply, test, reverse or edit
the files that the diff names.  The diff is text from an agent, so none of
them may be reachable from this buffer.")

(define-derived-mode magit-land-review-mode diff-mode "Land-Review"
  "Read-only `diff-mode' to review a held set before the owner approves it.
The diff names files that an agent chose.  This mode keeps the fontification
of `diff-mode' and drops every command and setting that reads, visits or
changes those files.

\\{magit-land-review-mode-map}"
  (set-keymap-parent magit-land-review-mode-map nil)
  (use-local-map magit-land-review-mode-map)
  (setq-local diff-font-lock-syntax nil
              diff-font-lock-prettify nil
              diff-mode-read-only nil
              read-only-mode-hook nil
              next-error-function #'ignore
              add-log-current-defun-function #'ignore
              add-log-buffer-file-name-function #'ignore))

(defun magit-land--show-review (id summary diff)
  "Show the set ID in `*magit-land review*': SUMMARY, then DIFF.
DIFF is a unibyte string.  Both texts go in as literal text, in
`magit-land-review-mode'.  The buffer is read-only, no file-local variable
is evaluated, and `default-directory' is not the repository."
  (with-current-buffer (get-buffer-create "*magit-land review*")
    (let ((inhibit-read-only t)
          (enable-local-variables nil)
          (enable-dir-local-variables nil))
      (setq default-directory (file-name-as-directory (expand-file-name "/")))
      (erase-buffer)
      (magit-land-review-mode)
      (insert (format "Set #%s\n\n" id) summary
              (if (string-suffix-p "\n" summary) "" "\n")
              "\n" (decode-coding-string diff 'utf-8-unix t))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (display-buffer (current-buffer))))

(defun magit-land--review (id)
  "Show the review of the held set ID in a buffer and return its patch id.
Run `git land approve ID' with no input and without `--yes'.  The command
answers its own question with no, so this approves nothing.  Its stdout is
the full diff of the held change, and its stderr is the gates, the changed
files, the diffstat and the line \"patch: ID\".  The buffer shows both.
Signal a `user-error' when the review has no gates or no patch line, when
the diff is empty, or when the sha256 of the diff bytes is not the patch id."
  (pcase-let ((`(,_code ,out ,err) (magit-land--call-raw (magit-land--dir) "approve" id)))
    (unless (string-match-p "^gates: " err)
      (user-error "Cannot review set %s: %s" id (magit-land--first-line err out)))
    (unless (string-match "^patch: \\([0-9a-f]\\{64\\}\\)$" err)
      (user-error "The review of set %s has no patch id" id))
    (let ((patch (match-string 1 err))
          (bytes (if (multibyte-string-p out) (encode-coding-string out 'utf-8-unix) out)))
      (when (string-empty-p bytes)
        (user-error "The review of set %s has an empty diff" id))
      (unless (equal (secure-hash 'sha256 bytes) patch)
        (user-error "The diff of set %s does not match its patch id; not shown" id))
      (magit-land--show-review id err bytes)
      patch)))

(defun magit-land--confirm-approval (ref)
  "Review the held set that REF names and ask the owner to approve it.
Return (ID . PATCH) after a yes, else nil: ID is the set id as a string and
PATCH the id of the change that the owner saw.  Signal a `user-error' when REF
has no held set or the review has no patch id.  The caller approves that set
with that patch id, never REF."
  (let* ((set (magit-land--held-set ref))
         (id (number-to-string (alist-get 'id set)))
         (patch (magit-land--review id)))
    (when (yes-or-no-p (format "Approve #%s %s, gates: %s, %s? " id
                               (string-join (mapcar (lambda (i) (alist-get 'branch i))
                                                    (alist-get 'items set))
                                            ", ")
                               (string-join (alist-get 'gates set) ", ")
                               (if (alist-get 'checked set) "checks green" "not checked")))
      (cons id patch))))

(defun magit-land-approve ()
  "Show the review of the held set at point and, after a yes, approve it.
The review is the gates, the changed files, the diffstat and the full diff.
The approval carries the patch id of the diff that the review showed."
  (interactive)
  (when-let* ((ok (magit-land--confirm-approval
                   (plist-get (magit-land--need 'set 'item) :set))))
    (magit-land--run "approve" "--yes" "--patch" (cdr ok) (car ok))))

(defun magit-land-reject ()
  "Return the held set at point, after a prompt."
  (interactive)
  (let ((ref (plist-get (magit-land--need 'set 'item) :set)))
    (when (y-or-n-p (format "Reject set #%s? " ref))
      (magit-land--run "reject" ref))))

(defun magit-land-remove ()
  "Take the set or the branch at point out of the queue, after a prompt."
  (interactive)
  (let ((ref (plist-get (magit-land--need 'set 'item) :ref)))
    (when (y-or-n-p (format "Remove %s from the queue? "
                            (if (string-match-p "\\`[0-9]+\\'" ref) (concat "#" ref) ref)))
      (magit-land--run "remove" ref))))

(defun magit-land-retry ()
  "Queue the returned or blocked set at point again."
  (interactive)
  (magit-land--run "retry" (plist-get (magit-land--need 'set 'item) :ref)))

(defun magit-land-bump ()
  "Move the set at point to the front of the lane."
  (interactive)
  (magit-land--run "bump" (plist-get (magit-land--need 'set 'item) :set)))

(defun magit-land-pause ()
  "Stop the worker after its current step."
  (interactive)
  (magit-land--run "pause"))

(defun magit-land--stop-state ()
  "Return the status alist when the daemon stopped itself, else nil."
  (let ((res (magit-land--fetch (magit-land--dir))))
    (when (eq (car res) 'ok)
      (let ((reason (alist-get 'pause_reason (cdr res))))
        (and (stringp reason) (not (string-empty-p reason)) (cdr res))))))

(defun magit-land--resume-after-stop (st)
  "Show the move that stopped the daemon of status ST and resume after a yes.
Run `git land resume' with no input to get the move, ask which main to trust
and resume with `--yes --main' and the full commit of the status."
  (let ((main (alist-get 'trip_main st)))
    (unless (and (stringp main) (string-match-p "\\`[0-9a-f]\\{40,64\\}\\'" main))
      (user-error "The status has no commit for the stop; run: git land resume"))
    (pcase-let ((`(,code ,out ,err) (magit-land--call (magit-land--dir) "resume")))
      (when (zerop code)
        (magit-land--refresh)
        (user-error "The daemon resumed without a review"))
      (when (string-empty-p out)
        (user-error "git land resume failed: %s" (magit-land--first-line err out)))
      (magit-land--show-text "*magit-land resume*" out))
    (when (yes-or-no-p (format "Trust main at %s after: %s? " (magit-land--short main)
                               (magit-land--plain (alist-get 'pause_reason st))))
      (magit-land--run "resume" "--yes" "--main" main))))

(defun magit-land-resume ()
  "Start the worker again.
When the daemon stopped itself because main moved outside the queue, show
the move first, ask whether to trust that main, and resume only after a yes
with the commit that the status named.  Otherwise resume at once."
  (interactive)
  (if-let* ((st (magit-land--stop-state)))
      (magit-land--resume-after-stop st)
    (magit-land--run "resume")))

(defun magit-land-show ()
  "Show the set or branch at point with `git land show'."
  (interactive)
  (let ((ref (plist-get (magit-land--need 'set 'item 'branch) :ref)))
    (magit-land--text-buffer "*magit-land show*" (list "show" ref))))

(defun magit-land-log ()
  "Show the landed and returned sets with `git land log'."
  (interactive)
  (magit-land--text-buffer "*magit-land log*"
                           (list "log" "-n" (number-to-string magit-land-log-count))))

(transient-define-prefix magit-land-dispatch ()
  "Act on the land queue section at point."
  [["Queue"
    ("a" "Add branch" magit-land-add)
    ("k" "Remove" magit-land-remove)
    ("R" "Retry" magit-land-retry)]
   ["Owner"
    ("A" "Approve" magit-land-approve)
    ("r" "Reject" magit-land-reject)
    ("b" "Bump to front" magit-land-bump)
    ("p" "Pause" magit-land-pause)
    ("P" "Resume" magit-land-resume)]
   ["Views"
    ("s" "Show" magit-land-show)
    ("l" "Log" magit-land-log)]])

(defvar-keymap magit-land-section-map
  :doc "Keys of the land queue sections."
  "&" #'magit-land-dispatch)

(defvar magit-land-set-section-map magit-land-section-map)
(defvar magit-land-item-section-map magit-land-section-map)
(defvar magit-land-branch-section-map magit-land-section-map)

;;;; The summary buffer of one branch

(defun magit-land--git (root &rest args)
  "Run git with ARGS in ROOT and return its output, or nil on failure."
  (let ((default-directory root))
    (with-temp-buffer
      (when (zerop (apply #'process-file "git" nil t nil args))
        (string-trim-right (buffer-string))))))

(defun magit-land--commits (root)
  "Return (HASH . SUBJECT) for each commit of BASE..HEAD in ROOT, newest first."
  (when-let* ((out (magit-land--git root "log" "--format=%h%x09%s"
                                    (concat magit-land-base "..HEAD"))))
    (mapcar (lambda (line)
              (let ((parts (split-string line "\t")))
                (cons (car parts) (string-join (cdr parts) "\t"))))
            (split-string out "\n" t))))

(defun magit-land--check-kinds (root hash)
  "Return the result kinds stored in refs/notes/checks on HASH in ROOT."
  (when-let* ((note (magit-land--git root "notes" "--ref" "checks" "show" hash)))
    (let (kinds)
      (dolist (line (split-string note "\n" t))
        (when (string-match "\\`\\([a-z]+\\): " line)
          (push (match-string 1 line) kinds)))
      (sort kinds #'string<))))

(defun magit-land--review-note (root hash)
  "Return the text of refs/notes/review on HASH in ROOT, or nil."
  (magit-land--git root "notes" "--ref" "review" "show" hash))

(defun magit-land--insert-commit (root hash subject)
  "Insert the block for commit HASH with SUBJECT of repository ROOT."
  (let ((start (point))
        (kinds (magit-land--check-kinds root hash))
        (review (magit-land--review-note root hash)))
    (insert (propertize hash 'face 'font-lock-constant-face) " " subject "\n")
    (insert "    checks: " (if kinds (string-join kinds " ") "none") "\n")
    (when review
      (insert "    review: "
              (replace-regexp-in-string "\n" "\n            " review)
              "\n"))
    (put-text-property start (point) 'magit-land-commit hash)))

(defun magit-land--render ()
  "Fill the current buffer from `magit-land--root'."
  (let* ((root magit-land--root)
         (branch (or (magit-land--git root "symbolic-ref" "--short" "HEAD")
                     "(detached)"))
         (commits (magit-land--commits root))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (format "Branch %s against %s\n\n" branch magit-land-base))
    (cond
     ((null (magit-land--git root "rev-parse" "--verify" "--quiet"
                             magit-land-base))
      (insert (format "No ref %s.\n" magit-land-base)))
     ((null commits) (insert "No commits to land.\n"))
     (t (insert (format "%d commit%s to land\n\n" (length commits)
                        (if (cdr commits) "s" "")))
        (dolist (c commits)
          (magit-land--insert-commit root (car c) (cdr c)))))
    (goto-char (point-min))))

(defun magit-land--commit-at-point ()
  "Return the hash of the commit block at point, or nil."
  (get-text-property (point) 'magit-land-commit))

(defun magit-land-refresh ()
  "Redraw the summary buffer."
  (interactive)
  (let ((line (line-number-at-pos)))
    (magit-land--render)
    (forward-line (1- line))))

(defun magit-land-land (&optional yes)
  "Run `git-land' for the branch in a compilation buffer.
With prefix argument YES, show the review of the held set of the branch
\(gates, files, diffstat, full diff), ask with `yes-or-no-p' and approve that
set with the patch id of the reviewed diff."
  (interactive "P")
  (let ((default-directory magit-land--root))
    (if yes
        (when-let* ((ok (magit-land--confirm-approval
                         (or (magit-land--git magit-land--root "symbolic-ref"
                                              "--short" "HEAD")
                             (user-error "No branch to approve")))))
          (compile (concat magit-land-land-command " approve --yes --patch "
                           (cdr ok) " " (car ok))))
      (compile magit-land-land-command))))

(defun magit-land-show-results ()
  "Show the check results of the commit at point."
  (interactive)
  (let ((hash (or (magit-land--commit-at-point)
                  (user-error "No commit at point")))
        (root magit-land--root)
        (buf (get-buffer-create "*magit-land results*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (default-directory root))
        (erase-buffer)
        (if (zerop (process-file magit-land-results-command nil t nil
                                 "show" hash))
            (special-mode)
          (insert (format "No check results on %s.\n" hash))
          (special-mode))))
    (display-buffer buf)))

(defun magit-land-remove-worktree ()
  "Remove the worktree of the repository with `wt remove', after a prompt."
  (interactive)
  (let ((root magit-land--root))
    (when (y-or-n-p (format "Remove the worktree %s? " root))
      (let ((default-directory (file-name-as-directory
                                (expand-file-name "~"))))
        (if (zerop (process-file magit-land-wt-command nil nil nil
                                 "-C" root "remove" root "--yes" "--foreground"))
            (progn (message "Removed %s" root)
                   (kill-buffer (current-buffer)))
          (user-error "wt remove failed for %s" root))))))

(defvar-keymap magit-land-mode-map
  :doc "Keymap for `magit-land-mode'."
  :parent special-mode-map
  "L" #'magit-land-land
  "S" #'magit-land-show-results
  "D" #'magit-land-remove-worktree
  "g" #'magit-land-refresh)

(define-derived-mode magit-land-mode special-mode "Land"
  "Major mode of the `magit-land' summary buffer.

\\{magit-land-mode-map}"
  (setq-local revert-buffer-function (lambda (&rest _) (magit-land-refresh))))

;;;###autoload
(defun magit-land ()
  "Show the commits of the current branch that are not on the base yet."
  (interactive)
  (let* ((dir (or (locate-dominating-file default-directory ".git")
                  (user-error "Not inside a git repository")))
         (root (file-name-as-directory (expand-file-name dir)))
         (buf (get-buffer-create (format "*magit-land: %s*"
                                         (file-name-nondirectory
                                          (directory-file-name root))))))
    (with-current-buffer buf
      (magit-land-mode)
      (setq default-directory root
            magit-land--root root)
      (magit-land--render))
    (pop-to-buffer-same-window buf)))

(provide 'magit-land)
;;; magit-land.el ends here
