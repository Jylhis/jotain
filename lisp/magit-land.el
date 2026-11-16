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
;;   L  run `git-land' in a compilation buffer (C-u L runs `approve --yes')
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
;; The library needs `magit-section' and `transient', and magit for the status
;; section; init-review.el loads it when magit has loaded.  `git-land',
;; `git-results' and `wt' are called when a key asks for them.

;;; Code:

(require 'compile)
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

(defun magit-land--insert-status (st)
  "Insert the queue sections for the status ST."
  (let ((now (magit-land--now)))
    (magit-insert-section (land)
      (magit-insert-heading
        (format "Land queue  main %s  lane: %s"
                (magit-land--short (alist-get 'main st)) (magit-land--lane-state st)))
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

(defun magit-land--text-buffer (name args)
  "Show the output of git-land ARGS in the buffer NAME."
  (pcase-let ((`(,code ,out ,err) (apply #'magit-land--call (magit-land--dir) args)))
    (with-current-buffer (get-buffer-create name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (if (zerop code)
                    out
                  (format "git land %s failed: %s\n" (car args)
                          (magit-land--first-line err out))))
        (goto-char (point-min))
        (special-mode))
      (display-buffer (current-buffer)))))

(defun magit-land-add ()
  "Queue the unqueued branch at point and return at once."
  (interactive)
  (magit-land--run "add" "--detach" (plist-get (magit-land--need 'branch) :ref)))

(defun magit-land-approve ()
  "Show the gates of the held set at point and, after a prompt, approve it."
  (interactive)
  (let* ((ref (plist-get (magit-land--need 'set 'item) :set))
         (res (magit-land--call (magit-land--dir) "show" "--json" ref))
         (set (and (zerop (car res))
                   (alist-get 'set (magit-land--parse (cadr res))))))
    (unless set
      (user-error "Cannot read set %s: %s" ref
                  (magit-land--first-line (nth 2 res) (nth 1 res))))
    (unless (equal (alist-get 'state set) "held")
      (user-error "Set %s is not held (%s)" ref (alist-get 'state set)))
    (when (y-or-n-p (format "Approve #%s %s, gates: %s, %s? " ref
                            (string-join (mapcar (lambda (i) (alist-get 'branch i))
                                                 (alist-get 'items set))
                                         ", ")
                            (string-join (alist-get 'gates set) ", ")
                            (if (alist-get 'checked set) "checks green" "not checked")))
      (magit-land--run "approve" "--yes" ref))))

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

(defun magit-land-resume ()
  "Start the worker again."
  (interactive)
  (magit-land--run "resume"))

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
With prefix argument YES, approve the held change of the branch instead."
  (interactive "P")
  (let ((default-directory magit-land--root))
    (compile (if yes
                 (concat magit-land-land-command " approve --yes "
                         (shell-quote-argument
                          (or (magit-land--git magit-land--root "symbolic-ref"
                                               "--short" "HEAD")
                              (user-error "No branch to approve"))))
               magit-land-land-command))))

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
