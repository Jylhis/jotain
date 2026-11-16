;;; magit-land-test.el --- ERT tests for magit-land -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-land)

(defmacro magit-land-test--with-repo (&rest body)
  "Run BODY in a repository with two commits above origin/main.
Binds `root'.  The first task commit carries a junit result and a
review note."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "magit-land-" t)))
          (default-directory root)
          (process-environment
           (append '("GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t"
                     "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@t"
                     "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_SYSTEM=/dev/null")
                   process-environment)))
     (unwind-protect
         (progn
           (dolist (args '(("init" "-q" "-b" "main")
                           ("commit" "-q" "--allow-empty" "-m" "base")
                           ("update-ref" "refs/remotes/origin/main" "HEAD")
                           ("commit" "-q" "--allow-empty" "-m" "first task")
                           ("notes" "--ref" "checks" "add" "-m" "junit: eA==")
                           ("notes" "--ref" "review" "add" "-m" "check the retry path")
                           ("commit" "-q" "--allow-empty" "-m" "second task")))
             (apply #'call-process "git" nil nil nil args))
           ,@body)
       (when (get-buffer (format "*magit-land: %s*"
                                 (file-name-nondirectory
                                  (directory-file-name root))))
         (kill-buffer (format "*magit-land: %s*"
                              (file-name-nondirectory
                               (directory-file-name root)))))
       (delete-directory root t))))

(ert-deftest magit-land-renders-commits-and-notes ()
  (magit-land-test--with-repo
    (magit-land)
    (let ((text (buffer-string)))
      (should (string-match-p "against origin/main" text))
      (should (string-match-p "2 commits to land" text))
      (should (string-match-p "second task\n    checks: none" text))
      (should (string-match-p "first task\n    checks: junit\n    review: check the retry path"
                              text))
      (should-not (string-match-p "\\bbase\\b" text)))))

(ert-deftest magit-land-commit-at-point ()
  (magit-land-test--with-repo
    (magit-land)
    (goto-char (point-min))
    (re-search-forward "first task")
    (should (equal (magit-land--commit-at-point)
                   (string-trim (shell-command-to-string
                                 "git rev-parse --short HEAD~1"))))
    (forward-line 2)
    (should (equal (magit-land--commit-at-point)
                   (string-trim (shell-command-to-string
                                 "git rev-parse --short HEAD~1"))))))

(ert-deftest magit-land-keys ()
  (should (eq (lookup-key magit-land-mode-map "L") 'magit-land-land))
  (should (eq (lookup-key magit-land-mode-map "S") 'magit-land-show-results))
  (should (eq (lookup-key magit-land-mode-map "D") 'magit-land-remove-worktree)))

(ert-deftest magit-land-reports-a-missing-base ()
  (magit-land-test--with-repo
    (let ((magit-land-base "origin/missing"))
      (magit-land)
      (should (string-match-p "No ref origin/missing" (buffer-string))))))

;;;; The land queue section

(ert-deftest magit-land-land-commands ()
  (magit-land-test--with-repo
    (let (cmd)
      (cl-letf (((symbol-function 'compile) (lambda (c &rest _) (push c cmd))))
        (magit-land)
        (magit-land-land)
        (magit-land-land t))
      (should (equal (nreverse cmd)
                     '("git-land" "git-land approve --yes main"))))))

(defconst magit-land-test--status
  (concat
   "{\"main\":\"3b23c5e000000000000000000000000000000000\",\"paused\":false,\"lane"
   "\":[{\"id\":41,\"pos\":1,\"state\":\"checking\",\"step\":\"land-check: check-a"
   "ffected\",\"step_since\":1700000070,\"created\":1700000000,\"updated\":1700000"
   "070,\"items\":[{\"idx\":0,\"branch\":\"lane/gdorg-fold\",\"sha\":\"11111111111"
   "11111111111111111111111111111\",\"base\":\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
   "bbbbb\",\"candidate\":\"2222222222222222222222222222222222222222\",\"state\":"
   "\"checking\",\"commits\":2}]},{\"id\":42,\"pos\":2,\"state\":\"queued\",\"crea"
   "ted\":1700000010,\"updated\":1700000010,\"items\":[{\"idx\":0,\"branch\":\"fea"
   "t/parser\",\"sha\":\"3333333333333333333333333333333333333333\",\"base\":\"bbb"
   "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",\"state\":\"queued\",\"commits\":3},{"
   "\"idx\":1,\"branch\":\"feat/parser-cli\",\"sha\":\"444444444444444444444444444"
   "4444444444444\",\"base\":\"3333333333333333333333333333333333333333\",\"state"
   "\":\"queued\",\"commits\":1}]}],\"parked\":[{\"id\":40,\"pos\":3,\"state\":\"h"
   "eld\",\"gates\":[\"platform\",\"hosts\"],\"checked\":true,\"created\":16999990"
   "00,\"updated\":1699999900,\"items\":[{\"idx\":0,\"branch\":\"chore/kernel-budg"
   "et\",\"sha\":\"5555555555555555555555555555555555555555\",\"base\":\"bbbbbbbbb"
   "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",\"candidate\":\"6666666666666666666666666666"
   "666666666666\",\"state\":\"held\",\"commits\":1}]},{\"id\":39,\"pos\":4,\"stat"
   "e\":\"blocked\",\"detail\":\"the main checkout has local changes\",\"retry_at"
   "\":1700000290,\"created\":1699998000,\"updated\":1699999000,\"items\":[{\"idx"
   "\":0,\"branch\":\"fix/blocked-one\",\"sha\":\"77777777777777777777777777777777"
   "77777777\",\"base\":\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",\"state\":\"b"
   "locked\",\"commits\":1}]}],\"unqueued\":[{\"name\":\"feat/idea\",\"sha\":\"888"
   "8888888888888888888888888888888888888\",\"commits\":2,\"state\":\"ready\"},{\""
   "name\":\"wip/kernel\",\"sha\":\"9999999999999999999999999999999999999999\",\"c"
   "ommits\":1,\"state\":\"held platform\"}]}")
  "Status JSON: running set, stack, held set, blocked set, unqueued branches.")
(defconst magit-land-test--show
  (concat
   "{\"set\":{\"id\":40,\"pos\":3,\"state\":\"held\",\"gates\":[\"platform\",\"hos"
   "ts\"],\"checked\":true,\"created\":1699999000,\"updated\":1699999900,\"items\""
   ":[{\"idx\":0,\"branch\":\"chore/kernel-budget\",\"sha\":\"55555555555555555555"
   "55555555555555555555\",\"base\":\"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\","
   "\"candidate\":\"6666666666666666666666666666666666666666\",\"state\":\"held\","
   "\"commits\":1}]},\"events\":[{\"id\":1,\"set\":40,\"time\":1699999900,\"kind\""
   ":\"held\",\"text\":\"held for approval\",\"state\":\"held\"}],\"log\":\"\"}")
  "Show JSON of the held set 40.")

(defconst magit-land-test--now 1700000200.0)

(defmacro magit-land-test--with-status (json &rest body)
  "Render the status JSON in a magit-section buffer and run BODY there."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'magit-land--call)
              (lambda (&rest _) (list 0 ,json "")))
             ((symbol-function 'magit-land--now)
              (lambda () magit-land-test--now)))
     (with-temp-buffer
       (magit-section-mode)
       (let ((inhibit-read-only t)
             (magit-insert-section--current nil))
         (magit-insert-section (root)
           (magit-land-insert-queue)))
       (goto-char (point-min))
       ,@body)))

(defun magit-land-test--at (text)
  "Move point onto the line that contains TEXT and return the section there."
  (goto-char (point-min))
  (search-forward text)
  (magit-current-section))

(defvar magit-land-test--calls nil
  "The argv lists that the stubbed `magit-land--call' saw, oldest first.")

(defmacro magit-land-test--recording (reply &rest body)
  "Run BODY with `magit-land--call' stubbed to REPLY.
The argv lists go to `magit-land-test--calls'."
  (declare (indent 1) (debug t))
  `(progn
     (setq magit-land-test--calls nil)
     (cl-letf (((symbol-function 'magit-land--call)
                (lambda (_root &rest args) (push args magit-land-test--calls) ,reply))
               ((symbol-function 'magit-land--refresh) #'ignore)
               ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
               ((symbol-function 'message) #'ignore))
       ,@body)
     (setq magit-land-test--calls (nreverse magit-land-test--calls))))

(defun magit-land-test--section (type value)
  "Return a section of TYPE and VALUE; an item sits in set 41."
  (let ((s (make-instance 'magit-section)))
    (oset s type type)
    (oset s value value)
    (when (eq type 'land-item)
      (oset s parent (magit-land-test--section 'land-set 41)))
    s))

(defmacro magit-land-test--point-on (type value &rest body)
  "Run BODY with `magit-current-section' faked as a section of TYPE and VALUE."
  (declare (indent 2) (debug t))
  `(cl-letf (((symbol-function 'magit-current-section)
              (lambda ()
                (magit-land-test--section ,type ,value))))
     ,@body))

(ert-deftest magit-land-section-renders-the-queue ()
  (magit-land-test--with-status magit-land-test--status
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   (concat
                    "Land queue  main 3b23c5e  lane: running\n"
                    "1 #41 checking  land-check: check-affected (2m10s)\n"
                    "  lane/gdorg-fold            2 commits\n"
                    "2 #42 queued  stack\n"
                    "  feat/parser                3 commits\n"
                    "  feat/parser-cli            1 commit\n"
                    "Held (1)\n"
                    "#40 held platform, hosts  green, waiting for: git land approve 40\n"
                    "  chore/kernel-budget        1 commit\n"
                    "Blocked (1)\n"
                    "#39 blocked  the main checkout has local changes  retry in 1m30s\n"
                    "  fix/blocked-one            1 commit\n"
                    "Not queued (2)\n"
                    "  feat/idea   8888888  2 commits  ready\n"
                    "  wip/kernel  9999999  1 commit   held platform\n"
                    "\n")))))

(ert-deftest magit-land-section-value-at-point ()
  (magit-land-test--with-status magit-land-test--status
    (let ((set (magit-land-test--at "#42 queued")))
      (should (eq (oref set type) 'land-set))
      (should (equal (oref set value) 42)))
    (let ((item (magit-land-test--at "feat/parser-cli")))
      (should (eq (oref item type) 'land-item))
      (should (equal (oref item value) "feat/parser-cli"))
      (should (equal (oref (oref item parent) value) 42)))
    (let ((branch (magit-land-test--at "wip/kernel")))
      (should (eq (oref branch type) 'land-branch))
      (should (equal (oref branch value) "wip/kernel")))
    (should (equal (magit-land--target) '(:kind branch :ref "wip/kernel")))
    (magit-land-test--at "feat/parser-cli")
    (should (equal (magit-land--target)
                   '(:kind item :ref "feat/parser-cli" :set "42")))
    (magit-land-test--at "#40 held")
    (should (equal (magit-land--target) '(:kind set :ref "40" :set "40")))))

(ert-deftest magit-land-section-keymap ()
  (magit-land-test--with-status magit-land-test--status
    (dolist (text '("#41 checking" "lane/gdorg-fold" "feat/idea"))
      (magit-land-test--at text)
      (should (eq (lookup-key (get-text-property (point) 'keymap) "&")
                  'magit-land-dispatch)))))

(ert-deftest magit-land-section-empty-and-paused ()
  (magit-land-test--with-status
      "{\"main\":\"abc1234\",\"paused\":true,\"lane\":null,\"parked\":null}"
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "Land queue  main abc1234  lane: paused\n\n")))
  (magit-land-test--with-status "{\"main\":\"abc1234\",\"paused\":false,\"lane\":[],\"parked\":[]}"
    (should (string-match-p "lane: empty" (buffer-string)))))

(ert-deftest magit-land-section-failures-are-one-line ()
  (let ((cases `((,(list 3 "" "daemon is not running\nmore\n") . "exited 3: daemon is not running")
                 (,(list 0 "usage: git land\n" "") . "sent no queue status")
                 (,(list 0 "[1,2]" "") . "sent no queue status")
                 (,(list 0 "{\"lane\":[]}" "") . "sent no queue status")
                 (,(list 0 "" "") . "sent no queue status")
                 (,(list -1 "" "Searching for program: No such file") . "exited -1"))))
    (pcase-dolist (`(,reply . ,want) cases)
      (cl-letf (((symbol-function 'magit-land--call) (lambda (&rest _) reply)))
        (with-temp-buffer
          (magit-section-mode)
          (let ((inhibit-read-only t) (magit-insert-section--current nil))
            (magit-insert-section (root)
              (magit-land-insert-queue)))
          (let ((lines (split-string (string-trim (buffer-string)) "\n")))
            (should (= (length lines) 1))
            (should (string-prefix-p "Land queue unavailable: " (car lines)))
            (should (string-match-p (regexp-quote want) (car lines)))))))))

(ert-deftest magit-land-section-survives-a-render-error ()
  (cl-letf (((symbol-function 'magit-land--fetch)
             (lambda (_) (cons 'ok '((main . "abc1234") (lane . ((1 2 3))))))))
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t) (magit-insert-section--current nil))
        (magit-insert-section (root)
          (should-not (condition-case err (magit-land-insert-queue) (error err)))))
      (should (string-match-p "Land queue" (buffer-string))))))

(ert-deftest magit-land-call-reports-a-missing-program ()
  (let ((magit-land-land-command "/nonexistent/git-land"))
    (pcase-let ((`(,code ,_out ,err) (magit-land--call default-directory "status")))
      (should (= code -1))
      (should (stringp err)))))

(ert-deftest magit-land-call-splits-stdout-and-stderr ()
  (let* ((script (make-temp-file "git-land-fake" nil ".sh"))
         (magit-land-land-command script))
    (unwind-protect
        (progn
          (with-temp-file script
            (insert "#!/bin/sh\necho \"args: $*\"\necho oops >&2\nexit 4\n"))
          (set-file-modes script #o755)
          (should (equal (magit-land--call default-directory "log" "-n" "5")
                         '(4 "args: log -n 5\n" "oops\n"))))
      (delete-file script))))

(ert-deftest magit-land-add-queues-an-unqueued-branch-detached ()
  (magit-land-test--recording '(0 "queued #43\n" "")
    (magit-land-test--point-on 'land-branch "feat/idea"
      (magit-land-add)))
  (should (equal magit-land-test--calls '(("add" "--detach" "feat/idea")))))

(ert-deftest magit-land-add-needs-a-branch ()
  (magit-land-test--recording '(0 "" "")
    (magit-land-test--point-on 'land-set 41
      (should-error (magit-land-add) :type 'user-error)))
  (should-not magit-land-test--calls))

(ert-deftest magit-land-approve-shows-the-gates-then-approves ()
  (let ((prompt nil))
    (magit-land-test--recording (if (equal (car (car magit-land-test--calls)) "show")
                                    (list 0 magit-land-test--show "")
                                  '(0 "approved #40\n" ""))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (p) (setq prompt p) t)))
        (magit-land-test--point-on 'land-set 40
          (magit-land-approve))))
    (should (equal magit-land-test--calls '(("show" "--json" "40") ("approve" "--yes" "40"))))
    (should (string-match-p "gates: platform, hosts" prompt))
    (should (string-match-p "chore/kernel-budget" prompt))))

(ert-deftest magit-land-approve-declined-runs-nothing-more ()
  (magit-land-test--recording (list 0 magit-land-test--show "")
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (magit-land-test--point-on 'land-set 40
        (magit-land-approve))))
  (should (equal magit-land-test--calls '(("show" "--json" "40")))))

(ert-deftest magit-land-approve-refuses-a-set-that-is-not-held ()
  (let ((json (replace-regexp-in-string "\"state\":\"held\",\"gates"
                                        "\"state\":\"queued\",\"gates"
                                        magit-land-test--show)))
    (magit-land-test--recording (list 0 json "")
      (magit-land-test--point-on 'land-set 40
        (should-error (magit-land-approve) :type 'user-error)))
    (should (equal magit-land-test--calls '(("show" "--json" "40"))))))

(ert-deftest magit-land-approve-uses-the-set-of-an-item ()
  (magit-land-test--recording (list 0 magit-land-test--show "")
    (magit-land-test--point-on 'land-item "feat/parser"
      (magit-land-approve)))
  (should (equal (car magit-land-test--calls) '("show" "--json" "41"))))

(ert-deftest magit-land-commands-build-their-argv ()
  (magit-land-test--recording '(0 "ok\n" "")
    (magit-land-test--point-on 'land-set 40
      (magit-land-reject)
      (magit-land-remove)
      (magit-land-retry)
      (magit-land-bump))
    (magit-land-test--point-on 'land-item "feat/parser"
      (magit-land-remove)
      (magit-land-retry)
      (magit-land-reject)
      (magit-land-bump))
    (magit-land-pause)
    (magit-land-resume))
  (should (equal magit-land-test--calls
                 '(("reject" "40") ("remove" "40") ("retry" "40") ("bump" "40")
                 ("remove" "feat/parser") ("retry" "feat/parser")
                 ("reject" "41") ("bump" "41")
                         ("pause") ("resume")))))

(ert-deftest magit-land-declined-prompt-runs-nothing ()
  (magit-land-test--recording '(0 "" "")
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (magit-land-test--point-on 'land-set 40
        (magit-land-reject)
        (magit-land-remove))))
  (should-not magit-land-test--calls))

(ert-deftest magit-land-show-and-log-fill-buffers ()
  (let ((shown nil))
    (magit-land-test--recording (list 0 "#40 held  chore/kernel-budget\n" "")
      (cl-letf (((symbol-function 'display-buffer) (lambda (b &rest _) (setq shown b))))
        (magit-land-test--point-on 'land-item "chore/kernel-budget"
          (magit-land-show))
        (should (equal (with-current-buffer shown (buffer-string))
                       "#40 held  chore/kernel-budget\n"))
        (magit-land-log)))
    (should (equal magit-land-test--calls '(("show" "chore/kernel-budget") ("log" "-n" "20"))))
    (kill-buffer "*magit-land show*")
    (kill-buffer "*magit-land log*")))

(ert-deftest magit-land-command-failure-is-a-user-error ()
  (magit-land-test--recording '(1 "" "the owner spool is not writable here\n")
    (let ((err (should-error (magit-land-pause) :type 'user-error)))
      (should (string-match-p "pause failed: the owner spool" (cadr err))))))

(ert-deftest magit-land-dispatch-keys ()
  (dolist (key '("a" "A" "r" "k" "R" "b" "s" "l" "p" "P"))
    (should (transient-get-suffix 'magit-land-dispatch key))))

(ert-deftest magit-land-duration-format ()
  (should (equal (mapcar #'magit-land--duration '(0 45 130 3600 3900 -5))
                  '("0s" "45s" "2m10s" "1h00m" "1h05m" "0s"))))

(provide 'magit-land-test)
;;; magit-land-test.el ends here
