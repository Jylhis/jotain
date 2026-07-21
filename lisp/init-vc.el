;;; init-vc.el --- Version control: vc + magit + diff-hl -*- lexical-binding: t; -*-

;;; Commentary:

;; Built-in `vc' bits live next to `magit' because in practice you tweak
;; them as a unit ("when I open a git file, what happens?"). diff-hl
;; ties the two together with fringe indicators.
;;
;; Future ideas (not yet wired up):
;;   - mergiraf      — syntax-aware structural merge driver
;;   - magit-delta   — render magit diffs through `delta' for syntax
;;                     highlighting (the same engine git-delta uses)
;;   - smerge / vc.el integration with the above for syntax-aware
;;     conflict resolution

;;; Code:

;;; @doc Built-in version control. Pinned to Git + Jujutsu — every
;;; other backend is a slow startup tax (probes every visited file's
;;; parents) you almost never benefit from. JJ is supplied by `vc-jj'
;;; below; without it in this list the backend never gets consulted
;;; and project.el won't discover `.jj' roots.
(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git JJ))
  :config
  ;; Emacs 31+: rewriting already-pushed history is a deliberate, normal
  ;; move in JJ / force-pushed feature-branch workflows, so don't fight
  ;; it. And refreshing a `vc-dir' buffer should fold away the
  ;; up-to-date entries on its own. Guarded for Emacs 30.
  (when (boundp 'vc-allow-rewriting-published-history)
    (setopt vc-allow-rewriting-published-history t))
  (when (boundp 'vc-dir-auto-hide-up-to-date)
    (setopt vc-dir-auto-hide-up-to-date 'revert)))

;;; @doc Quick jump to a file git status reports as changed. Runs
;;; `git status --porcelain=v1 -z -uall' (-z keeps spaces and
;;; non-ASCII paths intact; -uall forces untracked listing
;;; regardless of status.showUntrackedFiles) and offers
;;; M/A/R/C/U/T/?? entries through `completing-read'. Adapted
;;; from Rahul M. Juliato's emacs-solo/switch-git-status-buffer.
(use-package vc-git
  :ensure nil
  :bind ("C-x C-g" . jotain-switch-git-status-buffer)
  :preface
  (declare-function vc-git-root "vc-git" (file))
  (defun jotain-switch-git-status-buffer ()
    "Switch to a file git status reports as changed in this repo.
Candidates are parsed from `git status --porcelain=v1 -z -uall' so
paths containing spaces or non-ASCII characters arrive verbatim
and untracked files appear regardless of the user's
`status.showUntrackedFiles' setting. Modified, added, renamed,
copied, unmerged, type-changed, and untracked files are offered
through `completing-read'; pure deletions are omitted since the
working-tree file no longer exists to open."
    (interactive)
    (require 'vc-git)
    (let ((repo-root (vc-git-root default-directory)))
      (if (not repo-root)
          (message "Not inside a Git repository.")
        (let* ((expanded-root (expand-file-name repo-root))
               (default-directory expanded-root)
               (cmd-output (shell-command-to-string
                            "git status --porcelain=v1 -z -uall"))
               (target-files
                (let ((files nil)
                      (rest (split-string cmd-output "\0" t)))
                  (while rest
                    (let ((entry (pop rest)))
                      (when (> (length entry) 3)
                        (let ((status (substring entry 0 2))
                              (path-info (substring entry 3)))
                          (cond
                           ;; Rename/copy in -z mode: PATH (new) is
                           ;; on this entry, ORIG_PATH (old) is the
                           ;; next NUL chunk. See git-status(1).
                           ((string-match-p "^[RC]" status)
                            (let ((orig-path (and rest (pop rest))))
                              (push (cons (format "%s %s -> %s"
                                                  status
                                                  (or orig-path "?")
                                                  path-info)
                                          path-info)
                                    files)))
                           ((string-match-p "[MAUT?]" status)
                            (push (cons (format "%s %s"
                                                status path-info)
                                        path-info)
                                  files)))))))
                  (nreverse files))))
          (if (not target-files)
              (message "No changed files in this repository.")
            (let* ((selection (completing-read
                               "Switch to git-changed file: "
                               target-files nil t))
                   (file-path (cdr (assoc selection target-files))))
              (when file-path
                (find-file (expand-file-name file-path
                                             expanded-root))))))))))

;;; @doc Jujutsu (jj) backend for built-in `vc' and `project'. Adds the
;;; JJ entry pinned in `vc-handled-backends' above, so `C-x v …', the
;;; modeline VC state, and `project.el' all light up on jj repos
;;; (typically colocated with git). The jj side wants
;;;   [ui]
;;;   diff-formatter = ":git"
;;;   conflict-marker-style = "git"
;;; set via `jj config edit --user' so vc/diff-hl/smerge read jj diffs
;;; and conflicts in the format they expect.
;;;
;;; `jotain-switch-jj-status-buffer' (C-x C-j) is the jj twin of the
;;; git status jump above: it parses `jj diff --summary -r @' and offers
;;; the changed files (deletions omitted — the file is gone) through
;;; `completing-read'. The richer interactive view is `majutsu' (C-c j).
(use-package vc-jj
  :after vc
  :bind ("C-x C-j" . jotain-switch-jj-status-buffer)
  :preface
  (defun jotain-switch-jj-status-buffer ()
    "Switch to a file `jj' reports as changed in the working copy.
Candidates come from `jj diff --summary -r @', whose lines are
\"<LETTER> <path>\" (M/A/D/…). Modified and added files are offered
through `completing-read'; pure deletions are omitted since the
working-tree file no longer exists to open."
    (interactive)
    (let ((repo-root (locate-dominating-file default-directory ".jj")))
      (if (not repo-root)
          (message "Not inside a Jujutsu repository.")
        (let* ((expanded-root (expand-file-name repo-root))
               (default-directory expanded-root)
               (cmd-output (shell-command-to-string
                            "jj --no-pager diff --summary -r @"))
               (target-files
                (let (files)
                  (dolist (line (split-string cmd-output "\n" t))
                    (when (> (length line) 2)
                      (let ((status (substring line 0 1))
                            (path (substring line 2)))
                        (unless (string= status "D")
                          (push (cons (format "%s %s" status path) path)
                                files)))))
                  (nreverse files))))
          (if (not target-files)
              (message "No changed files in this jj working copy.")
            (let* ((selection (completing-read
                               "Switch to jj-changed file: "
                               target-files nil t))
                   (file-path (cdr (assoc selection target-files))))
              (when file-path
                (find-file (expand-file-name file-path expanded-root))))))))))

;;; @doc The Git porcelain. Bound C-x g for status, C-x M-g for global
;;; dispatch, C-c g for the file-specific menu. Refined hunks +
;;; whitespace-ignoring diffs are turned on globally.
(use-package magit
  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch)
   ("C-c g"   . magit-file-dispatch))
  :custom
  (magit-diff-refine-hunk t)
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-hide-trailing-cr-characters t)
  (magit-diff-context-lines 5)
  (magit-save-repository-buffers 'dontask)
  (magit-repository-directories '(("~/Developer" . 2)))
  :config
  ;; Show worktrees as a section in magit-status when more than one exists.
  (add-hook 'magit-status-sections-hook 'magit-insert-worktrees t))

;;; @doc The Jujutsu porcelain — a magit-style interface for jj, sitting
;;; alongside magit (jj is normally colocated with git, so both apply).
;;; C-c j opens the status/log buffer (`majutsu-log', aliased `majutsu');
;;; C-c M-j opens the top-level transient dispatcher. Provided by Nix
;;; (nix/extra-packages.nix), so `:ensure nil'.
(use-package majutsu
  :ensure nil
  :commands (majutsu majutsu-log majutsu-dispatch)
  :bind
  (("C-c j"   . majutsu-log)
   ("C-c M-j" . majutsu-dispatch)))

;;; @doc Surfaces TODO/FIXME/HACK comments as a section in magit-status.
;;; Scan depth pinned to 1 so it stays fast on large repos.
(use-package magit-todos
  :after magit
  :commands (magit-todos-mode global-magit-todos-mode)
  :custom
  (magit-todos-depth 1))

;;; @doc PRs, issues, and reviews from GitHub/GitLab/Forgejo inside
;;; magit. Uses the Emacs-30 built-in sqlite so no external
;;; emacsql binary is needed. Auth via ~/.authinfo.gpg
;;; (machine api.github.com login USER^forge password ghp_…).
(use-package forge
  :after magit
  :custom
  (forge-database-file (jotain-var-file "forge/database.sqlite"))
  (forge-post-directory (jotain-var-file "forge/posts/"))
  (forge-database-connector 'emacsql-sqlite-builtin))

;;; @doc Built-in transient menu system that magit/forge are built on.
;;; Themed under var/ so its three state files don't drop at the
;;; repo root.
(use-package transient
  :ensure nil
  :custom
  (transient-history-file (jotain-var-file "transient/history.el"))
  (transient-values-file  (jotain-var-file "transient/values.el"))
  (transient-levels-file  (jotain-var-file "transient/levels.el")))

;;; @doc Fringe indicators for added/changed/removed lines in the buffer
;;; you're editing. `diff-hl-flydiff-mode` updates pre-save so the
;;; indicators reflect uncommitted edits, not just the last save.
(use-package diff-hl
  :functions (diff-hl-flydiff-mode)
  :custom
  (diff-hl-draw-borders nil)
  (fringes-outside-margins t)
  (diff-hl-side 'left)
  :hook
  ;; No `:after magit'/`:demand' — adding functions to the
  ;; magit-{pre,post}-refresh hooks is safe before magit loads (hooks
  ;; are just variables), and gating on magit would postpone the
  ;; after-init registration past after-init itself.
  ((after-init . global-diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)
   (magit-pre-refresh  . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; Live, pre-save diff indicators.
  (diff-hl-flydiff-mode 1))

;;; @doc Adds `N/M` counters to the doom-modeline showing
;;; uncommitted line-level changes and commits made today (since
;;; local midnight, merges excluded). Works on both git and jj
;;; repos: a `.jj' directory selects the jj backend (`jj diff
;;; --git' for churn, an `author_date(after:"00:00")' revset for
;;; today's changes), otherwise git is used. The counter switches
;;; to a warning/urgent face above configurable thresholds — a
;;; nudge that the WIP is getting too large to squash into one
;;; coherent commit. Probes run async via `make-process` so the
;;; modeline render never blocks. (Names keep the `git-stats'
;;; prefix for back-compatibility of the user options below.)
(defgroup jotain-vc nil
  "Version-control modeline knobs for the Jotain configuration."
  :group 'jotain-ui)

(defcustom jotain-git-stats-update-interval 30
  "Seconds between background refreshes of the git-stats counters."
  :type 'integer
  :group 'jotain-vc)

(defcustom jotain-git-stats-warning-threshold 250
  "Uncommitted-change count above which the counter uses `jotain-git-stats-warning'."
  :type 'integer
  :group 'jotain-vc)

(defcustom jotain-git-stats-urgent-threshold 500
  "Uncommitted-change count above which the counter uses `jotain-git-stats-urgent'."
  :type 'integer
  :group 'jotain-vc)

(defface jotain-git-stats-normal '((t :inherit success))
  "Face for the uncommitted-changes counter below the warning threshold."
  :group 'jotain-vc)

(defface jotain-git-stats-warning '((t :inherit warning))
  "Face for the uncommitted-changes counter above the warning threshold."
  :group 'jotain-vc)

(defface jotain-git-stats-urgent '((t :inherit error))
  "Face for the uncommitted-changes counter above the urgent threshold."
  :group 'jotain-vc)

(defface jotain-git-stats-commits '((t :inherit font-lock-keyword-face))
  "Face for the commits-today counter."
  :group 'jotain-vc)

(defvar jotain-git-stats--cache (make-hash-table :test 'equal)
  "Hash table keyed by git repo root.
Each value is a plist (:changes N :commits M :ts TIMESTAMP :busy BOOL).")

(defvar jotain-git-stats--timer nil
  "Idle timer that walks the cache and invalidates stale entries.")

(defcustom jotain-git-stats-max-dotgit-bytes 4096
  "Maximum number of bytes read from a regular `.git' file.
Worktree indirection files are tiny; larger files are treated as
untrusted and ignored by `jotain-git-stats--git-dir'."
  :type 'integer
  :group 'jotain-vc)

(defun jotain-git-stats--entry (root)
  "Return the cache plist for ROOT, creating a zeroed one if missing."
  (or (gethash root jotain-git-stats--cache)
      (puthash root (list :changes 0 :commits 0 :ts 0 :busy nil)
               jotain-git-stats--cache)))

(defun jotain-git-stats--fresh-p (entry)
  "Return non-nil if ENTRY's cached values are still within the update interval."
  (< (- (float-time) (plist-get entry :ts))
     jotain-git-stats-update-interval))

(defun jotain-git-stats--git-dir (root)
  "Resolve the git-dir for ROOT.
Handles both normal repos (`.git' is a directory) and worktrees
(`.git' is a regular file holding `gitdir: <path>'). Returns nil
when ROOT is not under git control or parsing fails."
  (let ((dotgit (expand-file-name ".git" root)))
    (cond
     ((file-directory-p dotgit) dotgit)
     ((file-regular-p dotgit)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents dotgit nil 0 jotain-git-stats-max-dotgit-bytes)
            (goto-char (point-min))
            (when (re-search-forward "^gitdir: \\(.+\\)$" nil t)
              (let ((git-dir (expand-file-name (string-trim (match-string 1)) root)))
                (unless (file-remote-p git-dir)
                  git-dir))))
        (file-error nil)))
     (t nil))))

(defun jotain-git-stats--git-busy-p (root)
  "Return non-nil if ROOT has a long-running git op in progress.
Probes the sentinel files git drops while rebase / merge / bisect /
cherry-pick are mid-flight. The modeline skips its refresh in that
window — running our read-only `diff --numstat' against a foreground
`git rebase' is what lets our `make-process' calls race the user's
own `.git/index.lock' churn on the same checkout."
  (when-let* ((git-dir (jotain-git-stats--git-dir root)))
    (or (file-exists-p (expand-file-name "rebase-merge" git-dir))
        (file-exists-p (expand-file-name "rebase-apply" git-dir))
        (file-exists-p (expand-file-name "MERGE_HEAD" git-dir))
        (file-exists-p (expand-file-name "BISECT_LOG" git-dir))
        (file-exists-p (expand-file-name "CHERRY_PICK_HEAD" git-dir)))))

(defun jotain-git-stats--parse-numstat (output)
  "Sum the added + deleted columns from `git diff --numstat' OUTPUT.
Format is TAB-separated \"ADDED<TAB>DELETED<TAB>FILE\" per line; binary
files report a literal \"-\" in the count columns and are skipped.
This is locale-independent — unlike `--shortstat', whose English
prose breaks under `LANG=de_DE.UTF-8' etc."
  (let ((sum 0))
    (dolist (line (split-string output "\n" t))
      (let ((cols (split-string line "\t")))
        (when (and (>= (length cols) 2)
                   (string-match-p "\\`[0-9]+\\'" (nth 0 cols))
                   (string-match-p "\\`[0-9]+\\'" (nth 1 cols)))
          (setq sum (+ sum
                       (string-to-number (nth 0 cols))
                       (string-to-number (nth 1 cols)))))))
    sum))

(defun jotain-git-stats--count-lines (output)
  "Count non-empty lines in OUTPUT."
  (if (string-empty-p (string-trim output))
      0
    (length (split-string output "\n" t))))

(defun jotain-git-stats--count-diff-churn (output)
  "Count added + deleted lines in a unified-diff OUTPUT.
Used for the jj backend, where `jj diff --git' emits a git-format
diff.  Counts lines beginning with a single `+' or `-' while skipping
the `+++'/`---' file headers (which begin with a doubled marker), so
the result matches git's `--numstat' added+deleted semantics."
  (let ((sum 0))
    (dolist (line (split-string output "\n" t))
      (when (and (> (length line) 0)
                 (memq (aref line 0) '(?+ ?-))
                 (or (= (length line) 1)
                     (not (memq (aref line 1) '(?+ ?-)))))
        (setq sum (1+ sum))))
    sum))

(defun jotain-git-stats--run (root command extra-env parse-fn callback)
  "Run COMMAND (a full argv list) with `default-directory' set to ROOT.
EXTRA-ENV is a list of \"VAR=VALUE\" strings prepended to
`process-environment'.  PARSE-FN is applied to stdout; CALLBACK
receives the parsed value.  Errors and non-zero exits are mapped to 0.
If `make-process' itself fails (e.g. the binary is missing on PATH),
the buffer is killed and CALLBACK is still invoked with 0 so the cache
never deadlocks.

The git callers prepend `GIT_OPTIONAL_LOCKS=0' so the read paths
(`diff-index', `log') decline to take `.git/index.lock' for an
opportunistic refresh — that lock is what races the user's own `git
rebase' / `git commit' on the same checkout when N modeline refreshes
fire in parallel across sibling Emacs / Claude sessions.  The jj
callers pass `--ignore-working-copy' in COMMAND for the same reason:
it stops jj snapshotting (and locking) the working copy from a
background timer."
  (let* ((buffer (generate-new-buffer " *jotain-git-stats*"))
         (default-directory root)
         (process-environment (append extra-env process-environment)))
    (condition-case nil
        (make-process
         :name "jotain-git-stats"
         :buffer buffer
         :command command
         :noquery t
         :sentinel
         (lambda (proc _event)
           (when (memq (process-status proc) '(exit signal))
             (let* ((buf (process-buffer proc))
                    (value
                     (condition-case nil
                         (if (and (eq 0 (process-exit-status proc))
                                  (buffer-live-p buf))
                             (with-current-buffer buf
                               (funcall parse-fn (buffer-string)))
                           0)
                       (error 0))))
               (when (buffer-live-p buf)
                 (kill-buffer buf))
               (funcall callback value)))))
      (error
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (funcall callback 0)))))

(defun jotain-git-stats--root-and-backend (file-or-dir)
  "Return (ROOT . BACKEND) covering FILE-OR-DIR, or nil when uncontrolled.
BACKEND is `jj' when a `.jj' directory dominates (checked first, since
jj repos are typically colocated with a `.git'), otherwise `git'.  ROOT
is `expand-file-name'd."
  (when file-or-dir
    (let ((jj-root (locate-dominating-file file-or-dir ".jj")))
      (if jj-root
          (cons (expand-file-name jj-root) 'jj)
        (when-let* ((git-root (locate-dominating-file file-or-dir ".git")))
          (cons (expand-file-name git-root) 'git))))))

(defun jotain-git-stats--refresh (root backend)
  "Kick off the two async probes that populate the cache for ROOT.
BACKEND selects the command set (`git' or `jj')."
  (let ((entry (jotain-git-stats--entry root)))
    (unless (plist-get entry :busy)
      (plist-put entry :busy t)
      (let* ((pending 2)
             (after (lambda ()
                      (setq pending (1- pending))
                      (when (zerop pending)
                        (plist-put entry :busy nil)
                        (plist-put entry :ts (float-time))
                        (force-mode-line-update t))))
             (set-changes (lambda (n) (plist-put entry :changes n) (funcall after)))
             (set-commits (lambda (n) (plist-put entry :commits n) (funcall after))))
        (pcase backend
          ('jj
           ;; `--ignore-working-copy' keeps the background probe from
           ;; snapshotting/locking the working copy.  Changes = added +
           ;; deleted lines in @ vs its parent; commits-today = non-empty
           ;; changes authored since local midnight (root excluded).
           (jotain-git-stats--run
            root '("jj" "--no-pager" "--ignore-working-copy"
                   "diff" "--git" "-r" "@")
            nil #'jotain-git-stats--count-diff-churn set-changes)
           (jotain-git-stats--run
            root '("jj" "--no-pager" "--ignore-working-copy"
                   "log" "--no-graph"
                   "-r" "author_date(after:\"00:00\") ~ root() ~ empty()"
                   "-T" "\"x\\n\"")
            nil #'jotain-git-stats--count-lines set-commits))
          (_
           (jotain-git-stats--run
            root '("git" "--no-pager" "-c" "core.fsmonitor=false"
                   "-c" "diff.external=" "diff-index" "--numstat" "HEAD")
            '("GIT_OPTIONAL_LOCKS=0")
            #'jotain-git-stats--parse-numstat set-changes)
           (jotain-git-stats--run
            root '("git" "--no-pager" "-c" "core.fsmonitor=false"
                   "-c" "diff.external=" "log" "--since=midnight"
                   "--oneline" "--no-merges")
            '("GIT_OPTIONAL_LOCKS=0")
            #'jotain-git-stats--count-lines set-commits)))))))

(defun jotain-git-stats--maybe-refresh (root backend)
  "Refresh ROOT's cache if it's stale and no refresh is already in flight.
BACKEND is `git' or `jj'.  For git, also skips entirely while a
long-running op (rebase / merge / bisect / cherry-pick) is in progress
under ROOT — our read-only probes there are the same calls the user's
foreground rebase is racing, so just hold the cached counts until the
operation clears.  jj has no such index-lock contention (its ops are
atomic via the operation log and our probes use `--ignore-working-copy')."
  (let ((entry (jotain-git-stats--entry root)))
    (unless (or (plist-get entry :busy)
                (jotain-git-stats--fresh-p entry)
                (and (eq backend 'git)
                     (jotain-git-stats--git-busy-p root)))
      (jotain-git-stats--refresh root backend))))

(defun jotain-git-stats--face-for-changes (n)
  "Pick the appropriate face for N uncommitted changes."
  (cond ((>= n jotain-git-stats-urgent-threshold)  'jotain-git-stats-urgent)
        ((>= n jotain-git-stats-warning-threshold) 'jotain-git-stats-warning)
        (t                                         'jotain-git-stats-normal)))

(defun jotain-git-stats--render (root)
  "Return the propertised `N/M' string for ROOT, or nil when empty."
  (let* ((entry (jotain-git-stats--entry root))
         (n (plist-get entry :changes))
         (m (plist-get entry :commits)))
    (when (or (> n 0) (> m 0))
      (concat " "
              (propertize (number-to-string n)
                          'face (jotain-git-stats--face-for-changes n))
              (propertize "/" 'face 'shadow)
              (propertize (number-to-string m)
                          'face 'jotain-git-stats-commits)
              " "))))

(defun jotain-git-stats--invalidate-current-buffer (&rest _)
  "Drop the cached freshness for this buffer's repo so the next render refreshes.
Falls back to `default-directory' when `buffer-file-name' is nil so
that magit status buffers (which have no file) still trigger an
invalidation after `magit-post-refresh-hook'."
  (when-let* ((file-or-dir (or buffer-file-name default-directory))
              (rb (jotain-git-stats--root-and-backend file-or-dir)))
    (let ((entry (jotain-git-stats--entry (car rb))))
      (plist-put entry :ts 0))))

(defun jotain-git-stats--tick ()
  "Idle-timer callback: invalidate every cache entry so visible buffers re-fetch."
  (maphash (lambda (_root entry) (plist-put entry :ts 0))
           jotain-git-stats--cache)
  (force-mode-line-update t))

;; Attach the counters through `mode-line-misc-info' instead of
;; re-declaring doom-modeline's `main' modeline with a hand-copied
;; segment list (which would silently drift as upstream adds/renames
;; segments).  doom-modeline renders misc-info in its default `main',
;; and the stock mode line shows it too, so the counter survives
;; upstream changes and works without doom-modeline loaded.
(add-to-list 'mode-line-misc-info
             '(:eval (when-let* ((file buffer-file-name)
                                 (rb (and (mode-line-window-selected-p)
                                          (jotain-git-stats--root-and-backend file))))
                       (progn (jotain-git-stats--maybe-refresh (car rb) (cdr rb))
                              (or (jotain-git-stats--render (car rb)) ""))))
             t)

(add-hook 'after-save-hook #'jotain-git-stats--invalidate-current-buffer)
(add-hook 'magit-post-refresh-hook #'jotain-git-stats--invalidate-current-buffer)
(unless jotain-git-stats--timer
  (setq jotain-git-stats--timer
        (run-with-idle-timer jotain-git-stats-update-interval t
                             #'jotain-git-stats--tick)))

;;; @doc Built-in conflict-marker editor. Custom prefix C-c ^ groups
;;; upper/lower/next/prev so resolving merges doesn't require
;;; scrolling through the smerge menu.
(use-package smerge-mode
  :ensure nil
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)
              ("C-c ^ l" . smerge-keep-lower)
              ("C-c ^ n" . smerge-next)
              ("C-c ^ p" . smerge-prev)))

;;; @doc Built-in interactive diff. Configured with `plain` window setup
;;; so the control panel doesn't pop a separate frame, plus
;;; whitespace-ignoring diffs for less merge noise.
(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally)
  (ediff-diff-options "-w")
  (ediff-custom-diff-options "-u")
  (ediff-merge-revisions-with-ancestor t)
  :config
  (setopt ediff-control-frame-parameters
          '((name . "Ediff Control")
            (width . 60)
            (height . 14)
            (left . 200)
            (top . 200)
            (minibuffer . nil)
            (user-position . t)
            (vertical-scroll-bars . nil)
            (scrollbar-width . 0)
            (menu-bar-lines . 0)
            (tool-bar-lines . 0))))

(provide 'init-vc)
;;; init-vc.el ends here
