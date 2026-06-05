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

;;; @doc Built-in version control. Pinned to Git only — every other
;;; backend is a slow startup tax (probes every visited file's
;;; parents) you almost never benefit from.
(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git)))

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
  :after magit
  :demand t
  :functions (diff-hl-flydiff-mode diff-hl-magit-post-refresh diff-hl-dired-mode)
  :custom
  (diff-hl-draw-borders nil)
  (fringes-outside-margins t)
  (diff-hl-side 'left)
  :hook
  ((after-init . global-diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)
   (magit-pre-refresh  . ignore)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; Live, pre-save diff indicators.
  (diff-hl-flydiff-mode 1))

;;; @doc Adds `N/M` counters to the doom-modeline showing
;;; uncommitted line-level changes against HEAD and commits made
;;; today (since local midnight, merges excluded). The counter
;;; switches to a warning/urgent face above configurable
;;; thresholds — a nudge that the WIP is getting too large to
;;; squash into one coherent commit. Git invocations run async via
;;; `make-process` so the modeline render never blocks.
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

(defun jotain-git-stats--run (root args parse-fn callback)
  "Run \"git ARGS\" with `default-directory' set to ROOT.
PARSE-FN is applied to stdout; CALLBACK receives the parsed value.
Errors and non-zero exits are mapped to 0. If `make-process' itself
fails (e.g. `git' is missing on PATH), the buffer is killed and
CALLBACK is still invoked with 0 so the cache never deadlocks.

`GIT_OPTIONAL_LOCKS=0' is prepended to `process-environment' so the
read paths (`diff --numstat', `log') decline to take `.git/index.lock'
for an opportunistic refresh — that lock is what races the user's own
`git rebase' / `git commit' on the same checkout when N modeline
refreshes fire in parallel across sibling Emacs / Claude sessions."
  (let* ((buffer (generate-new-buffer " *jotain-git-stats*"))
         (default-directory root)
         (process-environment
          (cons "GIT_OPTIONAL_LOCKS=0" process-environment)))
    (condition-case nil
        (make-process
         :name "jotain-git-stats"
         :buffer buffer
         :command (append '("git"
                            "--no-pager"
                            "-c" "core.fsmonitor=false"
                            "-c" "diff.external=")
                          args)
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

(defun jotain-git-stats--refresh (root)
  "Kick off the two async git probes that populate the cache for ROOT."
  (let ((entry (jotain-git-stats--entry root)))
    (unless (plist-get entry :busy)
      (plist-put entry :busy t)
      (let* ((pending 2)
             (after (lambda ()
                      (setq pending (1- pending))
                      (when (zerop pending)
                        (plist-put entry :busy nil)
                        (plist-put entry :ts (float-time))
                        (force-mode-line-update t)))))
        (jotain-git-stats--run
         root '("diff-index" "--numstat" "HEAD")
         #'jotain-git-stats--parse-numstat
         (lambda (n) (plist-put entry :changes n) (funcall after)))
        (jotain-git-stats--run
         root '("log" "--since=midnight" "--oneline" "--no-merges")
         #'jotain-git-stats--count-lines
         (lambda (n) (plist-put entry :commits n) (funcall after)))))))

(defun jotain-git-stats--maybe-refresh (root)
  "Refresh ROOT's cache if it's stale and no refresh is already in flight.
Also skips entirely while a long-running git op (rebase / merge / bisect /
cherry-pick) is in progress under ROOT — our read-only probes there are
the same calls the user's foreground rebase is racing, so just hold the
cached counts until the operation clears."
  (let ((entry (jotain-git-stats--entry root)))
    (unless (or (plist-get entry :busy)
                (jotain-git-stats--fresh-p entry)
                (jotain-git-stats--git-busy-p root))
      (jotain-git-stats--refresh root))))

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
              (root (locate-dominating-file file-or-dir ".git")))
    (let ((entry (jotain-git-stats--entry (expand-file-name root))))
      (plist-put entry :ts 0))))

(defun jotain-git-stats--tick ()
  "Idle-timer callback: invalidate every cache entry so visible buffers re-fetch."
  (maphash (lambda (_root entry) (plist-put entry :ts 0))
           jotain-git-stats--cache)
  (force-mode-line-update t))

;; `doom-modeline-def-segment' and `doom-modeline-def-modeline' are
;; macros; make them available at byte-compile time so their forms
;; below get expanded instead of treated as unknown function calls.
(eval-when-compile (require 'doom-modeline))

(with-eval-after-load 'doom-modeline
  (doom-modeline-def-segment jotain-git-stats
    "Uncommitted-changes / commits-today counters."
    (let* ((file buffer-file-name)
           (raw-root (and (mode-line-window-selected-p)
                          file
                          (locate-dominating-file file ".git"))))
      (if (not raw-root)
          ""
        (let ((root (expand-file-name raw-root)))
          (jotain-git-stats--maybe-refresh root)
          (or (jotain-git-stats--render root) "")))))

  ;; Re-declare the `main' modeline with the new segment appended to the
  ;; right-hand list, just before `time'. Mirrors doom-modeline's default
  ;; main definition; only the trailing segment list differs.
  (doom-modeline-def-modeline 'main
    '(eldoc bar workspace-name window-number modals matches follow
            buffer-info remote-host buffer-position word-count
            parrot selection-info)
    '(compilation objed-state misc-info battery grip irc mu4e gnus github
                  debug repl lsp minor-modes input-method indent-info
                  buffer-encoding major-mode process vcs jotain-git-stats
                  check time))

  (add-hook 'after-save-hook #'jotain-git-stats--invalidate-current-buffer)
  (add-hook 'magit-post-refresh-hook #'jotain-git-stats--invalidate-current-buffer)
  (unless jotain-git-stats--timer
    (setq jotain-git-stats--timer
          (run-with-idle-timer jotain-git-stats-update-interval t
                               #'jotain-git-stats--tick))))

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
