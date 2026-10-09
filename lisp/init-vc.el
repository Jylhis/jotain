;;; init-vc.el --- Version control: vc + magit + diff-hl -*- lexical-binding: t; -*-

;;; Commentary:

;; Built-in `vc' lives next to `magit' because they are tweaked as a
;; unit.  diff-hl ties the two together with fringe indicators.

;;; Code:

;; Defined in init-project.el, which loads after this file; only magit's
;; deferred :config reads it.
(defvar jotain-repositories-roots)

;;; @doc Built-in version control, limited to Git and Jujutsu: every
;;; other backend probes each visited file's parent directories for
;;; nothing. JJ comes from `vc-jj' below; without it in this list
;;; project.el would not discover `.jj' roots.
(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-handled-backends '(Git JJ))
  :config
  ;; Emacs 31+ options, guarded for Emacs 30.  Rewriting pushed history is
  ;; normal in jj and force-push workflows; a `vc-dir' revert hides
  ;; up-to-date entries.
  (when (boundp 'vc-allow-rewriting-published-history)
    (setopt vc-allow-rewriting-published-history t))
  (when (boundp 'vc-dir-auto-hide-up-to-date)
    (setopt vc-dir-auto-hide-up-to-date 'revert))
  ;; Run `C-x v' from non-VC buffers (backend from `default-directory');
  ;; save buffers before a `vc-dir' revert; view old revisions without
  ;; temp files on disk; and use the `C-x v I'/`C-x v O' incoming/outgoing
  ;; prefixes.
  (when (boundp 'vc-deduce-backend-nonvc-modes)
    (setopt vc-deduce-backend-nonvc-modes t))
  (when (boundp 'vc-dir-save-some-buffers-on-revert)
    (setopt vc-dir-save-some-buffers-on-revert t))
  (when (boundp 'vc-find-revision-no-save)
    (setopt vc-find-revision-no-save t))
  (when (boundp 'vc-use-incoming-outgoing-prefixes)
    (setopt vc-use-incoming-outgoing-prefixes t))
  ;; Largely covered by `global-auto-revert-mode' (init-core.el); adopted
  ;; from the newcomers-presets theme.
  (when (fboundp 'vc-auto-revert-mode)
    (vc-auto-revert-mode 1)))

;;; @doc C-x G jumps to a file git status reports as changed (modified,
;;; added, renamed, copied, unmerged, type-changed or untracked).
;;; Adapted from Rahul M. Juliato's emacs-solo/switch-git-status-buffer.
(use-package vc-git
  :ensure nil
  ;; Not C-x C-g: a sequence ending in C-g would break C-g as abort.
  :bind ("C-x G" . jotain-switch-git-status-buffer)
  :preface
  (declare-function vc-git-root "vc-git" (file))
  (defun jotain-switch-git-status-buffer ()
    "Switch to a file git status reports as changed in this repo.
Parses `git status --porcelain=v1 -z -uall': -z keeps spaces and
non-ASCII paths verbatim, and -uall lists untracked files whatever
`status.showUntrackedFiles' says.  Deletions are omitted, since there
is no file to open."
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

;;; @doc Jujutsu (jj) backend for built-in `vc' and `project', so
;;; `C-x v …', the modeline VC state and project.el work in jj repos.
;;; vc, diff-hl and smerge expect git-format diffs and conflicts, so set
;;; this via `jj config edit --user':
;;;   [ui]
;;;   diff-formatter = ":git"
;;;   conflict-marker-style = "git"
;;;
;;; C-x J is the jj twin of the C-x G status jump. The richer
;;; interactive view is `majutsu' (C-c j).
(use-package vc-jj
  :after vc
  :bind ("C-x J" . jotain-switch-jj-status-buffer)
  :preface
  (defun jotain-switch-jj-status-buffer ()
    "Switch to a file `jj' reports as changed in the working copy.
Parses `jj diff --summary -r @' (lines are \"<LETTER> <path>\").
Deletions are omitted, since there is no file to open."
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

;;; @doc The Git porcelain. C-x g for status, C-x M-g for dispatch,
;;; C-c g for the file menu. Diffs refine hunks (ignoring whitespace)
;;; and status buffers list worktrees.
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
  :config
  ;; Not :custom: `jotain-repositories-roots' is defined later.
  (setopt magit-repository-directories
          (mapcar (lambda (root) (cons root 2)) jotain-repositories-roots))
  (add-hook 'magit-status-sections-hook 'magit-insert-worktrees t))

;;; @doc The Jujutsu porcelain, a magit-style interface for jj that sits
;;; alongside magit in colocated repos. C-c j opens the log
;;; (`majutsu-log'); C-c M-j the transient dispatcher. Provided by Nix
;;; (nix/extra-packages.nix).
(use-package majutsu
  :ensure nil
  :commands (majutsu majutsu-log majutsu-dispatch)
  :bind
  (("C-c j"   . majutsu-log)
   ("C-c M-j" . majutsu-dispatch)))

;;; @doc Lists TODO/FIXME/HACK comments as a magit-status section.
;;; Scan depth 1 keeps it fast on large repos.
(use-package magit-todos
  :after magit
  :commands (magit-todos-mode global-magit-todos-mode)
  :custom
  (magit-todos-depth 1))

;;; @doc PRs, issues, and reviews from GitHub/GitLab/Forgejo inside
;;; magit. Uses Emacs's built-in sqlite, so no external emacsql binary is
;;; needed. Tokens come from auth-source (e.g. machine api.github.com
;;; login USER^forge password ghp_…).
(use-package forge
  :after magit
  :custom
  (forge-database-file (jotain-var-file "forge/database.sqlite"))
  (forge-post-directory (jotain-var-file "forge/posts/"))
  (forge-database-connector 'emacsql-sqlite-builtin))

;;; @doc Built-in transient menu system that magit/forge are built on.
;;; Its three state files live under var/.
(use-package transient
  :ensure nil
  ;; Deferred to keep transient off the startup path; the paths are set
  ;; when it loads, before any state file is read.
  :defer t
  :init
  (with-eval-after-load 'transient
    (setopt transient-history-file (jotain-var-file "transient/history.el")
            transient-values-file  (jotain-var-file "transient/values.el")
            transient-levels-file  (jotain-var-file "transient/levels.el"))))

;;; @doc Fringe indicators for added/changed/removed lines.
;;; `diff-hl-flydiff-mode` updates them before saving, too.
(use-package diff-hl
  :functions (diff-hl-flydiff-mode)
  :custom
  (diff-hl-draw-borders nil)
  (fringes-outside-margins t)
  (diff-hl-side 'left)
  :hook
  ;; No `:after magit': hooking before magit loads is fine, and gating on
  ;; it would miss after-init.  `diff-hl-magit-pre-refresh' is obsolete
  ;; (diff-hl 1.11); only the post-refresh hook is needed.
  ((after-init . global-diff-hl-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode 1))

;;; @doc VC change indicators next to files in dired. Its own block
;;; because `diff-hl-dired-mode' lives in diff-hl-dired.el: hooked from
;;; the `diff-hl' block, use-package would autoload it from "diff-hl",
;;; and with stale package autoloads that errors and empties every dired
;;; buffer.
(use-package diff-hl-dired
  :ensure nil
  :hook (dired-mode . diff-hl-dired-mode))

;;; @doc An `N/M` mode-line counter (via `mode-line-misc-info`): N is
;;; uncommitted added+deleted lines, M is commits made today (since local
;;; midnight, merges excluded). Works in git and jj repos (a `.jj`
;;; directory selects jj). N changes face at configurable thresholds, a
;;; nudge that the WIP is getting too large for one commit. Probes run
;;; async, so rendering never blocks.
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
  "Idle timer that periodically invalidates every cache entry.")

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
Probes the files git leaves while a rebase, merge, bisect or
cherry-pick is in flight, so the refresh can stay out of its way."
  (when-let* ((git-dir (jotain-git-stats--git-dir root)))
    (or (file-exists-p (expand-file-name "rebase-merge" git-dir))
        (file-exists-p (expand-file-name "rebase-apply" git-dir))
        (file-exists-p (expand-file-name "MERGE_HEAD" git-dir))
        (file-exists-p (expand-file-name "BISECT_LOG" git-dir))
        (file-exists-p (expand-file-name "CHERRY_PICK_HEAD" git-dir)))))

(defun jotain-git-stats--parse-numstat (output)
  "Sum the added + deleted columns of `--numstat' OUTPUT.
Lines are \"ADDED<TAB>DELETED<TAB>FILE\"; binary files show \"-\" and
are skipped.  Unlike `--shortstat' prose, this is locale-independent."
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
Used for jj's `jj diff --git'.  Skips lines starting with a doubled
marker (the `+++'/`---' headers), matching git's `--numstat'."
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
receives the parsed value.  Errors and non-zero exits map to 0, and so
does a failing `make-process' (e.g. binary not on PATH), so the cache
never stays busy.

Git callers pass `GIT_OPTIONAL_LOCKS=0' so a background refresh never
takes `.git/index.lock' and races the user's own git commands; jj
callers pass `--ignore-working-copy' so jj does not snapshot (and lock)
the working copy."
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
           ;; Changes: added + deleted lines in @.  Commits today:
           ;; non-empty changes authored since local midnight.
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
BACKEND is `git' or `jj'.  For git, also wait while a rebase, merge,
bisect or cherry-pick is in progress, keeping the cached counts.  jj
needs no such wait: its probes pass `--ignore-working-copy'."
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
Uses `default-directory' in file-less buffers, so magit status buffers
also invalidate after `magit-post-refresh-hook'."
  (when-let* ((file-or-dir (or buffer-file-name default-directory))
              (rb (jotain-git-stats--root-and-backend file-or-dir)))
    (let ((entry (jotain-git-stats--entry (car rb))))
      (plist-put entry :ts 0))))

(defun jotain-git-stats--tick ()
  "Idle-timer callback: invalidate every cache entry so visible buffers re-fetch."
  (maphash (lambda (_root entry) (plist-put entry :ts 0))
           jotain-git-stats--cache)
  (force-mode-line-update t))

;; `mode-line-misc-info' is shown by doom-modeline's `main' and the stock
;; mode line alike, so there is no hand-copied segment list to drift.
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

;; smerge-mode needs no block: it turns on for files with conflict
;; markers and binds C-c ^ (`smerge-command-prefix').

;;; @doc Built-in interactive diff. The `plain` window setup keeps the
;;; control panel out of a separate frame; diffs ignore whitespace.
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
            (tool-bar-lines . 0))))

(provide 'init-vc)
;;; init-vc.el ends here
