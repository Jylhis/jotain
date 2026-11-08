;;; magit-land.el --- Summary buffer for landing a task branch -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; `magit-land' opens a buffer for the current branch against its base
;; (default origin/main).  It lists one line per commit and, under each,
;; the kinds of check result in refs/notes/checks and the text of the
;; note in refs/notes/review.  Keys:
;;
;;   L  run `git-land' in a compilation buffer (C-u L adds --yes)
;;   S  show the check results of the commit at point (`git-results show')
;;   D  remove the worktree with `wt remove', after a y-or-n-p
;;   g  refresh
;;
;; The library needs only git.  `git-land', `git-results' and `wt' are
;; called when a key asks for them.

;;; Code:

(require 'compile)
(require 'subr-x)

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

(defvar-local magit-land--root nil
  "The top-level directory of the repository the buffer shows.")

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
With prefix argument YES, pass --yes to approve a gated change."
  (interactive "P")
  (let ((default-directory magit-land--root))
    (compile (concat magit-land-land-command (and yes " --yes")))))

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
