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

(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  ;; Only enable git: tramp + the other backends are slow startup taxes
  ;; you almost never benefit from.
  (vc-handled-backends '(Git)))

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

(use-package magit-todos
  :after magit
  :commands (magit-todos-mode global-magit-todos-mode)
  :custom
  ;; Limit scan depth to 1 — keeps things fast on large repos.
  (magit-todos-depth 1))

;; Forge: GitHub/GitLab/Forgejo PRs, issues, and reviews inside Magit.
;; Auth: tokens are fetched on the fly from the `gh' (GitHub) and
;; `glab' (GitLab) CLI tools.  Falls through to auth-source when
;; neither CLI is available.
(use-package forge
  :after magit
  :custom
  (forge-database-file (jotain-var-file "forge/database.sqlite"))
  (forge-post-directory (jotain-var-file "forge/posts/"))
  ;; Use the built-in sqlite (Emacs 30+) instead of the external emacsql.
  (forge-database-connector 'emacsql-sqlite-builtin)
  :config
  ;; --- CLI-based authentication for forge ---
  ;; Instead of maintaining tokens in ~/.authinfo.gpg, shell out to
  ;; `gh auth token' / `glab auth token'.  The :before-until advices
  ;; intercept ghub's token lookup; when the CLI returns a valid token
  ;; auth-source is never consulted.

  (defun jotain--cli-auth-token (cli &optional host)
    "Return a token string from CLI (\"gh\" or \"glab\") for HOST.
Return nil when the CLI is absent or the command fails, so the
caller falls through to auth-source."
    (when (executable-find cli)
      (with-temp-buffer
        (let ((args (list "auth" "token")))
          (when host (setq args (append args (list "-h" host))))
          (when (zerop (apply #'call-process cli nil t nil args))
            (let ((token (string-trim (buffer-string))))
              (unless (string-empty-p token) token)))))))

  ;; GitHub — `gh auth token [-h HOST]'
  (defun jotain--ghub-cli-token (host username package &optional noerror forge)
    "Return a GitHub token from the `gh' CLI for HOST."
    (ignore username package noerror forge)
    (jotain--cli-auth-token
     "gh"
     (unless (equal host "api.github.com")
       (replace-regexp-in-string "\\`api\\." "" host))))

  (advice-add 'ghub--token :before-until #'jotain--ghub-cli-token)

  ;; GitLab — `glab auth token [-h HOST]'
  ;; glab.el is autoloaded when forge visits a GitLab remote, so defer
  ;; the advice until it's actually needed.
  (with-eval-after-load 'glab
    (defun jotain--glab-cli-token (host username package &optional noerror forge)
      "Return a GitLab token from the `glab' CLI for HOST."
      (ignore username package noerror forge)
      (jotain--cli-auth-token
       "glab"
       (replace-regexp-in-string "/api/v4\\'" "" host)))
    (advice-add 'glab--token :before-until #'jotain--glab-cli-token)))

;; transient is the menu system magit/forge are built on. Theme its
;; three state files under var/ so they don't drop at the repo root.
(use-package transient
  :ensure nil
  :custom
  (transient-history-file (jotain-var-file "transient/history.el"))
  (transient-values-file  (jotain-var-file "transient/values.el"))
  (transient-levels-file  (jotain-var-file "transient/levels.el")))

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

(use-package smerge-mode
  :ensure nil
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)
              ("C-c ^ l" . smerge-keep-lower)
              ("C-c ^ n" . smerge-next)
              ("C-c ^ p" . smerge-prev)))

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
