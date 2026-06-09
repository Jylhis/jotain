;;; init-tabs.el --- Workspace tabs via tab-bar-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Frame-local tabs as tmux-like workspaces. Each tab holds its own
;; window configuration; `tab-bar-history-mode' adds undo/redo for the
;; per-tab layout. Project switches open the project in a fresh tab
;; named after the directory, so projects don't fight over windows.
;;
;; Built-in keybindings (C-x t prefix):
;;   C-x t 2  new tab            C-x t 0  close tab
;;   C-x t o  next tab           C-x t O  previous tab
;;   C-x t RET / C-x t b  switch to a named tab

;;; Code:

(declare-function tab-bar-tabs "tab-bar")
(declare-function tab-bar-select-tab "tab-bar" (&optional tab-number))
(declare-function tab-bar-new-tab "tab-bar" (&optional arg from-number))
(declare-function tab-bar-rename-tab "tab-bar" (name &optional tab-number))

;;; @doc Built-in frame-local tab bar — used as a workspace switcher.
;;; Bar hides itself when only one tab exists, so single-window
;;; sessions are unaffected.
(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :functions (tab-bar-history-mode)
  :custom
  (tab-bar-show 1)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t)
  :config
  (tab-bar-history-mode 1))

(defun jotain-tabs--switch-project-in-tab (orig dir)
  "Open DIR in its own tab.
Tabs are keyed by the abbreviated project directory (stored as
the `jotain-tabs-project-dir' tab parameter), so two projects
that share a basename across different roots get distinct tabs."
  (let* ((dir-key (abbreviate-file-name (directory-file-name dir)))
         (name    (file-name-nondirectory (directory-file-name dir)))
         (tabs    (tab-bar-tabs))
         (idx     (cl-position
                   dir-key tabs
                   :key (lambda (tab)
                          (alist-get 'jotain-tabs-project-dir tab))
                   :test #'equal)))
    (if idx
        (progn (tab-bar-select-tab (1+ idx))
               (funcall orig dir))
      (tab-bar-new-tab)
      (tab-bar-rename-tab name)
      (let ((current (cl-find-if (lambda (tab)
                                   (alist-get 'current-tab tab))
                                 (tab-bar-tabs))))
        (when current
          (setf (alist-get 'jotain-tabs-project-dir current) dir-key)))
      (funcall orig dir))))

(advice-add 'project-switch-project :around
            #'jotain-tabs--switch-project-in-tab)

(provide 'init-tabs)
;;; init-tabs.el ends here
