;;; init-tabs.el --- Workspace tabs via tab-bar-mode -*- lexical-binding: t; -*-

;;; Commentary:

;; Frame-local tabs as tmux-like workspaces.  `tab-bar-history-mode'
;; adds per-tab layout undo/redo.  Switching project selects that
;; project's tab, creating one named after the directory if needed.
;;
;; Built-in keybindings (C-x t prefix):
;;   C-x t 2  new tab            C-x t 0  close tab
;;   C-x t o  next tab           C-x t O  previous tab
;;   C-x t RET  switch to a named tab

;;; Code:

(declare-function tab-bar-tabs "tab-bar")
(declare-function tab-bar-select-tab "tab-bar" (&optional tab-number))
(declare-function tab-bar-new-tab "tab-bar" (&optional arg from-number))
(declare-function tab-bar-rename-tab "tab-bar" (name &optional tab-number))

;;; @doc Built-in frame-local tab bar, used as a workspace switcher.
;;; The bar is hidden while only one tab exists.
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
  "Around advice for ORIG: open project DIR in its own tab.
Tabs are keyed by the abbreviated directory (the
`jotain-tabs-project-dir' tab parameter), so projects sharing a
basename still get distinct tabs."
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
      (let ((current (assq 'current-tab (tab-bar-tabs))))
        (when current
          (push (cons 'jotain-tabs-project-dir dir-key) (cdr current))))
      (funcall orig dir))))

(advice-add 'project-switch-project :around
            #'jotain-tabs--switch-project-in-tab)

(provide 'init-tabs)
;;; init-tabs.el ends here
