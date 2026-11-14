;;; init-tabs.el --- Workspace tabs via tab-bar-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Markus Jylhänkangas

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

;; Frame-local tabs as tmux-like workspaces.  `tab-bar-history-mode' (on
;; through the newcomers-presets theme) adds per-tab layout undo/redo.
;; Switching project selects that project's tab, creating one named
;; after the directory if needed.
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
  :custom
  ;; Overrides the newcomers-presets theme's 0 (always shown).
  (tab-bar-show 1)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t))

(defun my-tabs--switch-project-in-tab (orig dir)
  "Around advice for ORIG: open project DIR in its own tab.
Tabs are keyed by the abbreviated directory (the
`my-tabs-project-dir' tab parameter), so projects sharing a
basename still get distinct tabs."
  (let* ((dir-key (abbreviate-file-name (directory-file-name dir)))
         (name    (file-name-nondirectory (directory-file-name dir)))
         (tabs    (tab-bar-tabs))
         (idx     (cl-position
                   dir-key tabs
                   :key (lambda (tab)
                          (alist-get 'my-tabs-project-dir tab))
                   :test #'equal)))
    (if idx
        (progn (tab-bar-select-tab (1+ idx))
               (funcall orig dir))
      (tab-bar-new-tab)
      (tab-bar-rename-tab name)
      (let ((current (assq 'current-tab (tab-bar-tabs))))
        (when current
          (push (cons 'my-tabs-project-dir dir-key) (cdr current))))
      (funcall orig dir))))

(advice-add 'project-switch-project :around
            #'my-tabs--switch-project-in-tab)

(provide 'init-tabs)
;;; init-tabs.el ends here
