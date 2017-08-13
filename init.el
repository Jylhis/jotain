;; - editorconfig
;; - yasnippets
;; - whitespace-cleanup-mode???
;;
;; plugins to find
;; - buffers to tabs


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (version<= emacs-version "24")
	(message "Emacs is old"))

;; Please don't load outdated byte code
(setq load-prefer-newer t)
(setq gc-cons-threshold 50000000)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;;;;;;;;;;;;;;;;
;; PLUGINS
;;;;;;;;;;;;;;;;;

(require 'prelude-packages)
(require 'jylhis-debugger)
;;(require 'init-yasnippet)
;; Editgorconfig
;; autocopmlete??company??


;;;;;;;;;;;;;;;;;
;; SETTINGS
;;;;;;;;;;;;;;;;;

;; Poista toolbar
(tool-bar-mode -1)

;; Brackets pair homma
(electric-pair-mode 1)

;; Kelpaa pelkkä y ja n
(fset 'yes-or-no-p 'y-or-n-p)

;; Rivi highlight
(global-hl-line-mode 0)

;; Näyttää brackets parin
(show-paren-mode 1)

;; auto brackets
;(autopair-global-mode)

;; Line numbers
;(global-linum-mode 1)

;; Marking text
(delete-selection-mode t)
(transient-mark-mode t)
(setq select-enable-clipboard t)

;; Startup
(setq inhibit-startup-screen t
 inhibit-startup-echo-area-message t
 indicate-empty-lines t)

(setq visible-bell 1)

(setq large-file-warning-threshold 100000000)

(setq c-default-style "bsd"
      c-basic-offset 4
      blink-cursor-interval 0.4
      column-number-mode t
      tooltip-delay 1.5
      make-backup-files nil)

(transient-mark-mode t)

;; newline behavior
(global-set-key (kbd "RET") 'newline-and-indent)



;;(message "works")


;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;

;; Delete the current file
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;; Rename the current file
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))


;; Themes
(setq-default custom-enabled-themes '(tango-dark))
(defun light ()
  "Activate a light color theme."
  (interactive)
  (tango-dark))


(defun dark ()
  "Activate a light color theme."
  (interactive)
  (tango))
