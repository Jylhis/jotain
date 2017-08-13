
;;;;;;;;;;;;;;;
;; TODO: Modify menu items
;;;;;;;;;;;;;;;

;; File | Edit | View | Build | Debug | Tools | [locals] | Help

;; File
;; https://www.emacswiki.org/emacs/RecentFiles
;; https://gitlab.com/Jylhis/Emacs/blob/master/lisp/init-recentf.el
;; https://github.com/guancio/emacsconfig/blob/master/.emacs.d/clean_menu.el

;; New file, Open file, close | save, save as, save copy as, , rename save all, reload from Disk, recover crashed session | Print |recent files: [list] |Exit

;;(define-key global-map [menu-bar file open-file] '("Open file or folder" . ido-find-file))

;;(define-key global-map [menu-bar file new-file] '("New" . new-empty-buffer))

;; Reload from Disk
(define-key menu-bar-file-menu [revert-buffer]
  (list 'menu-item "Reload from Disk" 'revert-buffer))

;; Frames to windows
(define-key menu-bar-file-menu [make-frame]
  (list 'menu-item "New Window" 'make-frame-command))

(define-key menu-bar-file-menu [delete-this-frame]
  (list 'menu-item "Delete Window" 'delete-frame))

(define-key global-map [menu-bar file make-frame-on-display] 'undefined)

;; Remove printing stuff
(define-key global-map [menu-bar file ps-print-buffer] 'undefined)
(define-key global-map [menu-bar file ps-print-region] 'undefined)
(define-key global-map [menu-bar file ps-print-buffer-faces] 'undefined)
(define-key global-map [menu-bar file ps-print-region-faces] 'undefined)

;; Edit
;; undo, redo | cut, copy, paste, delete, select all |[format options, indent file, selectino etc] |find, find and replace, goto line, navigate to...(functons classess etc)


;; view

;; build & run
;; shell command
;; TODO: read makefile for build options

;; Debug
;; TODO

;; Macro
;; https://www.emacswiki.org/emacs/KeyboardMacros
;; Start recording, Stop recording, Playback, Save current macro | [list of saved macros] | Remove macro
(define-key global-map [menu-bar macro]
  (cons "Macro" (make-sparse-keymap "Macro")))

(define-key global-map
  [menu-bar macro start-macro]
  '("Start recording" . kmacro-start-macro-or-insert-counter))

(define-key global-map
  [menu-bar macro stop-macro]
  '("Stop recording or Run" . kmacro-end-or-call-macro))


(define-key global-map
  [menu-bar macro run-macro]
  '("Run" . kmacro-end-and-call-macro))

;; settings, theme selector,

(define-key global-map [menu-bar tools gnus] 'undefined)
(define-key global-map [menu-bar tools rmail] 'undefined)
(define-key global-map [menu-bar tools compose-mail] 'undefined)
(define-key global-map [menu-bar tools directory-search] 'undefined)
(define-key global-map [menu-bar tools browse-web] 'undefined)

(define-key global-map [menu-bar tools games] 'undefined)
(define-key global-map [menu-bar tools encryption-decryption] 'undefined)
(define-key global-map [menu-bar tools simple-calculator] 'undefined)
(define-key global-map [menu-bar tools calendar] 'undefined)
(define-key global-map [menu-bar tools calc] 'undefined)

(define-key global-map [menu-bar tools separator] 'undefined)
;;(define-key global-map [menu-bar tools separator] 'undefined)
;;(define-key global-map [menu-bar tools separator-exit] 'undefined)
;;(global-unset-key [menu-bar tools separator])
;;(global-unset-key [menu-bar tools separator])
;;(global-unset-key [menu-bar tools menu-bar-separator])



;; Make a menu keymap (with a prompt string)
;; and make it the menu bar item's definition.
;;(define-key global-map [menu-bar words]
;;  (cons "Words" (make-sparse-keymap "Words")))

;; Define specific subcommands in this menu.
;;(define-key global-map
;;  [menu-bar words forward]
;;  '("Forward word" . forward-word))
;;(define-key global-map
;;  [menu-bar words backward]
;;  '("Backward word" . backward-word))


;; Remove item
;;(define-key global-map [menu-bar edit] 'undefined)


;; Right click menu
;; TODO: Lisää copy pastet??
(global-set-key [mouse-3] (lambda (event &optional prefix)
			    (interactive "@e\nP")
			    (popup-menu (mouse-menu-major-mode-map)event prefix)))


(provide 'menu-bar)

;;; menu-bar.el ends here
