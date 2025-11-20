;;; app-launchers.el --- Possible alternatives to dmenu/rofi

;;; Code:

;; Counsel-Linux-App
;; Since we have counsel installed, we can use 'counsel-linux-app' to launch our Linux apps.  It list the apps by their executable command, so it's kind of tricky to use.

;; app-launcher is a local file, not a package
(use-package app-launcher
  :ensure nil)

(require 'app-launcher)

;;;###autoload
(defun app-launchers-counsel-launcher ()
  "Create and select a frame called emacs-counsel-launcher which consists only of a minibuffer and has specific dimensions. Runs counsel-linux-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
  (interactive)
  (with-selected-frame
      (make-frame '((name . "emacs-run-launcher")
                    (minibuffer . only)
                    (fullscreen . 0) ; no fullscreen
                    (undecorated . t) ; remove title bar
                    ;;(auto-raise . t) ; focus on this frame
                    ;;(tool-bar-lines . 0)
                    ;;(menu-bar-lines . 0)
                    (internal-border-width . 10)
                    (width . 80)
                    (height . 11)))
    (unwind-protect
        (counsel-linux-app)
      (delete-frame))))


;; App-Launcher
;; The 'app-launcher' is a better run launcher since it reads the desktop applications on your system and you can search them by their names as defined in their desktop file.  This means that sometimes you have to search for a generic term rather than the actual binary command of the program.

;; (use-package app-launcher
;; :ensure)
;; create a global keyboard shortcut with the following code
;; emacsclient -cF "((visibility . nil))" -e "(app-launchers-run-launcher)"

;;;###autoload
(defun app-launchers-run-launcher ()
  "Create and select a frame called emacs-run-launcher which consists only of a minibuffer and has specific dimensions. Runs app-launcher-run-app on that frame, which is an emacs command that prompts you to select an app and open it in a dmenu like behaviour. Delete the frame after that command has exited"
  (interactive)
  (with-selected-frame
      (make-frame '((name . "emacs-run-launcher")
                    (minibuffer . only)
                    (fullscreen . 0) ; no fullscreen
                    (undecorated . t) ; remove title bar
                    ;;(auto-raise . t) ; focus on this frame
                    ;;(tool-bar-lines . 0)
                    ;;(menu-bar-lines . 0)
                    (internal-border-width . 10)
                    (width . 80)
                    (height . 11)))
    (unwind-protect
        (let ((vertico-resize t)
              (vertico-count 20))
          (app-launcher-run-app))
      (delete-frame))))


(provide 'app-launchers)
;;; app-launchers.el ends here
