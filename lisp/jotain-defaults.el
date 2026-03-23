;;; jotain-defaults.el --- Sensible Emacs defaults -*- lexical-binding: t; -*-

;; Author: Markus Jylhänkangas <markus@jylhis.com>

;;; Commentary:
;; Extends Emacs default settings with sensible values.
;; Version check ensures minimum Emacs 30 requirement.
;; Dependencies: none (this is a foundational module).

;;; Code:

;; Version check — warn early if Emacs is too old
(when (< emacs-major-version 30)
  (display-warning 'jotain
                   (format "Jotain requires Emacs 30+, you are running %s. Some features will not work."
                           emacs-version)
                   :warning))

;; Custom file — keep customizations out of init.el
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file t))

(provide 'jotain-defaults)
;;; jotain-defaults.el ends here
