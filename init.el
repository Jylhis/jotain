;;; init.el --- Jotain Emacs configuration -*- lexical-binding: t; -*-

;;; Code:

(require 'package)
(setq package-archives nil)
(setq package-archive-priorities nil)
(setq package-vc-heuristic-alist nil)
(package-initialize)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

(provide 'init)
;;; init.el ends here
