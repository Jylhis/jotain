(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package corfu
  :ensure t
  :demand t
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :config
  (global-corfu-mode)
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  (require 'corfu-history)
  (corfu-history-mode))

(print "Test successful")
