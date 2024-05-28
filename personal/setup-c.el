(use-package make-mode
  :straight nil
  :init
  (add-to-list 'auto-mode-alist '("Makefile$" . makefile-mode))
  (add-to-list 'auto-mode-alist '("makefile$" . makefile-mode))
  :mode ("Makefile\\'" "makefile\\'" "Make.obj\\'"))


(use-package cmake-font-lock
  :straight t)

(use-package cmake-mode
  :straight t
  :mode
  ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  )
;;(use-package :rtags
;;  :straight t)
(use-package cmake-ide
  :straight t
 ;; :after rtags
  :init
  ;;(require 'rtags)
  (cmake-ide-setup))



(use-package modern-cpp-font-lock
  :straight t
  :hook c++-mode-hook
  )
(use-package google-c-style
  :straight t
  )

(setq-default c-basic-offset 4)

(add-hook 'c-mode-common-hook
          (lambda ()
            (company-mode)
            (make-local-variable 'standard-indent)
            (setq standard-indent 4)))
