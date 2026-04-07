;;; init-lang-systems.el --- Systems language modes: Go, C/C++, CMake, Haskell -*- lexical-binding: t; -*-

;;; Commentary:

;; Compiled/systems-programming modes. Go and cc-mode are built in
;; (ensure nil); cmake-mode and haskell-mode come from MELPA.
;;
;; eglot wiring for go + rust is in `init-prog.el'. If you add hooks
;; for more servers, keep them there so all LSP wiring is in one place.

;;; Code:

(use-package go-mode
  :defer t)

(use-package cc-mode
  :ensure nil
  :custom
  (c-basic-offset 4)
  (c-default-style
   '((c-mode   . "stroustrup")
     (c++-mode . "stroustrup")
     (java-mode . "java")
     (awk-mode  . "awk")
     (other     . "gnu")))
  :mode (("\\.h\\'"   . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.hxx\\'" . c++-mode)
         ("\\.cc\\'"  . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.tpp\\'" . c++-mode)
         ("\\.txx\\'" . c++-mode)))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package haskell-mode
  :defer t)

(provide 'init-lang-systems)
;;; init-lang-systems.el ends here
