;;; lang-assembly.el --- Assembly language configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Assembly development configuration with nasm-mode, x86 instruction lookup,
;; and comprehensive keybindings following Doom/Spacemacs patterns.

;;; Code:

(use-package nasm-mode
  :if (locate-library "nasm-mode")
  :mode (("\\.asm\\'" . nasm-mode)
         ("\\.nasm\\'" . nasm-mode))
  :custom
  (nasm-basic-offset 8)
  :config
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps 'nasm-mode-map
      "" '(:ignore t :which-key "assembly")

      ;; Help/Documentation
      "h" '(:ignore t :which-key "help")
      "hh" '(x86-lookup :which-key "x86 instruction lookup")

      ;; Compile
      "c" '(:ignore t :which-key "compile")
      "cc" '(compile :which-key "compile")
      "cC" '(recompile :which-key "recompile"))))

;; x86 instruction lookup
(use-package x86-lookup
  :if (locate-library "x86-lookup")
  :commands x86-lookup
  :custom
  (x86-lookup-pdf nil "Use online documentation if PDF not available"))

;; GAS/AT&T syntax support
(use-package asm-mode
  :mode (("\\.s\\'" . asm-mode)
         ("\\.S\\'" . asm-mode))
  :config
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps 'asm-mode-map
      "" '(:ignore t :which-key "assembly")

      ;; Compile
      "c" '(:ignore t :which-key "compile")
      "cc" '(compile :which-key "compile")
      "cC" '(recompile :which-key "recompile"))))

(provide 'lang-assembly)
;;; lang-assembly.el ends here
