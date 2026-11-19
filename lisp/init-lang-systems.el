;;; init-lang-systems.el --- Systems language modes: C/C++, CMake, Meson, Haskell, OCaml, Zig -*- lexical-binding: t; -*-

;;; Commentary:

;; Systems-language modes: C/C++, CUDA, CMake, Meson, Haskell, OCaml,
;; Dune, Zig, and GLSL.  C/C++ open in `c-ts-mode'/`c++-ts-mode'
;; through `treesit-enabled-modes' (init-prog.el); `.h' is pinned to C.
;; CUDA has no upstream tree-sitter mode, so this file defines
;; `cuda-ts-mode'.
;;
;; All LSP and formatter wiring lives in `init-prog.el'; keep new server
;; hooks there.

;;; Code:

(use-package cc-mode
  :ensure nil
  :mode (("\\.h\\'"   . c-mode)
         ("\\.tpp\\'" . c++-mode)
         ("\\.txx\\'" . c++-mode))
  :config
  (dolist (mode '(c-mode c++-mode))
    (add-to-list 'c-default-style (cons mode "stroustrup"))))

(use-package c-ts-mode
  :ensure nil
  :defer t
  :custom
  (c-ts-indent-offset 4)
  (c-ts-mode-indent-style 'k&r))

;;;; CUDA

;; `cuda-ts-mode' derives from `c-ts-base-mode' and reuses its font-lock
;; and indent builders, so load the library.
(require 'c-ts-mode)

;; CUDA has no upstream tree-sitter mode (Emacs bug#72388).  With the
;; `cuda' grammar (shipped with the distribution),
;; `treesit-language-remap-alist' resolves the `cpp'/`c' grammars to
;; `cuda', so c-ts-mode's cpp-tagged rules parse real CUDA while staying
;; labelled `cpp'.  Kernel launches parse as `call_expression'; the extra
;; rules only colour `<<<'/`>>>' and the `__device__'-family keywords.
;; Without the grammar it behaves as plain C++.
(define-derived-mode cuda-ts-mode c-ts-base-mode "CUDA"
  "Major mode for editing CUDA, powered by tree-sitter.

With the `cuda' grammar available this parses with the CUDA
grammar (a superset of C++); otherwise it falls back to the C++ grammar,
where the `<<<...>>>' launch syntax parses as an error node."
  :group 'c
  :after-hook (c-ts-mode-set-modeline)
  (when (treesit-ready-p 'cpp t)
    (let ((cuda-p (treesit-ready-p 'cuda t)))
      ;; Must be set before the parser is created or any query compiled.
      (when cuda-p
        (setq-local treesit-language-remap-alist '((cpp . cuda) (c . cuda))))
      (treesit-parser-create 'cpp)
      (setq-local syntax-propertize-function #'c-ts-mode--syntax-propertize)
      (setq-local treesit-simple-indent-rules
                  (if (functionp c-ts-mode-indent-style)
                      (funcall c-ts-mode-indent-style)
                    (c-ts-mode--simple-indent-rules 'cpp c-ts-mode-indent-style)))
      (setq-local treesit-font-lock-settings
                  (append
                   (c-ts-mode--font-lock-settings 'cpp)
                   ;; Compiled under the remap, where these CUDA-only tokens
                   ;; exist; skipped on the C++ fallback.
                   (when cuda-p
                     (treesit-font-lock-rules
                      :language 'cpp
                      :feature 'cuda-keyword
                      :override t
                      '(["__host__" "__device__" "__global__" "__managed__"
                         "__forceinline__" "__noinline__" "__launch_bounds__"
                         "__shared__" "__constant__" "__grid_constant__" "__local__"]
                        @font-lock-keyword-face)
                      :language 'cpp
                      :feature 'cuda-operator
                      :override t
                      '(["<<<" ">>>"] @font-lock-operator-face)))))
      ;; Append the CUDA features to the last (level 4) feature list.
      (when cuda-p
        (setq-local treesit-font-lock-feature-list
                    (append (butlast c-ts-mode--feature-list)
                            (list (append (car (last c-ts-mode--feature-list))
                                          '(cuda-keyword cuda-operator))))))
      (treesit-major-mode-setup))))

;; Count `cuda-ts-mode' as a `c++-ts-mode' for `derived-mode-p', so
;; apheleia, dape, tempel, and inlay-hint entries keyed on it match.
;; clangd treats a `.cu' buffer as CUDA by extension.
(derived-mode-add-parents 'cuda-ts-mode '(c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'"  . cuda-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-ts-mode))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package meson-mode
  :mode (("/meson\\.build\\'" . meson-mode)
         ("/meson_options\\.txt\\'" . meson-mode)
         ("/meson\\.options\\'" . meson-mode)))

(use-package haskell-mode
  :defer t)

(use-package haskell-ts-mode
  :mode ("\\.hs\\'" . haskell-ts-mode))

(use-package tuareg
  :mode ("\\.ml[ly]\\'" . tuareg-mode))

(use-package neocaml
  :mode (("\\.ml\\'"  . neocaml-mode)
         ("\\.mli\\'" . neocaml-interface-mode)))

(use-package dune
  :mode (("/dune\\'" . dune-mode)
         ("/dune-project\\'" . dune-mode)
         ("/dune-workspace\\'" . dune-mode)))

(use-package zig-ts-mode
  :mode "\\.\\(?:zig\\|zon\\)\\'")

;; Chosen over a tree-sitter mode because nixpkgs ships no `glsl-ts-mode'.
(use-package glsl-mode
  :defer t)

(provide 'init-lang-systems)
;;; init-lang-systems.el ends here
