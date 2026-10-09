;;; init-lang-systems.el --- Systems language modes: C/C++, CMake, Meson, Haskell, OCaml, Zig -*- lexical-binding: t; -*-

;;; Commentary:

;; Systems-language modes: C/C++, CUDA, CMake, Meson, Haskell, OCaml,
;; Dune, and Zig.  C/C++ open in `c-ts-mode'/`c++-ts-mode' via
;; `jotain-prog-ts-remaps' (init-prog.el); cc-mode decides which
;; extensions are C vs C++.  CUDA has no upstream tree-sitter mode, so
;; this file defines `cuda-ts-mode'.
;;
;; All LSP and formatter wiring lives in `init-prog.el'; keep new server
;; hooks there.

;;; Code:

;;; @doc Built-in cc-mode, kept for its `auto-mode-alist' decisions and as
;;; the grammarless fallback. The `:mode' list decides which extensions
;;; are C vs C++ (headers default to C++); `jotain-prog-ts-remaps'
;;; (init-prog.el) then remaps `c-mode'/`c++-mode' to the tree-sitter
;;; modes when the grammar loads. `c-basic-offset'/`c-default-style'
;;; apply to the remaining cc-mode buffers (Java, AWK, and C/C++ without
;;; a grammar).
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

;;; @doc Built-in tree-sitter C/C++ modes, reached via
;;; `jotain-prog-ts-remaps' (init-prog.el). Indent 4 to match cc-mode's
;;; `c-basic-offset'; style `k&r', the closest tree-sitter style to
;;; Stroustrup, which has no tree-sitter equivalent. clangd (eglot) and
;;; clang-format (apheleia) are wired in init-prog.el.
(use-package c-ts-mode
  :ensure nil
  :defer t
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'k&r))

;;;; CUDA

;; `cuda-ts-mode' derives from `c-ts-base-mode' and reuses its font-lock
;; and indent builders, so load the library.  `treesit-language-remap-alist'
;; does not exist on Emacs 30; declare it for the byte-compiler.
(require 'c-ts-mode)
(defvar treesit-language-remap-alist)

;; CUDA has no upstream tree-sitter mode (Emacs bug#72388).  On Emacs 31
;; with the `cuda' grammar (shipped with the distribution),
;; `treesit-language-remap-alist' resolves the `cpp'/`c' grammars to
;; `cuda', so c-ts-mode's cpp-tagged rules parse real CUDA while staying
;; labelled `cpp'.  Kernel launches parse as `call_expression'; the extra
;; rules only colour `<<<'/`>>>' and the `__device__'-family keywords.  On
;; Emacs 30 or without the grammar it behaves as plain C++.
(define-derived-mode cuda-ts-mode c-ts-base-mode "CUDA"
  "Major mode for editing CUDA, powered by tree-sitter.

On Emacs 31 with the `cuda' grammar available this parses with the CUDA
grammar (a superset of C++); otherwise it falls back to the C++ grammar,
where the `<<<...>>>' launch syntax parses as an error node."
  :group 'c
  :after-hook (c-ts-mode-set-modeline)
  (when (treesit-ready-p 'cpp t)
    (let ((cuda-p (and (boundp 'treesit-language-remap-alist)
                       (treesit-ready-p 'cuda t))))
      ;; Must be set before the parser is created or any query compiled.
      (when cuda-p
        (setq-local treesit-language-remap-alist '((cpp . cuda) (c . cuda))))
      (treesit-parser-create 'cpp)
      (setq-local syntax-propertize-function #'c-ts-mode--syntax-propertize)
      ;; The indent-rules builder is `c-ts-mode--get-indent-style' (MODE)
      ;; on Emacs 30 and `c-ts-mode--simple-indent-rules' (MODE STYLE) on
      ;; 31.  Calling via a quoted symbol keeps byte-compilation clean on
      ;; the version lacking the other.
      (setq-local treesit-simple-indent-rules
                  (cond
                   ((functionp c-ts-mode-indent-style)
                    (funcall c-ts-mode-indent-style))
                   ((fboundp 'c-ts-mode--simple-indent-rules)
                    (funcall 'c-ts-mode--simple-indent-rules 'cpp c-ts-mode-indent-style))
                   ((fboundp 'c-ts-mode--get-indent-style)
                    (funcall 'c-ts-mode--get-indent-style 'cpp))))
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

;;; @doc CMake mode for CMakeLists.txt and `.cmake` files, the fallback
;;; when `cmake-ts-mode` (via `jotain-prog-ts-remaps`) has no grammar.
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;;; @doc Meson mode for `meson.build`, `meson_options.txt`, and
;;; `meson.options`. Format-on-save runs `meson format` through apheleia
;;; (init-prog).
(use-package meson-mode
  :mode (("/meson\\.build\\'" . meson-mode)
         ("/meson_options\\.txt\\'" . meson-mode)
         ("/meson\\.options\\'" . meson-mode)))

;;; @doc Haskell major mode, loaded on demand. Kept as the parent of
;;; `haskell-ts-mode' below and for `.cabal'/`.lhs' files.
(use-package haskell-mode
  :defer t)

;;; @doc Tree-sitter Haskell mode (NonGNU ELPA) for `.hs' files. It
;;; derives from `haskell-mode', so eglot's built-in HLS entry matches it;
;;; HLS comes from the project PATH.
(use-package haskell-ts-mode
  :mode ("\\.hs\\'" . haskell-ts-mode))

;;; @doc Tuareg, kept for OCaml lexer/parser sources (`.mll'/`.mly'),
;;; which neocaml does not cover.
(use-package tuareg
  :mode ("\\.ml[ly]\\'" . tuareg-mode))

;;; @doc Tree-sitter OCaml via neocaml (MELPA): `neocaml-mode' for `.ml'
;;; and `neocaml-interface-mode' for `.mli'. ocamllsp (eglot) and
;;; ocamlformat (apheleia) are wired for both in init-prog, from the
;;; project PATH.
(use-package neocaml
  :mode (("\\.ml\\'"  . neocaml-mode)
         ("\\.mli\\'" . neocaml-interface-mode)))

;;; @doc Major mode for the OCaml build system's `dune`, `dune-project`,
;;; and `dune-workspace` files.
(use-package dune
  :mode (("/dune\\'" . dune-mode)
         ("/dune-project\\'" . dune-mode)
         ("/dune-workspace\\'" . dune-mode)))

;;; @doc Tree-sitter Zig mode (MELPA). eglot's built-in entry starts
;;; zls; format-on-save runs `zig fmt' through apheleia (init-prog).
(use-package zig-ts-mode
  :mode "\\.zig\\'")

(provide 'init-lang-systems)
;;; init-lang-systems.el ends here
