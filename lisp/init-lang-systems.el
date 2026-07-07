;;; init-lang-systems.el --- Systems language modes: C/C++, CMake, Meson, Haskell, Zig -*- lexical-binding: t; -*-

;;; Commentary:

;; Compiled/systems-programming modes. cc-mode is built in (ensure nil);
;; cmake-mode, meson-mode, haskell-mode, and zig-ts-mode come from MELPA.
;; Go graduated to its own file (`init-lang-go') once it grew workspace
;; config, helpers, and debugging.
;;
;; eglot wiring for zig (and rust/go, in their own files) is in
;; `init-prog.el'. If you add hooks for more servers, keep them there so
;; all LSP wiring is in one place.

;;; Code:

;;; @doc Built-in C/C++ mode with a Stroustrup style bias. The header
;;; extensions below default to C++ — Jotain assumes that's the
;;; more common case in modern repos.
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

;;; @doc CMake mode for CMakeLists.txt and `.cmake` files. Mode regex
;;; covers both file conventions.
(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;;; @doc Meson mode for `meson.build`, `meson_options.txt`, and
;;; `meson.options` files. Formatting is configured centrally through
;;; apheleia, using the Meson CLI supplied by the dev shell.
(use-package meson-mode
  :mode (("/meson\\.build\\'" . meson-mode)
         ("/meson_options\\.txt\\'" . meson-mode)
         ("/meson\\.options\\'" . meson-mode)))

;;; @doc Haskell major mode. Loaded on demand only — keeps the rare
;;; Haskell editing session from costing every Emacs start.
(use-package haskell-mode
  :defer t)

;;; @doc Tree-sitter Zig mode (MELPA). Eglot wires zls in init-prog;
;;; format-on-save runs `zig fmt' through apheleia.
(use-package zig-ts-mode
  :mode "\\.zig\\'")

(provide 'init-lang-systems)
;;; init-lang-systems.el ends here
