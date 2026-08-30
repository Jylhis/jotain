;;; init-lang-systems.el --- Systems language modes: C/C++, CMake, Meson, Haskell, OCaml, Zig -*- lexical-binding: t; -*-

;;; Commentary:

;; Compiled/systems-programming modes. C/C++ use the built-in tree-sitter
;; modes (`c-ts-mode'/`c++-ts-mode'), routed via `jotain-prog-ts-remaps'
;; in `init-prog.el'; cc-mode is still built in (ensure nil) and owns the
;; `auto-mode-alist' decisions (which extensions are C vs C++) plus the
;; style used by the remaining cc-mode buffers. CUDA (`.cu'/`.cuh') has no
;; tree-sitter mode of its own, so it routes to `c++-ts-mode' (cpp grammar)
;; and picks up the whole C++ tool stack. cmake-mode, meson-mode,
;; haskell-mode, tuareg, dune, and zig-ts-mode come from MELPA. Go
;; graduated to its own file (`init-lang-go') once it grew workspace
;; config, helpers, and debugging.
;;
;; eglot wiring for zig (and rust/go, in their own files) is in
;; `init-prog.el'. If you add hooks for more servers, keep them there so
;; all LSP wiring is in one place. OCaml needs no per-mode wiring: eglot
;; already maps `tuareg-mode' to `ocamllsp' and apheleia maps it to
;; `ocamlformat', so the generic auto-start in `init-prog' lights up the
;; LSP and format-on-save whenever the tools are on PATH.

;;; Code:

;;; @doc Built-in cc-mode, kept for its `auto-mode-alist' decisions and as
;;; the grammarless fallback. The `:mode' list is the source of truth for
;;; which extensions are C vs C++ (headers and `.cu'/`.cuh' default to
;;; C++); `jotain-prog-ts-remaps' (init-prog.el) then remaps the chosen
;;; `c-mode'/`c++-mode' to `c-ts-mode'/`c++-ts-mode' whenever the grammar
;;; is loadable, so buffers actually open in tree-sitter. The
;;; `c-basic-offset'/`c-default-style' below still apply to the remaining
;;; cc-mode buffers (java/awk/other) and when a build lacks the C grammar.
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
         ("\\.txx\\'" . c++-mode)
         ;; CUDA — no tree-sitter major mode exists (Emacs bug#72388), so
         ;; treat `.cu'/`.cuh' as C++; the `cpp' remap sends them to
         ;; `c++-ts-mode' and the C++ tool stack (clangd, clang-format,
         ;; codelldb) applies. clangd reads `.cu' as CUDA by extension.
         ("\\.cu\\'"  . c++-mode)
         ("\\.cuh\\'" . c++-mode)))

;;; @doc Built-in tree-sitter C/C++ modes. C/C++ buffers land here via the
;;; `c'/`cpp' entries in `jotain-prog-ts-remaps' (init-prog.el); this block
;;; carries the shared indentation. `c-ts-mode-indent-offset' is 4 (matching
;;; the old cc-mode `c-basic-offset'); the style is `k&r' — the closest
;;; built-in tree-sitter style, since the previous Stroustrup style has no
;;; tree-sitter equivalent. eglot wires clangd and apheleia wires
;;; clang-format for both modes in init-prog.el.
(use-package c-ts-mode
  :ensure nil
  :defer t
  :custom
  (c-ts-mode-indent-offset 4)
  (c-ts-mode-indent-style 'k&r))

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

;;; @doc Tuareg — the standard OCaml major mode for `.ml`/`.mli` sources
;;; (OCaml has no built-in tree-sitter mode). Eglot already maps
;;; `tuareg-mode' to `ocamllsp' and apheleia maps it to `ocamlformat',
;;; so the generic eglot auto-start and format-on-save in init-prog pick
;;; OCaml up with no extra wiring. All OCaml tooling (ocamllsp,
;;; ocamlformat) comes from the project/host PATH, not this config.
(use-package tuareg
  :mode (("\\.mli?\\'" . tuareg-mode)
         ("\\.ml[ly]\\'" . tuareg-mode)))

;;; @doc Dune major mode for the OCaml build system's `dune`,
;;; `dune-project`, and `dune-workspace` files — the OCaml counterpart to
;;; the CMake/Meson modes above.
(use-package dune
  :mode (("/dune\\'" . dune-mode)
         ("/dune-project\\'" . dune-mode)
         ("/dune-workspace\\'" . dune-mode)))

;;; @doc Tree-sitter Zig mode (MELPA). Eglot wires zls in init-prog;
;;; format-on-save runs `zig fmt' through apheleia.
(use-package zig-ts-mode
  :mode "\\.zig\\'")

(provide 'init-lang-systems)
;;; init-lang-systems.el ends here
