;;; init-lang-systems.el --- Systems language modes: C/C++, CMake, Meson, Haskell, OCaml, Zig -*- lexical-binding: t; -*-

;;; Commentary:

;; Compiled/systems-programming modes. C/C++ use the built-in tree-sitter
;; modes (`c-ts-mode'/`c++-ts-mode'), routed via `jotain-prog-ts-remaps'
;; in `init-prog.el'; cc-mode is still built in (ensure nil) and owns the
;; `auto-mode-alist' decisions (which extensions are C vs C++) plus the
;; style used by the remaining cc-mode buffers. CUDA (`.cu'/`.cuh') has no
;; upstream tree-sitter mode, so this file defines `cuda-ts-mode' on the
;; `cuda' grammar (a C++ superset) and, via `derived-mode-add-parents', it
;; inherits the whole C++ tool stack. cmake-mode, meson-mode,
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
;;; which extensions are C vs C++ (headers default to C++);
;;; `jotain-prog-ts-remaps' (init-prog.el) then remaps the chosen
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
         ("\\.txx\\'" . c++-mode)))

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

;;;; CUDA

;; `cuda-ts-mode' is built on `c-ts-mode' internals, so load the library
;; here (the mode derives from `c-ts-base-mode' and reuses its font-lock /
;; indent builders).  Forward-declare `treesit-language-remap-alist' so the
;; byte-compiler stays quiet on the Emacs 30 floor where it does not exist
;; (same idiom as the `treesit-extra-load-path' declaration in init-prog).
(require 'c-ts-mode)
(defvar treesit-language-remap-alist)

;; Tree-sitter CUDA mode, built on the `cuda' grammar (shipped with the
;; distribution). CUDA has no upstream tree-sitter major mode (Emacs
;; bug#72388), so this derives from `c-ts-base-mode' and, on Emacs 31,
;; aliases the `cpp'/`c' grammars to `cuda' via `treesit-language-remap-alist'
;; — that makes every one of `c-ts-mode's cpp-tagged font-lock and indent
;; rules parse and highlight real CUDA (kernel launches, `__global__', …)
;; while staying labelled `cpp'. Kernel launches parse as `call_expression',
;; so they inherit the C++ rules; the extra rules only colour the CUDA-only
;; `<<<'/`>>>' and `__device__'/`__shared__'-family tokens. On Emacs 30 or a
;; build without the `cuda' grammar it degrades to plain C++ behaviour. Via
;; `derived-mode-add-parents' the mode counts as `c++-ts-mode', so the C++
;; tool stack (clangd, clang-format, codelldb, inlay hints, tempel snippets,
;; folding) applies with no extra per-mode wiring. clangd reads a `.cu'
;; buffer as CUDA by extension.
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
      ;; Must be set BEFORE the parser is created or any query compiled: it
      ;; resolves every `cpp'/`c' grammar request to `cuda' while keeping the
      ;; parser/query language labelled `cpp' (so c-ts-mode's cpp rules match).
      (when cuda-p
        (setq-local treesit-language-remap-alist '((cpp . cuda) (c . cuda))))
      (treesit-parser-create 'cpp)
      (setq-local syntax-propertize-function #'c-ts-mode--syntax-propertize)
      ;; The indent-rules builder was renamed between Emacs 30
      ;; (`c-ts-mode--get-indent-style', MODE only) and Emacs 31
      ;; (`c-ts-mode--simple-indent-rules', MODE + STYLE). Dispatch on
      ;; `fboundp' and call via a quoted symbol so byte-compilation stays
      ;; clean on whichever version lacks the other.
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
                   ;; Compiled here, under the remap, so `:language cpp' resolves
                   ;; to the cuda grammar and these cuda-only anonymous tokens
                   ;; exist; skipped on the fallback path where they would not.
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
      ;; Enable the two extra features at level 4 (the config's font-lock
      ;; level) by appending them to c-ts-mode's last feature-list level.
      (when cuda-p
        (setq-local treesit-font-lock-feature-list
                    (append (butlast c-ts-mode--feature-list)
                            (list (append (car (last c-ts-mode--feature-list))
                                          '(cuda-keyword cuda-operator))))))
      (treesit-major-mode-setup))))

;; Treat `cuda-ts-mode' as a `c++-ts-mode' for `derived-mode-p', so eglot,
;; apheleia, dape and tempel entries keyed on `c++-ts-mode' all match it.
(derived-mode-add-parents 'cuda-ts-mode '(c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'"  . cuda-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . cuda-ts-mode))

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
