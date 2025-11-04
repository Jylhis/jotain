;;; lang-cc.el --- C/C++/CUDA language configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Enhanced C/C++/CUDA development configuration with clangd LSP, modern C++ support,
;; CUDA integration, and comprehensive debugging following Doom/Spacemacs patterns.

;;; Code:

;; Enhanced C/C++ configuration building on cc-mode from programming.el
(with-eval-after-load 'cc-mode
  ;; Configure clangd as the preferred LSP server
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((c-mode c++-mode c-ts-mode c++-ts-mode cuda-mode) .
                   ("clangd"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--header-insertion=iwyu"
                    "--pch-storage=memory"
                    "--all-scopes-completion"
                    "--cross-file-rename"
                    ;; "--compile-commands-dir=build"  ; Uncomment if compile_commands.json is in build/
                    ))))

  ;; Helper function to find alternate file (header <-> source)
  (defun my/cc-find-alternate-file ()
    "Switch between header and source file."
    (interactive)
    (let* ((file (buffer-file-name))
           (ext (file-name-extension file))
           (base (file-name-sans-extension file))
           (alternates (cond
                       ((member ext '("cpp" "cc" "cxx" "c"))
                        (list ".h" ".hpp" ".hxx" ".hh"))
                       ((member ext '("h" "hpp" "hxx" "hh"))
                        (list ".cpp" ".cc" ".cxx" ".c"))
                       (t nil))))
      (if alternates
          (let ((found nil))
            (dolist (alt alternates)
              (let ((candidate (concat base alt)))
                (when (and (not found) (file-exists-p candidate))
                  (setq found candidate))))
            (if found
                (find-file found)
              (message "No alternate file found")))
        (message "Not a C/C++ file"))))

  ;; Debugging helper
  (defun my/cc-debug-file ()
    "Start GDB debugging session for current file."
    (interactive)
    (let* ((file (buffer-file-name))
           (base (file-name-sans-extension file))
           (executable (or (and (file-exists-p base) base)
                          (and (file-exists-p (concat base ".out"))
                               (concat base ".out"))
                          base)))
      (gdb (format "gdb -i=mi %s" executable))))

  ;; Major-mode keybindings following Doom/Spacemacs patterns
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps '(c-mode-map c++-mode-map c-ts-mode-map c++-ts-mode-map cuda-mode-map)
      "" '(:ignore t :which-key "c/c++/cuda")

      ;; Goto operations
      "g" '(:ignore t :which-key "goto")
      "gg" '(xref-find-definitions :which-key "definition")
      "gG" '(xref-find-definitions-other-window :which-key "definition other")
      "gr" '(xref-find-references :which-key "references")
      "gi" '(eglot-find-implementation :which-key "implementation")
      "ga" '(my/cc-find-alternate-file :which-key "alternate file")
      "gh" '(my/cc-find-alternate-file :which-key "header/source")
      "gb" '(xref-go-back :which-key "go back")
      "gf" '(xref-go-forward :which-key "go forward")

      ;; Help/Documentation
      "h" '(:ignore t :which-key "help")
      "hh" '(eldoc-doc-buffer :which-key "doc at point")

      ;; Compile
      "c" '(:ignore t :which-key "compile")
      "cc" '(compile :which-key "compile")
      "cC" '(recompile :which-key "recompile")
      "cx" '(compile-multi :which-key "compile multi")

      ;; Debug
      "d" '(:ignore t :which-key "debug")
      "dd" '(gdb :which-key "start gdb")
      "df" '(my/cc-debug-file :which-key "debug file")
      "dD" '(dape :which-key "start dape")

      ;; Format
      "=" '(eglot-format-buffer :which-key "format buffer")
      "==" '(eglot-format-buffer :which-key "format buffer")
      "=r" '(eglot-format :which-key "format region")

      ;; Refactor
      "r" '(:ignore t :which-key "refactor")
      "rr" '(eglot-rename :which-key "rename")

      ;; Actions
      "a" '(:ignore t :which-key "actions")
      "aa" '(eglot-code-actions :which-key "code actions")
      "aq" '(eglot-code-action-quickfix :which-key "quickfix"))))

;; CUDA support
(use-package cuda-mode
  :ensure
  :mode "\\.cu\\'"
  :hook (cuda-mode . eglot-ensure)
  :config
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps 'cuda-mode-map
      "" '(:ignore t :which-key "cuda")

      ;; Goto operations
      "g" '(:ignore t :which-key "goto")
      "gg" '(xref-find-definitions :which-key "definition")
      "gG" '(xref-find-definitions-other-window :which-key "definition other")
      "gr" '(xref-find-references :which-key "references")
      "ga" '(my/cc-find-alternate-file :which-key "alternate file")
      "gb" '(xref-go-back :which-key "go back")
      "gf" '(xref-go-forward :which-key "go forward")

      ;; Help/Documentation
      "h" '(:ignore t :which-key "help")
      "hh" '(eldoc-doc-buffer :which-key "doc at point")

      ;; Compile
      "c" '(:ignore t :which-key "compile")
      "cc" '(compile :which-key "compile")
      "cC" '(recompile :which-key "recompile")

      ;; Debug
      "d" '(:ignore t :which-key "debug")
      "dd" '(gdb :which-key "start gdb")

      ;; Format
      "=" '(eglot-format-buffer :which-key "format buffer")
      "==" '(eglot-format-buffer :which-key "format buffer")

      ;; Actions
      "a" '(:ignore t :which-key "actions")
      "aa" '(eglot-code-actions :which-key "code actions")
      "aq" '(eglot-code-action-quickfix :which-key "quickfix"))))

;; CMake integration
(use-package cmake-mode
  :ensure
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps 'cmake-mode-map
      "" '(:ignore t :which-key "cmake")

      "c" '(:ignore t :which-key "compile")
      "cc" '(compile :which-key "compile")
      "cC" '(recompile :which-key "recompile"))))

(provide 'lang-cc)
;;; lang-cc.el ends here
