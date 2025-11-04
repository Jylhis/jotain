;;; lang-build.el --- Build system configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Build system configuration for Make, CMake, Autotools, and Justfiles
;; with comprehensive keybindings following Doom/Spacemacs patterns.

;;; Code:

;; Makefile support
(use-package make-mode
  :mode (("Makefile\\'" . makefile-gmake-mode)
         ("\\.mk\\'" . makefile-gmake-mode)
         ("GNUmakefile\\'" . makefile-gmake-mode))
  :config
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps 'makefile-mode-map
      "" '(:ignore t :which-key "makefile")

      ;; Build operations
      "c" '(:ignore t :which-key "compile")
      "cc" '(compile :which-key "make")
      "cC" '(recompile :which-key "remake")
      "ct" '((lambda () (interactive) (compile "make test")) :which-key "make test")
      "ci" '((lambda () (interactive) (compile "make install")) :which-key "make install")
      "cd" '((lambda () (interactive) (compile "make clean")) :which-key "make clean")

      ;; Help
      "h" '(:ignore t :which-key "help")
      "hh" '((lambda () (interactive) (compile "make help")) :which-key "make help"))))

;; Extended CMake support (already has basic in lang-cc.el)
(with-eval-after-load 'cmake-mode
  (defun my/cmake-configure ()
    "Run cmake configuration."
    (interactive)
    (compile "cmake -B build"))

  (defun my/cmake-build ()
    "Build with cmake."
    (interactive)
    (compile "cmake --build build"))

  (defun my/cmake-test ()
    "Run cmake tests."
    (interactive)
    (compile "cd build && ctest"))

  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps 'cmake-mode-map
      "" '(:ignore t :which-key "cmake")

      ;; Configure
      "c" '(:ignore t :which-key "configure")
      "cc" '(my/cmake-configure :which-key "configure")
      "cb" '(my/cmake-build :which-key "build")
      "ct" '(my/cmake-test :which-key "test")

      ;; Build operations
      "b" '(:ignore t :which-key "build")
      "bb" '(my/cmake-build :which-key "build")
      "bc" '(my/cmake-configure :which-key "configure")
      "bt" '(my/cmake-test :which-key "test"))))

;; Autotools support (configure.ac, Makefile.am)
(use-package autoconf-mode
  :mode (("configure\\.ac\\'" . autoconf-mode)
         ("configure\\.in\\'" . autoconf-mode))
  :config
  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps 'autoconf-mode-map
      "" '(:ignore t :which-key "autoconf")

      ;; Generate configure script
      "c" '(:ignore t :which-key "configure")
      "cg" '((lambda () (interactive) (compile "autoconf")) :which-key "generate")
      "ca" '((lambda () (interactive) (compile "autoreconf -i")) :which-key "autoreconf")
      "cc" '((lambda () (interactive) (compile "./configure")) :which-key "run configure"))))

;; Automake support
(use-package automake-mode
  :if (locate-library "automake-mode")
  :mode ("Makefile\\.am\\'" . automake-mode))

;; Justfile support
(use-package just-mode
  :ensure
  :mode (("justfile\\'" . just-mode)
         ("\\.just\\'" . just-mode))
  :config
  (defun my/just-run-recipe ()
    "Run a just recipe."
    (interactive)
    (let ((recipe (read-string "Just recipe: ")))
      (compile (format "just %s" recipe))))

  (defun my/just-list ()
    "List all just recipes."
    (interactive)
    (compile "just --list"))

  (with-eval-after-load 'keybindings
    (my/local-leader-def
      :keymaps 'just-mode-map
      "" '(:ignore t :which-key "just")

      ;; Run operations
      "r" '(:ignore t :which-key "run")
      "rr" '(my/just-run-recipe :which-key "run recipe")
      "rl" '(my/just-list :which-key "list recipes")

      ;; Common recipes (customize based on your justfile)
      "c" '(:ignore t :which-key "common")
      "cb" '((lambda () (interactive) (compile "just build")) :which-key "build")
      "ct" '((lambda () (interactive) (compile "just test")) :which-key "test")
      "cc" '((lambda () (interactive) (compile "just check")) :which-key "check")
      "cC" '((lambda () (interactive) (compile "just clean")) :which-key "clean"))))

;; Tree-sitter mode for just files
(use-package just-ts-mode
  :if (and (locate-library "just-ts-mode") (treesit-available-p))
  :mode (("justfile\\'" . just-ts-mode)
         ("\\.just\\'" . just-ts-mode)))

(provide 'lang-build)
;;; lang-build.el ends here
