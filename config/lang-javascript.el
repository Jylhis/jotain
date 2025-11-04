;;; lang-javascript.el --- JavaScript/TypeScript language configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; JavaScript and TypeScript development configuration with typescript-language-server,
;; Node.js REPL support, and comprehensive keybindings following Doom/Spacemacs patterns.

;;; Code:

(use-package js-mode
  :mode (("\\.js\\'" . js-mode)
         ("\\.mjs\\'" . js-mode)
         ("\\.cjs\\'" . js-mode))
  :hook (js-mode . eglot-ensure)
  :custom
  (js-indent-level 2)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(js-mode . ("typescript-language-server" "--stdio")))))

(use-package typescript-mode
  :if (locate-library "typescript-mode")
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :hook (typescript-mode . eglot-ensure)
  :custom
  (typescript-indent-level 2)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((typescript-mode) . ("typescript-language-server" "--stdio")))))

;; Tree-sitter modes if available
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

;; Helper functions
(defun my/js-run-file ()
  "Run current JavaScript file with Node.js."
  (interactive)
  (compile (format "node %s" (buffer-file-name))))

(defun my/js-test-file ()
  "Run Jest tests for current file."
  (interactive)
  (compile (format "npm test -- %s" (buffer-file-name))))

(defun my/js-test-project ()
  "Run all Jest tests."
  (interactive)
  (compile "npm test"))

(defun my/js-npm-run ()
  "Run npm script."
  (interactive)
  (let ((script (read-string "npm run script: ")))
    (compile (format "npm run %s" script))))

;; Shared keybindings for JS/TS modes
(with-eval-after-load 'keybindings
  (dolist (map '(js-mode-map js-ts-mode-map typescript-mode-map typescript-ts-mode-map tsx-ts-mode-map))
    (my/local-leader-def
      :keymaps map
      "" '(:ignore t :which-key "javascript/typescript")

      ;; Goto operations
      "g" '(:ignore t :which-key "goto")
      "gg" '(xref-find-definitions :which-key "definition")
      "gG" '(xref-find-definitions-other-window :which-key "definition other")
      "gr" '(xref-find-references :which-key "references")
      "gi" '(eglot-find-implementation :which-key "implementation")
      "gb" '(xref-go-back :which-key "go back")
      "gf" '(xref-go-forward :which-key "go forward")

      ;; Help/Documentation
      "h" '(:ignore t :which-key "help")
      "hh" '(eldoc-doc-buffer :which-key "doc at point")

      ;; Testing
      "t" '(:ignore t :which-key "test")
      "tt" '(my/js-test-file :which-key "test file")
      "ta" '(my/js-test-project :which-key "test all")

      ;; Run
      "r" '(:ignore t :which-key "run")
      "rr" '(my/js-run-file :which-key "run file")
      "rn" '(my/js-npm-run :which-key "npm run")

      ;; NPM operations
      "n" '(:ignore t :which-key "npm")
      "ni" '((lambda () (interactive) (compile "npm install")) :which-key "install")
      "nu" '((lambda () (interactive) (compile "npm update")) :which-key "update")
      "nr" '(my/js-npm-run :which-key "run script")
      "nb" '((lambda () (interactive) (compile "npm run build")) :which-key "build")

      ;; Imports
      "i" '(:ignore t :which-key "imports")
      "io" '(eglot-code-action-organize-imports :which-key "organize imports")

      ;; Format
      "=" '(eglot-format-buffer :which-key "format buffer")
      "==" '(eglot-format-buffer :which-key "format buffer")
      "=r" '(eglot-format :which-key "format region")

      ;; Refactor
      "R" '(:ignore t :which-key "refactor")
      "Rr" '(eglot-rename :which-key "rename")

      ;; Actions
      "a" '(:ignore t :which-key "actions")
      "aa" '(eglot-code-actions :which-key "code actions")
      "aq" '(eglot-code-action-quickfix :which-key "quickfix"))))

(provide 'lang-javascript)
;;; lang-javascript.el ends here
