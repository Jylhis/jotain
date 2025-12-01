;;; programming.el --- Programming and development configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Programming tools including LSP, tree-sitter, debugging, and language modes.
;;; Code:

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . display-line-numbers-mode))

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure
  :custom
  ;; Tree-sitter grammars are provided by Nix and configured in early-init.el
  ;; Grammar path: TREE_SITTER_DIR -> treesit-extra-load-path (see early-init.el)
  ;; Grammar definitions: nix/lib/runtime-deps.nix
  ;; PATH setup: emacs.nix wraps Emacs with runtime dependencies
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold
  :disabled
  :diminish
  :ensure
  :hook (after-init . global-treesit-fold-indicators-mode)
  :init (setq treesit-fold-indicators-priority -1))

(use-package flymake
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  ;; (flymake-show-diagnostics-at-end-of-line t) ; FIXME
  (flymake-margin-indicators-string '((error "!" compilation-error)
                                      (warning "?" compilation-warning)
                                      (note "·" compilation-info)))
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! p" . flymake-show-project-diagnostics))
  :config
  ;; Show diagnostics in echo area when cursor is on an error
  (defun j10s/flymake-show-diagnostic-at-point ()
    "Display flymake diagnostic at point in echo area."
    (when (and flymake-mode (not (minibufferp)))
      (let ((diagnostics (flymake-diagnostics (point))))
        (when diagnostics
          (let ((diagnostic (car diagnostics)))
            (message "%s: %s"
                     (propertize (upcase (symbol-name (flymake-diagnostic-type diagnostic)))
                                 'face (pcase (flymake-diagnostic-type diagnostic)
                                         (:error 'error)
                                         (:warning 'warning)
                                         (_ 'default)))
                     (flymake-diagnostic-text diagnostic)))))))

  ;; Show diagnostic after a short delay
  (defvar-local j10s/flymake-diagnostic-timer nil)
  (defun j10s/flymake-show-diagnostic-delayed ()
    "Show diagnostic after a delay."
    (when flymake-mode
      (when j10s/flymake-diagnostic-timer
        (cancel-timer j10s/flymake-diagnostic-timer))
      (setq j10s/flymake-diagnostic-timer
            (run-with-timer 0.5 nil #'j10s/flymake-show-diagnostic-at-point))))

  (add-hook 'flymake-mode-hook
            (lambda ()
              (if flymake-mode
                  (add-hook 'post-command-hook #'j10s/flymake-show-diagnostic-delayed nil t)
                (remove-hook 'post-command-hook #'j10s/flymake-show-diagnostic-delayed t))))

  ;; Configure elisp-flymake-byte-compile to trust local configuration files
  (with-eval-after-load 'elisp-mode
    ;; Trust files in the user-emacs-directory for byte-compilation checks
    (setq elisp-flymake-byte-compile-load-path
          (append elisp-flymake-byte-compile-load-path (list user-emacs-directory)))

    ;; Add hook to trust local config files
    (defun j10s/trust-local-elisp-files ()
      "Trust elisp files in the current Emacs configuration directory."
      (when (and buffer-file-name
                 (string-prefix-p (expand-file-name user-emacs-directory)
                                  (expand-file-name buffer-file-name)))
        ;; Mark buffer as safe for byte-compilation
        (setq-local safe-local-variable-values
                    (append safe-local-variable-values
                            '((elisp-flymake-byte-compile . t))))))

    (add-hook 'emacs-lisp-mode-hook #'j10s/trust-local-elisp-files))

  ;; Disable flymake during smerge mode
  ;; (add-hook 'smerge-mode-hook
  ;;           (lambda ()
  ;;             (when (bound-and-true-p flymake-mode)
  ;;               (flymake-mode -1))))

  ;; (add-hook 'smerge-mode-exit-hook
  ;;           (lambda ()
  ;;             (when (derived-mode-p 'prog-mode)
  ;;               (flymake-mode 1))))
  )

(use-package flyspell
  :ensure nil
  :hook
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  :bind (:map flyspell-mode-map
              ("C-M-i" . nil)))  ; Unbind to allow completion-at-point

(use-package consult-flyspell
  :ensure
  :defer t
  :after (consult flyspell))

(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'makefile-mode 'snippet-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure)
	 ;; (eglot-mode . sideline-mode)
	 )
  :init
  (setq eglot-send-changes-idle-time 0.5)
  (setq eglot-autoshutdown t)
  (setq eglot-events-buffer-size 0) ; Disable event logging for performance
  :custom
  (eglot-report-progress nil "Prevent Eglot minibuffer spam")
  (eglot-extend-to-xref t "Activate Eglot in cross-referenced non-project files")
  (eglot-confirm-server-initiated-edits nil) ; Auto-accept server edits
  :bind (:map eglot-mode-map
              ("C-c r r" . eglot-rename)
              ("C-c r f" . eglot-format)
              ("C-c r a" . eglot-code-actions)
              ("C-c r o" . eglot-code-action-organize-imports)
              ("C-c r q" . eglot-code-action-quickfix)
              ("C-h ." . eldoc-doc-buffer))
  :config
  ;; Remove redundant flymake activation - Eglot does this automatically
  (add-hook 'eglot-managed-mode-hook #'eldoc-mode)

  ;; Enable inlay hints for supported languages (Emacs 29+)
  (when (fboundp 'eglot-inlay-hints-mode)
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (when (member major-mode '(go-mode go-ts-mode
						   rust-mode rust-ts-mode
						   typescript-mode typescript-ts-mode
						   python-mode python-ts-mode))
                  (eglot-inlay-hints-mode 1)))))

  ;; Configure server-specific settings
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls")))

  ;; Optimize for better performance
  (fset #'jsonrpc--log-event #'ignore)) ; Disable JSON-RPC event logging

(use-package consult-eglot
  :ensure
  :defer t
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)))

(use-package consult-eglot-embark
  :ensure
  :defer t
  :after (consult eglot embark))

(use-package xref
  :ensure nil
  :init
  ;; Ripgrep is guaranteed to be available via Nix (see nix/lib/runtime-deps.nix)
  (setq xref-search-program 'ripgrep))

(use-package elisp-lint
  :defer t
  :commands (elisp-lint-file elisp-lint-buffer))
(use-package package-lint
  :defer t
  :commands (package-lint-current-buffer package-lint-buffer))

(use-package dtrt-indent
  :defer t
  :diminish
  :hook (prog-mode . dtrt-indent-mode)
  )

(use-package direnv
  :ensure
  :demand t
  ;; Direnv is guaranteed to be available via Nix (see nix/lib/runtime-deps.nix)
  :config
  (direnv-mode)
  (add-to-list 'warning-suppress-types '(direnv)))

;; Debugging
(use-package gdb-mi
  :ensure nil
  :custom
  (gdb-many-windows t)
  (gdb-show-main t)
  (gdb-debuginfod-enable-setting t))

(use-package dape
  :ensure
  :defer t
  :commands (dape dape-breakpoint-toggle)
  :custom
  (dape-buffer-window-arrangement 'gud)
  (dape-info-hide-mode-line nil)
  (dape-inlay-hints t "Showing inlay hints")

  :init
  (with-eval-after-load 'dape
    (dape-breakpoint-global-mode)
    (add-hook 'dape-compile-hook 'kill-buffer)))

(use-package wgrep
  :ensure
  :defer t
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

;; Major modes
(use-package gnuplot
  :ensure
  :mode ("\\.plt\\'" . gnuplot-mode))

(use-package markdown-mode
  :after dash
  :ensure
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown")
  :hook (markdown-mode . visual-line-mode)
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package cc-mode
  :ensure nil
  :custom
  (c-basic-indent 5)
  (c-basic-offset 5)
  (c-default-style '((c-mode . "stroustrup")
                     (c++-mode . "stroustrup")
                     (java-mode . "java")
                     (awk-mode . "awk")
                     (other . "gnu")))
  :mode (("\\.h\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.hxx\\'" . c++-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.tpp\\'" . c++-mode)
         ("\\.txx\\'" . c++-mode)))

(use-package conf-mode
  :ensure nil
  :defer t
  :mode
  (("/.dockerignore\\'" . conf-unix-mode)
   ("/.gitignore\\'" . conf-unix-mode)))

(use-package cuda-mode)
(use-package haskell-mode)
(use-package diff-mode
  :ensure
  :defer t
  :mode "\\.patch[0-9]*\\'")
(use-package terraform-mode
  :defer
  :mode "\\.tf\\'")

(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package gitlab-ci-mode)
(use-package ansible
  :defer t)
(use-package ssh-config-mode)
(use-package adoc-mode
  :defer t
  :mode "\\.adoc\\'")
(use-package go-mode)
(use-package nix-mode)
(use-package nix-ts-mode :ensure :mode "\\.nix\\'")
(use-package cmake-mode
  :ensure
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))
(use-package mermaid-mode)
(use-package yaml-mode
  :defer t
  )
(use-package modern-cpp-font-lock :ensure
  :hook (c++-mode . modern-c++-font-lock-mode))
(use-package just-mode)
(use-package demangle-mode
  :ensure
  :hook asm-mode)
(use-package sql-indent)

(use-package web-mode
  :ensure
  :mode (("\\.html?\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode))
  :bind (:map web-mode-map
              ("M-o" . other-window))  ; Fix Alt+o window switching
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t))

(use-package xt-mouse
  :custom
  (xterm-mouse-mode 1 "Enable mouse in terminal"))

;; vterm requires libvterm which is not available on macOS
;; Also disabled on Android for performance/compatibility
(platform-unless (or platform-macos-p platform-android-p)
		 (use-package vterm
		   :ensure
		   :defer t
		   :commands (vterm vterm-other-window)
		   :custom
		   (vterm-always-compile-module t)))

(use-package editorconfig
  :defer t
  :diminish
  :hook (prog-mode . editorconfig-mode))

;; Enhanced error display using built-in features
(use-package eldoc
  :ensure nil
  :custom
  (eldoc-idle-delay 0.5)
  (eldoc-print-after-edit t)
  (eldoc-echo-area-display-truncation-message nil))

;; Enhanced tooltip support for better diagnostic display
(when (display-graphic-p)
  (tooltip-mode 1)
  (setq tooltip-delay 0.5
        tooltip-short-delay 0.1
        tooltip-recent-seconds 1
        tooltip-hide-delay 10))

;; Future enhancement options (commented out - packages not in nixpkgs):
;; For even better diagnostic display, consider these packages via straight.el or melpa:
;; - flymake-diagnostic-at-point: Shows diagnostics in minibuffer/tooltip

(provide 'programming)
;;; programming.el ends here
