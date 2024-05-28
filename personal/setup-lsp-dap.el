
;; lsp-mode
(use-package lsp-mode
  :straight t
  :defines lsp-language-id-configuration
  :hook (
         (lsp-mode . lsp-enable-which-key-integration)
	 ;;(terraform-mode . lsp)
	 (go-mode . lsp)
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (python-mode . lsp)
	 (haskell-mode . lsp)
	 (asm-mode . lsp)
	 (shell-script-mode . lsp)
	 (cmake-mode . lsp)
	 (dockerfile-mode . lsp)
	 (html-mode . lsp)
	 (javascript-mode . lsp)
	 (typescript-ts-mode . lsp)
	 (json-mode . lsp)
	 (markdown-mode . lsp)
	 (nix-mode . lsp)

	 )
  :commands lsp
  :custom
  (lsp-use-plists t)
  (gc-cons-threshold (* 100 1024 1024))
  (read-process-output-max (* 3 1024 1024))

  ;; (treemacs-space-between-root-nodes nil)

  (lsp-auto-guess-root t)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-before-save-edits nil)
  (lsp-idle-delay 0.3)
  (lsp-completion-provider :capf)
  ;; Prevent constant auto-formatting...)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-indentation nil)
  ;; be more ide-ish)
  (lsp-headerline-breadcrumb-enable t)
  ;; python-related settings)
  (lsp-pyls-plugins-autopep8-enabled nil)
  (lsp-pyls-plugins-yapf-enabled t)
  )
;; Taken from https://tychoish.com/post/emacs-and-lsp-mode/
(use-package lsp-ui
  :straight t
  :after (lsp-mode)
  :commands lsp-ui-doc-hide
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :custom
  (lsp-ui-doc-alignment 'at-point)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-doc-delay 0.3)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-use-childframe t)
  ;; (lsp-ui-doc-use-webkit nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-show-directory t)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-update-mode 'line)
  ;; :custom-face
  ;; (lsp-ui-peek-highlight ((t (:inherit nil :background nil :foreground nil :weight semi-bold :box (:line-width -1)))))
  :config
  ;; (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
                                   (face-background 'tooltip))))

  (defun lsp-update-server ()
    "Update LSP server."
    (interactive)
    ;; Equals to `C-u M-x lsp-install-server'
    (lsp-install-server t))
  )

;; Debug
(use-package dap-mode
  :straight t
  :defines dap-python-executable
  :functions dap-hydra/nil
  :diminish
  :after (lsp-mode)
  :functions dap-hydra/nil
  :custom
  (dap-auto-configure-mode t)
  (dap-tooltip-mode 1)
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  (dap-auto-configure-features
   '(sessions locals breakpoints expressions controls tooltip))
  :hook ((dap-mode . dap-ui-mode)
         (dap-session-created . (lambda (&_rest) (dap-hydra)))
         (dap-stopped . (lambda (_args) (dap-hydra)))
         (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))
         (python-mode . (lambda () (require 'dap-python)))
         (diff-mode . (lambda () (dap-mode -1)))
	 (go-mode . (lambda () (require 'dap-dlv-go)))
         (powershell-mode . (lambda () (dap-mode -1)))
         (shell-script-mode . (lambda () (dap-mode -1)))
         (cmake-mode . (lambda () (dap-mode -1)))
         ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
         (powershell-mode . (lambda () (require 'dap-pwsh))))
  :init
  (when (executable-find "python3")
    (setq dap-python-executable "python3"))
  (require 'dap-cpptools)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  )

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :straight t
  :commands lsp-treemacs-errors-list
  :init (lsp-treemacs-sync-mode 1)
  ;; :bind (:map lsp-mode-map
  ;;        ("M-9" . lsp-treemacs-errors-list))
  )
