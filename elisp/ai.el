;;; ai.el --- AI tools and assistants configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; AI coding assistants and LLM integrations.

;;; Code:

;; AI tool hierarchy:
;;
;;   claude-code-ide  C-c C-'       Agentic editing — autonomous multi-file
;;                                  code changes via Claude Code CLI.
;;
;;   gptel            C-c RET       Quick LLM queries and inline rewrites.
;;                    C-c M-RET     Full gptel menu (model, backend, options).
;;
;;   minuet           C-c A s       Toggle inline completion suggestions
;;                                  (off by default, Claude backend).
;;
;;   mcp              M-x mcp-connect-server + gptel-mcp-connect
;;                                  Model Context Protocol tool use via gptel.

(use-package claude-code-ide
  :ensure t
  :defer t
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

(use-package gptel
  :ensure t
  :defer t
  :bind (("C-c RET"   . gptel-send)
         ("C-c M-RET" . gptel-menu))
  :config
  ;; Anthropic (Claude) — primary backend
  (setopt gptel-backend
          (gptel-make-anthropic "Claude"
				:stream t
				:key (lambda ()
				       (or (getenv "ANTHROPIC_API_KEY")
					   (auth-source-pick-first-password
					    :host "api.anthropic.com"
					    :user "apikey"))))
          gptel-model 'claude-sonnet-4-20250514)

  ;; Google Gemini — secondary backend
  (gptel-make-gemini "Gemini"
		     :stream t
		     :key (lambda ()
			    (or (getenv "GEMINI_API_KEY")
				(auth-source-pick-first-password
				 :host "generativelanguage.googleapis.com"
				 :user "apikey"))))

  ;; Ollama — local models, no key needed
  (gptel-make-ollama "Ollama"
		     :stream t
		     :host "localhost:11434"
		     :models '(llama3.1:latest)))

(use-package minuet
  :ensure t
  :defer t
  :bind ("C-c A s" . minuet-auto-suggestion-mode)
  :custom
  (minuet-provider 'claude)
  (minuet-auto-suggestion-debounce-delay 0.4))

(use-package mcp
  :ensure t
  :defer t
  :after gptel)
;; Activation: M-x mcp-connect-server, then M-x gptel-mcp-connect.
;; No auto-connect — servers are connected on demand.

(provide 'ai)
;;; ai.el ends here
