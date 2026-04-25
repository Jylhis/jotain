;;; init-ai.el --- AI assistants -*- lexical-binding: t; -*-

;;; Commentary:

;; AI tool hierarchy:
;;
;;   claude-code-ide  C-c C-'       Agentic editing — autonomous multi-file
;;                                  changes via the Claude Code CLI.
;;
;;   gptel            C-c RET       Send region/buffer to an LLM.
;;                    C-c M-RET     Full menu (model, backend, system prompt).
;;
;;   mcp              M-x mcp-connect-server + gptel-mcp-connect
;;                                  Model Context Protocol tool use via gptel.
;;
;; Auth: API keys come from the environment first
;; (ANTHROPIC_API_KEY / GEMINI_API_KEY) and fall back to auth-source —
;; auth-source-1password (configured in init-systems.el) makes that
;; transparent.

;;; Code:

(use-package claude-code-ide
  :ensure nil ; Provided by Nix
  :defer t
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

(use-package gptel
  :defer t
  :functions (gptel-make-anthropic gptel-make-gemini gptel-make-ollama)
  :bind
  (("C-c RET"   . gptel-send)
   ("C-c M-RET" . gptel-menu))
  :config
  ;; Anthropic (Claude) — primary backend.
  (setopt gptel-backend
          (gptel-make-anthropic "Claude"
            :stream t
            :key (lambda ()
                   (or (getenv "ANTHROPIC_API_KEY")
                       (auth-source-pick-first-password
                        :host "api.anthropic.com"
                        :user "apikey"))))
          gptel-model 'claude-sonnet-4-20250514)

  ;; Google Gemini — secondary backend.
  (gptel-make-gemini "Gemini"
    :stream t
    :key (lambda ()
           (or (getenv "GEMINI_API_KEY")
               (auth-source-pick-first-password
                :host "generativelanguage.googleapis.com"
                :user "apikey"))))

  ;; Ollama — local models, no key needed.
  (gptel-make-ollama "Ollama"
    :stream t
    :host "localhost:11434"
    :models '(llama3.1:latest)))

(use-package mcp
  :defer t
  :after gptel)

(provide 'init-ai)
;;; init-ai.el ends here
