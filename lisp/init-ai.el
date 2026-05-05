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

;;; @doc Agentic multi-file editing through the Claude Code CLI. Bound to
;;; @doc C-c C-' so the menu is one key away whenever a refactor needs more
;;; @doc context than a single LSP rename can carry. Provided by Nix
;;; @doc (manzaltu/claude-code-ide.el is not on MELPA).
(use-package claude-code-ide
  :ensure nil ; Provided by Nix
  :defer t
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

;;; @doc Conversational LLM front-end with multiple backends (Anthropic,
;;; @doc Gemini, local Ollama) configured below. Bound to C-c RET / C-c
;;; @doc M-RET for quick send and full menu. Keys come from the
;;; @doc environment first, then auth-source via auth-source-1password.
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

;;; @doc Model Context Protocol bridge — lets gptel call MCP tools so the
;;; @doc LLM can read files, query databases, and act through registered
;;; @doc servers. Loaded after gptel.
(use-package mcp
  :defer t
  :after gptel)

(provide 'init-ai)
;;; init-ai.el ends here
