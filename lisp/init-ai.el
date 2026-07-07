;;; init-ai.el --- AI assistants -*- lexical-binding: t; -*-

;;; Commentary:

;; AI tool hierarchy:
;;
;;   claude-code-ide  C-c C-'       Agentic editing — autonomous multi-file
;;                                  changes via the Claude Code CLI.
;;
;;   eca              C-c C-e       Editor Code Assistant — chat, inline
;;                                  completion, rewrite, and MCP through an
;;                                  external `eca' server (C-c . for the menu
;;                                  inside eca windows).
;;
;;   gptel            C-c RET       Send region/buffer to an LLM.
;;                    C-c M-RET     Full menu (model, backend, system prompt).
;;
;;   mcp              M-x mcp-connect-server + gptel-mcp-connect
;;                                  Model Context Protocol tool use via gptel.
;;                                  `devenv-mcp-setup' (init-devenv, C-c v M)
;;                                  registers the project's `devenv mcp'
;;                                  server here.
;;
;; Auth: API keys come from the environment first (OPENROUTER_API_KEY /
;; ANTHROPIC_API_KEY / GEMINI_API_KEY) and fall back to auth-source —
;; auth-source-1password (configured in init-systems.el) makes that
;; transparent.  The eca server reads the same provider keys from the
;; environment; its OpenRouter provider is defined in config/eca/config.json
;; (opt-in via services.jotain.openrouter.enable in the Home Manager module).

;;; Code:

;;; @doc Agentic multi-file editing through the Claude Code CLI. Bound to
;;; C-c C-' so the menu is one key away whenever a refactor needs more
;;; context than a single LSP rename can carry. Provided by Nix
;;; (manzaltu/claude-code-ide.el is not on MELPA).
(use-package claude-code-ide
  :ensure nil ; Provided by Nix
  :defer t
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

;;; @doc Editor Code Assistant — AI pair-programming client (chat, inline
;;; completion, rewrite, MCP) talking to an external `eca' server over
;;; JSONRPC. The server binary is provided by Nix and found on $PATH, so
;;; nothing is downloaded; provider keys come from the environment, same as
;;; gptel. C-c C-e starts a session and opens the chat.
(use-package eca
  :defer t
  :bind ("C-c C-e" . eca))

;;; @doc Conversational LLM front-end with multiple backends. OpenRouter
;;; — an OpenAI-compatible aggregator fronting Claude, GPT, Gemini,
;;; DeepSeek, Qwen, GLM and more behind one key — is the default; direct
;;; Anthropic, Gemini and local Ollama backends stay selectable from the
;;; C-c M-RET menu. Bound to C-c RET / C-c M-RET for quick send and full
;;; menu. Keys come from the environment first, then auth-source via
;;; auth-source-1password.
(use-package gptel
  :defer t
  :functions (gptel-make-openai gptel-make-anthropic gptel-make-gemini
                                gptel-make-ollama)
  :bind
  (("C-c RET"   . gptel-send)
   ("C-c M-RET" . gptel-menu))
  :config
  ;; OpenRouter — OpenAI-compatible aggregator, primary backend.
  (setopt gptel-backend
          (gptel-make-openai "OpenRouter"
            :host "openrouter.ai"
            :endpoint "/api/v1/chat/completions"
            :stream t
            :key (lambda ()
                   (or (getenv "OPENROUTER_API_KEY")
                       (auth-source-pick-first-password
                        :host "openrouter.ai"
                        :user "apikey")))
            :models '(anthropic/claude-opus-4.8
                      anthropic/claude-sonnet-4.6
                      openai/gpt-5.5
                      google/gemini-3.5-flash
                      deepseek/deepseek-v4-pro
                      qwen/qwen3.5-35b-a3b
                      z-ai/glm-4.7))
          gptel-model 'anthropic/claude-sonnet-4.6)

  ;; Anthropic (Claude) — direct backend, no aggregator.
  (gptel-make-anthropic "Claude"
    :stream t
    :key (lambda ()
           (or (getenv "ANTHROPIC_API_KEY")
               (auth-source-pick-first-password
                :host "api.anthropic.com"
                :user "apikey"))))

  ;; Google Gemini — direct backend.
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
;;; LLM can read files, query databases, and act through registered
;;; servers. Loaded after gptel.
(use-package mcp
  :defer t
  :after gptel)

(provide 'init-ai)
;;; init-ai.el ends here
