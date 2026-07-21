;;; init-ai.el --- AI assistants -*- lexical-binding: t; -*-

;;; Commentary:

;; AI tool hierarchy:
;;
;;   claude-code-ide  C-c q         Agentic editing — autonomous multi-file
;;                                  changes via the Claude Code CLI.
;;
;;   jotain-screenshot              Capture the frame to var/screenshots/,
;;                                  also served to Claude as the
;;                                  `emacs_screenshot' MCP tool so the AI
;;                                  can see how Emacs looks.
;;
;;   eca              C-c e         Editor Code Assistant — chat, inline
;;                                  completion, rewrite, and MCP through an
;;                                  external `eca' server (C-c . for the menu
;;                                  inside eca windows).
;;
;;   gptel            C-c s         Send region/buffer to an LLM.
;;                    C-c S         Full menu (model, backend, system prompt).
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

;;;; Frame screenshots for AI tooling
;;
;; `x-export-frames' is a cairo-only primitive (Linux X11/pgtk builds;
;; absent on noGui/tty and macOS NS/macport builds).

(declare-function x-export-frames "xfns.c" (&optional frames type))
(declare-function jotain-var-file "init-core" (name))

(defun jotain-screenshot (&optional file format)
  "Capture the selected frame to FILE; return the absolute path.
FORMAT is one of the symbols `png' (default), `svg' or `pdf'.
FILE defaults to var/screenshots/<timestamp>.<format> under
`jotain-var-dir'.  Signals `user-error' on tty frames and on
builds without `x-export-frames' (noGui, macOS NS/macport).
Interactively, echo the path and push it onto the kill ring."
  (interactive)
  (unless (display-graphic-p)
    (user-error "jotain-screenshot needs a graphical frame"))
  (unless (fboundp 'x-export-frames)
    (user-error "x-export-frames unavailable — needs a cairo build (Linux X11/pgtk)"))
  (let* ((format (or format 'png))
         (file (expand-file-name
                (or file
                    (jotain-var-file
                     (format "screenshots/%s.%s"
                             (format-time-string "%Y%m%dT%H%M%S") format))))))
    (make-directory (file-name-directory file) t)
    (redisplay t)
    (let ((coding-system-for-write 'binary))
      (write-region (x-export-frames nil format) nil file nil 'silent))
    (when (called-interactively-p 'interactive)
      (kill-new file)
      (message "Screenshot: %s" file))
    file))

;;; @doc Agentic multi-file editing through the Claude Code CLI. Bound to
;;; C-c q (user-reserved space, so no major mode shadows it) so the menu
;;; is one key away whenever a refactor needs more context than a single
;;; LSP rename can carry. Provided by Nix (manzaltu/claude-code-ide.el is
;;; not on MELPA).
(use-package claude-code-ide
  :ensure nil ; Provided by Nix
  :defer t
  :bind ("C-c q" . claude-code-ide-menu)
  :functions (claude-code-ide-emacs-tools-setup claude-code-ide-make-tool)
  :custom
  ;; Serve custom MCP tools (emacs_screenshot below) to attached sessions.
  (claude-code-ide-enable-mcp-server t)
  :config
  (claude-code-ide-emacs-tools-setup)
  ;; Let Claude see the frame: capture an image, return the path, Read it.
  ;; fboundp-guarded so an upstream API rename degrades to a no-op instead
  ;; of breaking startup.
  (when (fboundp 'claude-code-ide-make-tool)
    (claude-code-ide-make-tool
     :name "emacs_screenshot"
     :description "Capture a screenshot of the current Emacs GUI frame and return the absolute path of the written image file. View it by calling Read on the returned path. Fails in tty sessions and non-cairo builds."
     :args '((:name "format" :type string :enum ["png" "svg" "pdf"]
              :optional t :description "Image format; default png"))
     :function (lambda (&optional format)
                 (jotain-screenshot nil (and format (intern format)))))))

;;; @doc Editor Code Assistant — AI pair-programming client (chat, inline
;;; completion, rewrite, MCP) talking to an external `eca' server over
;;; JSONRPC. The server binary is provided by Nix and found on $PATH, so
;;; nothing is downloaded; provider keys come from the environment, same as
;;; gptel. C-c e starts a session and opens the chat.
(use-package eca
  :defer t
  :bind ("C-c e" . eca))

;;; @doc Conversational LLM front-end with multiple backends. OpenRouter
;;; — an OpenAI-compatible aggregator fronting Claude, GPT, Gemini,
;;; DeepSeek, Qwen, GLM and more behind one key — is the default; direct
;;; Anthropic, Gemini and local Ollama backends stay selectable from the
;;; C-c S menu. Bound to C-c s / C-c S for quick send and full menu —
;;; user-reserved space, so org-mode and friends can't shadow them. Keys
;;; come from the environment first, then auth-source via
;;; auth-source-1password.
(use-package gptel
  :defer t
  :functions (gptel-make-openai gptel-make-anthropic gptel-make-gemini
                                gptel-make-ollama)
  :bind
  (("C-c s" . gptel-send)
   ("C-c S" . gptel-menu))
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
            ;; Keep this model list in sync with config/eca/config.json
            ;; (providers.openrouter.models) — the eca server's copy.
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
;;; servers. Loaded on demand via its autoloads (`devenv-mcp-setup',
;;; M-x mcp-connect-server) — nothing here forces a load.
(use-package mcp
  :defer t)

(provide 'init-ai)
;;; init-ai.el ends here
