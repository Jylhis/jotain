;;; init-ai.el --- AI assistants -*- lexical-binding: t; -*-

;;; Commentary:

;; AI tool hierarchy:
;;
;;   claude-code-ide  C-c q         Agentic editing — autonomous multi-file
;;                                  changes via the Claude Code CLI.
;;
;;   jotain-screenshot              Capture the frame to var/screenshots/;
;;                                  also the `emacs_screenshot' MCP tool,
;;                                  so Claude can see Emacs.
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
;; Auth: API keys (OPENROUTER_API_KEY / ANTHROPIC_API_KEY / GEMINI_API_KEY)
;; come from the environment, falling back to auth-source (1Password via
;; init-systems.el, or authinfo files from `services.jotain.authSources').
;; The eca server reads keys only from its environment, so
;; `jotain-ai-export-api-keys' exports missing ones from auth-source before
;; a session starts.  Its OpenRouter provider is config/eca/config.json
;; (opt-in via services.jotain.eca.openrouter.enable in Home Manager).

;;; Code:

;;;; Frame screenshots for AI tooling
;;
;; `x-export-frames' needs a cairo build (Linux X11/pgtk), not noGui or NS.

(declare-function x-export-frames "xfns.c" (&optional frames type))
(declare-function jotain-var-file "init-core" (name))
(declare-function auth-source-pick-first-password "auth-source" (&rest spec))

(defun jotain-screenshot (&optional file format)
  "Capture the selected frame to FILE; return the absolute path.
FORMAT is one of the symbols `png' (default), `svg' or `pdf'.
FILE defaults to var/screenshots/<timestamp>.<format> under
`jotain-var-dir'.  Signals `user-error' on tty frames and on
builds without `x-export-frames' (noGui, macOS NS).
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

;;; @doc Agentic multi-file editing through the Claude Code CLI; C-c q
;;; opens its menu. Attached sessions also get an `emacs_screenshot` MCP
;;; tool. Provided by Nix (manzaltu/claude-code-ide.el is not on MELPA).
(use-package claude-code-ide
  :ensure nil
  :defer t
  :bind ("C-c q" . claude-code-ide-menu)
  :functions (claude-code-ide-emacs-tools-setup claude-code-ide-make-tool)
  :custom
  ;; Serve custom MCP tools (emacs_screenshot) to attached sessions.
  (claude-code-ide-enable-mcp-server t)
  :config
  (claude-code-ide-emacs-tools-setup)
  ;; Guarded so an upstream API rename degrades to a no-op.
  (when (fboundp 'claude-code-ide-make-tool)
    (claude-code-ide-make-tool
     :name "emacs_screenshot"
     :description "Capture a screenshot of the current Emacs GUI frame and return the absolute path of the written image file. View it by calling Read on the returned path. Fails in tty sessions and non-cairo builds."
     :args '((:name "format" :type string :enum ["png" "svg" "pdf"]
              :optional t :description "Image format; default png"))
     :function (lambda (&optional format)
                 (jotain-screenshot nil (and format (intern format)))))))

(defvar jotain-ai-provider-auth-keys
  '(("OPENROUTER_API_KEY" "openrouter.ai" "apikey")
    ("ANTHROPIC_API_KEY" "api.anthropic.com" "apikey")
    ("GEMINI_API_KEY" "generativelanguage.googleapis.com" "apikey"))
  "Provider API-key env vars and their auth-source lookup, as (VAR HOST USER).
Read by `jotain-ai-export-api-keys'.")

(defun jotain-ai-export-api-keys ()
  "Export any missing provider API key from auth-source into the environment.
For each unset variable in `jotain-ai-provider-auth-keys', look the
secret up by host and user and `setenv' it, so subprocesses such as the
eca server inherit it.  Set variables are left alone; missing secrets
are skipped."
  (require 'auth-source)
  (dolist (entry jotain-ai-provider-auth-keys)
    (pcase-let ((`(,var ,host ,user) entry))
      (unless (getenv var)
        (when-let* ((secret (auth-source-pick-first-password
                             :host host :user user)))
          (setenv var secret))))))

;;; @doc Editor Code Assistant: AI pair-programming client (chat, inline
;;; completion, rewrite, MCP) talking to an external `eca` server over
;;; JSONRPC. The server binary is on the wrapper PATH, so nothing is
;;; downloaded. Missing provider keys are exported from auth-source before
;;; a session starts. C-c e starts a session and opens the chat.
(use-package eca
  :defer t
  :bind ("C-c e" . eca)
  :init
  (advice-add 'eca :before #'jotain-ai-export-api-keys))

;;; @doc Conversational LLM front-end. The default backend is OpenRouter,
;;; an OpenAI-compatible aggregator (Claude, GPT, Gemini, DeepSeek, Qwen,
;;; GLM, ...) behind one key; direct Anthropic, Gemini and local Ollama
;;; backends are selectable from the C-c S menu. C-c s sends. Keys come
;;; from the environment, then auth-source.
(use-package gptel
  :defer t
  :functions (gptel-make-openai gptel-make-anthropic gptel-make-gemini
                                gptel-make-ollama)
  :bind
  (("C-c s" . gptel-send)
   ("C-c S" . gptel-menu))
  :config
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
            ;; Keep in sync with config/eca/config.json
            ;; (providers.openrouter.models); checked by eca-models-in-sync.
            :models '(anthropic/claude-opus-4.8
                      anthropic/claude-sonnet-4.6
                      openai/gpt-5.5
                      google/gemini-3.5-flash
                      deepseek/deepseek-v4-pro
                      qwen/qwen3.5-35b-a3b
                      z-ai/glm-4.7))
          gptel-model 'anthropic/claude-sonnet-4.6)

  (gptel-make-anthropic "Claude"
    :stream t
    :key (lambda ()
           (or (getenv "ANTHROPIC_API_KEY")
               (auth-source-pick-first-password
                :host "api.anthropic.com"
                :user "apikey"))))

  (gptel-make-gemini "Gemini"
    :stream t
    :key (lambda ()
           (or (getenv "GEMINI_API_KEY")
               (auth-source-pick-first-password
                :host "generativelanguage.googleapis.com"
                :user "apikey"))))

  ;; Local models, no key needed.
  (gptel-make-ollama "Ollama"
    :stream t
    :host "localhost:11434"
    :models '(llama3.1:latest)))

;;; @doc Model Context Protocol client: lets gptel call tools on
;;; registered MCP servers. Loaded on demand (M-x mcp-connect-server,
;;; `devenv-mcp-setup`).
(use-package mcp
  :defer t)

(provide 'init-ai)
;;; init-ai.el ends here
