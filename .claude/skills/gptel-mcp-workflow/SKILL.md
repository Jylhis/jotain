---
name: gptel-mcp-workflow
description: >
  Configures gptel LLM backends, writes gptel directives, and sets up MCP tool
  definitions for AI-assisted Emacs workflows. Use when configuring gptel for Claude,
  writing system prompts, integrating mcp.el, or building self-modifying Emacs configs.
  Also trigger on mentions of gptel, mcp.el, mcp-hub, or LLM integration in Emacs.
---

# gptel & MCP Workflow

Configure AI-assisted workflows within Emacs using gptel (LLM client) and mcp.el (MCP client).

## gptel Setup

```elisp
(use-package gptel
  :config
  (setopt gptel-model 'claude-sonnet-4-20250514)
  (setopt gptel-backend
    (gptel-make-anthropic "Claude"
      :stream t
      :key #'gptel-api-key)))  ; reads from auth-source
```

### API Key (auth-source)

Store in `~/.authinfo.gpg`:
```
machine api.anthropic.com login apikey password sk-ant-api03-...
```

### Directives

```elisp
(setopt gptel-directives
  '((default . "You are a helpful assistant.")
    (elisp . "You are an Emacs Lisp expert. Use lexical-binding, keymap-set, setopt, named hook functions, jotain- prefix.")
    (nix . "You are a Nix expert for Emacs packaging. Use emacsPackagesFor, trivialBuild. Never suggest :ensure t.")))
```

## mcp.el Setup

```elisp
(use-package mcp
  :config
  (setopt mcp-hub-servers
    '(("nixos" . (:command "uvx" :args ("mcp-nixos")))
      ("elisp-dev" . (:command "emacs" :args ("--batch" "--load" "path/to/server.el"
                                               "--eval" "(emacs-mcp-run-stdio)"))))))
```

## gptel-mcp Bridge

```elisp
(use-package gptel-mcp
  :after (gptel mcp)
  :config
  (gptel-mcp-connect))  ; auto-registers MCP tools with gptel
```

## Custom Tools

```elisp
(gptel-make-tool
  :name "eval-elisp"
  :function (lambda (code)
    (format "%S" (condition-case err
                     (eval (read code))
                   (error (format "Error: %S" err)))))
  :description "Evaluate Emacs Lisp code safely"
  :args (list '(:name "code" :type "string" :description "Elisp expression"))
  :confirm t)  ; ALWAYS require confirmation for eval
```

## Security

Emacs has NO sandboxing for `eval`. Mitigations:
1. **Nix as validation gate** — changes must survive `nix build`
2. **`:confirm t`** on all tools that modify state
3. **Git as rollback** — commit before AI-driven changes
4. **Review before apply** — never auto-apply generated code
