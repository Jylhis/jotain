# CLAUDE.md - Emacs Configuration

## System Rules

1. **USE emacs-expert** for ALL Emacs-related tasks - configuration, packages, elisp
2. **MODULAR approach** - Keep configs in appropriate `config/*.el` files
3. **LAZY LOADING** - Use `:defer`, `:hook`, or `:after` for performance
4. **TEST with `just test`** before committing changes
5. **UPDATE default.nix** first when adding new packages
6. **PREFER built-in tools over Bash** - Use Read, Write, Edit, Grep, Glob, LS instead of shell commands

---

## Agent Behavior Patterns

**DEFAULT: Implementation-first** - emacs-expert MUST use tools to implement changes by default

**EXCEPTION: Explicit advice requests** - Only provide advice without implementation when user explicitly asks:

- "How should I configure..." or "What's the best approach..." → Advice acceptable
- "Advise me on..." or "What would you do..." → Advice acceptable

**IMPLEMENTATION REQUIRED for:**

- "Fix this config..." → MUST implement using tools
- "Add package..." → MUST implement using tools
- "Update my setup..." → MUST implement using tools
- Any direct Emacs task → MUST implement using tools

## Agent Work Verification

**ALWAYS verify delegated work** - When agents report completion:

- Check that tasks match original instructions
- Verify outputs/changes are correct (use `git status` to confirm file modifications)
- Test functionality if applicable
- Request fixes if work is incomplete
- **CRITICAL**: If agent only provided instructions without using tools, re-delegate with explicit requirement to USE tools

---

## Configuration Structure

**Base**: `sources/emacs/` - Modern Emacs 30.1 configuration

```
default.nix         # Package definitions (Nix)
config/
  core.el          # Base settings, files, backup
  ui.el            # Theme, modeline, visual
  completion.el    # Vertico/Consult/Corfu stack
  programming.el   # LSP, treesitter, languages
  writing.el       # Org-mode, markdown, prose
  git.el           # Magit and version control
  help.el          # Documentation, guides
  ai.el            # AI assistants, copilot
lisp/
  *.el            # Custom utility functions
```

---

## Stack & Patterns

**Core Stack**: Vertico + Consult + Corfu + Embark + Eglot + Magit

### use-package Template

```elisp
(use-package package-name
  :ensure              ; Auto-install (if in default.nix)
  :defer t             ; Lazy load for performance
  :after (deps)        ; Load dependencies first
  :hook (mode . func)  ; Mode-specific activation
  :bind (key . cmd)    ; Keybindings
  :custom (var val)    ; Variable settings
  :diminish            ; Clean mode line
  :config              ; Post-load configuration
  (setq var value))
```

---

## Performance Guidelines

- **Lazy load everything** - Use `:defer t`, `:hook`, or `:commands`
- **Minimize startup** - Defer non-essential packages
- **Clean mode line** - Use `:diminish` for minor modes
- **Prefer built-ins** - Use Emacs internals when suitable
- **GC optimization** - Configured in early-init.el

---

## Adding Packages

1. **Update default.nix** - Add package to Nix configuration
2. **Choose module** - Add use-package to appropriate `config/*.el`
3. **Test locally** - Run `just test` to verify
4. **Performance check** - Ensure proper lazy loading

---

## Testing & Development

```bash
just test           # Run ERT test suite
just format         # Format all elisp files
just check          # Full validation
nix develop         # Enter development shell
```

---

## Common Tasks

- **New language support** � `config/programming.el`
- **UI tweaks** � `config/ui.el`
- **Completion changes** � `config/completion.el`
- **Org-mode setup** � `config/writing.el`
- **Custom functions** � `lisp/*.el`

---

# Tool Usage Guidelines

**ALWAYS use Claude Code's built-in tools instead of system commands.** Built-in tools provide better reliability, security, and IDE integration.

## Command Replacements

| System Command           | Built-in Tool                 | Usage                                |
| ------------------------ | ----------------------------- | ------------------------------------ |
| `ls -la`                 | **LS**                        | List directory contents              |
| `cat <file>`             | **Read**                      | View file contents                   |
| `grep <pattern> <files>` | **Grep**                      | Search patterns in files             |
| `find . -name "*.py"`    | **Glob**                      | Find files with `**/*.py` pattern    |
| `sed -i 's/old/new/g'`   | **Edit/MultiEdit**            | Edit files (single/multiple changes) |
| `curl <url>`             | **WebFetch**                  | Retrieve web content                 |
| `touch/mkdir`            | **Write**                     | Create files/directories             |
| `jupyter notebook`       | **NotebookRead/NotebookEdit** | Read/modify Jupyter notebooks        |
| Google search in browser | **WebSearch**                 | Perform web searches                 |

**Preferred built-in tools:**

- `Read/Write/Edit/MultiEdit` - File operations
- `Grep/Glob/LS` - Search and navigation
- `NotebookRead/NotebookEdit` - Jupyter notebook operations
- `WebFetch/WebSearch` - Web content and searches
- `TodoWrite/Task` - Task management

**Use Bash only for:**

- System commands (nix, just, deploy)
- Git operations
- Build/test commands

## Reference: https://docs.anthropic.com/en/docs/claude-code/settings#tools-available-to-claude

---

_Delegate all Emacs tasks to emacs-expert agent_

- When installing new packages, check with nixos MCP or nix search, if the package exists

# important-instruction-reminders

Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (\*.md) or README files. Only create documentation files if explicitly requested by the User.
