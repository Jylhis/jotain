# CLAUDE.md - Emacs Configuration

## System Rules

1. **USE emacs-expert** for ALL Emacs-related tasks - configuration, packages, elisp
2. **MODULAR approach** - Keep configs in appropriate `config/*.el` files
3. **LAZY LOADING** - Use `:defer`, `:hook`, or `:after` for performance
4. **TEST with `just test`** before committing changes
5. **UPDATE default.nix** first when adding new packages

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

- **New language support** ’ `config/programming.el`
- **UI tweaks** ’ `config/ui.el`
- **Completion changes** ’ `config/completion.el`
- **Org-mode setup** ’ `config/writing.el`
- **Custom functions** ’ `lisp/*.el`

---

_Delegate all Emacs tasks to emacs-expert agent_

- When installing new packages, check with nixos MCP or nix search, if the package exists
