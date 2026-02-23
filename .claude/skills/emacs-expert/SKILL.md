---
name: emacs-expert
description: Emacs configuration conventions for the jotain modular Emacs 30+ distribution. Auto-loaded when editing elisp files, use-package configurations, or Emacs config modules.
user-invocable: false
---

# Jotain Emacs Configuration Conventions

Apply these conventions when editing any `elisp/*.el` file or creating new Emacs configuration.

## Module Structure

Each `elisp/*.el` file covers a functional domain and is loaded by `init.el` in order:

| File | Purpose |
|------|---------|
| `core.el` | Base settings, keybindings, built-in modes |
| `ui.el` | Theme system (doom-themes, nord), theme toggling, daemon frame setup |
| `fonts.el` | Font selection and rendering |
| `completion.el` | Vertico, Corfu, Consult, Orderless |
| `programming.el` | Eglot (LSP), tree-sitter, DAP, language-specific modes |
| `git.el` | Magit integration |
| `writing.el` | Org-mode, documentation tools |
| `dashboard.el` | Startup screen |
| `platforms.el` | Linux/macOS/Android adaptations |
| `platform.el` | Platform detection helpers |
| `systems.el` | System administration tools |
| `per-project.el` | Project-local configuration via `.dir-locals.el` |
| `help.el` | Enhanced help system |
| `collaboration.el` | Collaborative editing |
| `android.el` | Android-specific features |
| `utils.el` | Shared utility functions |

Place new functionality in the correct existing module. Only create new modules for genuinely new domains.

## use-package Template

```elisp
(use-package package-name
  :ensure              ; Auto-install (must map in nix/lib/dependencies.nix)
  :defer t             ; Lazy load for performance
  :after (deps)        ; Load after dependencies
  :diminish            ; Clean mode line
  :hook (mode . func)  ; Mode-specific activation
  :bind (key . cmd)    ; Keybindings
  :custom (var val)    ; Variable settings
  :config              ; Post-load configuration
  (setq var value))
```

**ALWAYS use lazy loading** via `:defer t`, `:hook`, `:after`, or `:commands`. Never load packages eagerly at startup.

## Built-in vs Third-Party

Prefer built-in packages when they meet requirements. This project uses:
- **eglot** (built-in) over lsp-mode
- **flymake** (built-in) over flycheck
- **project.el** (built-in) over projectile
- **treesit** (built-in) over tree-sitter.el
- **magit** (exception: superior UX over vc-mode)
- **vertico/corfu/consult** (exception: better UX over fido/icomplete)

Use `:ensure nil` for built-in packages.

## Elisp Standards

### Naming
- Global symbols use `jotain-` prefix (e.g., `jotain-theme-light`)
- Use `lisp-case`, never `camelCase` or `snake_case`
- Predicates end with `-p`
- Use `--` for private/internal symbols (e.g., `jotain-ui--disable-all-themes`)

### File Headers
Every `.el` file must have `lexical-binding: t`:
```elisp
;;; module.el --- Brief description -*- lexical-binding: t; -*-
```

### Docstrings
- First line must be a standalone complete sentence
- Arguments in CAPS on first mention
- Describe return value

### Error Handling
- `user-error` for user mistakes (won't trigger debugger)
- `error` for programming errors
- `condition-case` for recoverable errors

## Package Installation

This project uses Nix for all package management. When adding a new package:

1. Add `use-package` form with `:ensure` in the appropriate `elisp/*.el` file
2. `nix/lib/dependencies.nix` auto-extracts packages from `use-package` declarations
3. If the package name doesn't map directly to nixpkgs, add a manual mapping in `nix/lib/dependencies.nix`
4. For runtime tools (LSP servers, CLI tools, fonts, tree-sitter grammars), add to `nix/lib/runtime-deps.nix`
5. Use `:nodep` in a `use-package` form to suppress auto-extraction for a specific package

## Integration Stack

- **Completion**: Everything routes through Vertico/Consult/Corfu/Orderless
- **LSP**: Eglot (built-in), configured in `programming.el`
- **Git**: Magit in `git.el`
- **Projects**: Built-in `project.el`
- **Theme system**: `jotain-theme-light`/`jotain-theme-dark` in `ui.el`, toggle with `C-c t`

## Validation

After changes, run:
- `just test-smoke` - Ultra-fast smoke tests (< 1s)
- `just compile` - Byte-compile all .el files
- `just test-fast` - Fast unit tests (< 5s)
