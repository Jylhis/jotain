# Jotain Emacs Distribution - Module Structure

## Overview
Jotain is a minimal, modular Emacs distribution built for Emacs 30+ with Nix integration.

## Directory Structure

```
jotain/
├── templates/
│   ├── early-init.el          # Early initialization (GC, native-comp, UI)
│   └── init.el                # Minimal loader, sets up load-path
├── elisp/
│   ├── jotain.el              # Main entry point, module system
│   ├── core/
│   │   ├── jotain-lib.el      # Utility functions
│   │   ├── jotain-paths.el    # XDG path management
│   │   ├── jotain-packages.el # use-package configuration
│   │   ├── jotain-gc.el       # GC optimization
│   │   └── jotain-core.el     # Core Emacs settings
│   ├── ui/
│   │   ├── jotain-ui-themes.el    # Theme configuration (modus-themes)
│   │   ├── jotain-ui-fonts.el     # Font setup with fallbacks
│   │   └── jotain-ui-modeline.el  # doom-modeline configuration
│   ├── completion/
│   │   ├── jotain-completion-orderless.el   # Flexible matching
│   │   ├── jotain-completion-vertico.el     # Vertical completion UI
│   │   ├── jotain-completion-marginalia.el  # Rich annotations
│   │   ├── jotain-completion-consult.el     # Enhanced commands
│   │   ├── jotain-completion-corfu.el       # In-buffer completion
│   │   └── jotain-completion-embark.el      # Contextual actions
│   ├── editor/
│   │   └── jotain-editor-core.el  # Core editing features
│   └── programming/
│       ├── jotain-programming-elisp.el  # Elisp development
│       ├── jotain-programming-nix.el    # Nix language support
│       └── jotain-programming-shell.el  # Shell scripting support
```

## Module Loading Order

1. **Core modules** (loaded first):
   - jotain-lib
   - jotain-paths
   - jotain-packages
   - jotain-gc
   - jotain-core

2. **UI modules**:
   - jotain-ui-themes
   - jotain-ui-fonts
   - jotain-ui-modeline

3. **Completion stack** (order matters):
   - jotain-completion-orderless
   - jotain-completion-vertico
   - jotain-completion-marginalia
   - jotain-completion-consult
   - jotain-completion-corfu
   - jotain-completion-embark

4. **Editor**:
   - jotain-editor-core

5. **Programming languages**:
   - jotain-programming-elisp
   - jotain-programming-nix
   - jotain-programming-shell

## Development Mode

Set `JOTAIN_DEV_MODE=1` to enable:
- Module loading messages
- Additional debugging output
- Module reload capability (`M-x jotain-reload-module`)

## Key Features

### Performance Optimizations
- Deferred GC during startup (early-init.el)
- Idle GC collection
- Minibuffer GC pausing
- Native compilation support (Emacs 30+)

### XDG Compliance
- Cache: `$XDG_CACHE_HOME/jotain/`
- Data: `$XDG_DATA_HOME/jotain/`
- State: `$XDG_STATE_HOME/jotain/`
- Config: `$XDG_CONFIG_HOME/jotain/`

### Completion Stack
Full Vertico ecosystem:
- **Orderless**: Flexible, order-independent matching
- **Vertico**: Minimal, fast vertical completion UI
- **Marginalia**: Rich annotations in minibuffer
- **Consult**: Enhanced search and navigation commands
- **Corfu**: Modern in-buffer completion popup
- **Embark**: Context-sensitive actions on candidates

### Programming Support
- **Elisp**: Full IDE with helpful, package-lint, macrostep
- **Nix**: nix-mode with nil LSP server
- **Shell**: sh-mode with bash-language-server and shellcheck

## Package Management

Packages are managed by Nix. All `use-package` declarations have `:ensure nil` (implicit or explicit) since packages are provided by the Nix environment.

## Configuration Philosophy

- **Minimal**: Only essential features included
- **Modular**: Each module is independent and focused
- **Performance**: Lazy loading, optimized GC, fast startup
- **Standards**: Follows Emacs 30+ best practices
- **Extensible**: Easy to add new modules following existing patterns

## Usage

```elisp
;; In your Nix configuration, symlink templates to ~/.config/emacs/
ln -s /path/to/jotain/templates/early-init.el ~/.config/emacs/
ln -s /path/to/jotain/templates/init.el ~/.config/emacs/

;; Or set user-emacs-directory to point to the templates directory
```

## Adding New Modules

1. Create new file in appropriate subdirectory
2. Follow naming convention: `jotain-category-feature.el`
3. Include proper header with `lexical-binding: t`
4. Use `use-package` for external packages
5. Add `(provide 'jotain-category-feature)` at end
6. Load in `jotain.el` via `jotain-load-module`

## Required Nix Packages

Based on use-package declarations, these packages need to be in default.nix:

### Core & UI
- doom-modeline
- nerd-icons
- diminish

### Completion
- vertico
- consult
- corfu
- embark
- embark-consult
- marginalia
- orderless
- cape

### Editor
- undo-tree
- multiple-cursors
- expand-region
- move-text
- anzu
- highlight-symbol
- rainbow-delimiters
- smartparens
- which-key

### Programming
- helpful
- package-lint
- macrostep
- eros
- auto-compile
- nix-mode
- nix-repl
- fish-mode
- shfmt
- flymake-shellcheck

## License

Copyright (C) 2025 Jotain Contributors
