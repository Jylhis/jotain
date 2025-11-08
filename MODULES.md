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
2. **UI modules**:
3. **Completion stack** (order matters):
4. **Editor**:
5. **Programming languages**:

## Development Mode

Set `JOTAIN_DEV_MODE=1` to enable:
- Module loading messages
- Additional debugging output
- Module reload capability (`M-x jotain-reload-module`)

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

