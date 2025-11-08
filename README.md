# Jotain

A NixOS-native Emacs distribution with automatic dependency management.

## Features

- **Nix-native**: All dependencies managed through Nix
- **Automatic dependency extraction**: Use-package declarations are automatically parsed
- **Isolated development**: XDG-compliant with isolated development environment
- **Emacs 30+ optimized**: Native compilation and modern performance optimizations
- **Full completion stack**: Vertico, Consult, Corfu, Embark, Marginalia, Orderless
- **Language support**: Elisp, Nix, Shell scripting with LSP

## Quick Start

### Development

```bash
# Clone repository
git clone https://github.com/yourusername/jotain.git
cd jotain

# Enter development shell (automatic with direnv)
nix develop

# Launch development Emacs
emacs-dev

# Or use the CLI
jot doctor
jot run
```

### Installation

#### Home Manager

```nix
{
  inputs.jotain.url = "github:yourusername/jotain";

  # In home-manager configuration:
  programs.jotain = {
    enable = true;
  };
}
```

#### NixOS

```nix
{
  inputs.jotain.url = "github:yourusername/jotain";

  # In NixOS configuration:
  programs.jotain = {
    enable = true;
  };
}
```

## Development Workflow

```bash
# Run tests
just test

# Build package
just build

# Check flake
just check

# Format code (Nix, Shell, Elisp)
just fmt

# Check formatting
just fmt-check

# Run doctor check
just doctor
```

## Project Structure

```
jotain/
├── flake.nix           # Nix flake definition
├── Justfile            # Task automation
│
├── nix/                # Nix infrastructure
│   ├── lib/            # Library functions (dependency extraction)
│   ├── dev/            # Development shell
│   ├── packages/       # Package definitions
│   ├── modules/        # Home-manager & NixOS modules
│   ├── overlays/       # Package overlays
│   └── checks/         # CI checks
│
├── elisp/              # Emacs Lisp modules
│   ├── jotain.el       # Main entry point
│   ├── core/           # Core functionality
│   ├── ui/             # UI configuration
│   ├── completion/     # Completion stack
│   ├── editor/         # Editor features
│   └── programming/    # Language support
│
├── cli/                # Command-line interface
│   ├── jot             # Main CLI script
│   ├── lib/            # Shared libraries
│   └── commands/       # CLI commands
│
├── templates/          # User templates
│   ├── early-init.el   # Early initialization
│   └── init.el         # User init template
│
└── tests/              # Test suite
    ├── test-helper.el  # Test utilities
    ├── test-smoke.el   # Smoke tests
    └── test-runner.el  # Test runner
```

## Testing

```bash
# Run smoke tests
just test

# Run in development mode
just test-dev

# Run specific test
emacs --batch -L elisp -L tests -l test-smoke.el -f ert-run-tests-batch-and-exit
```
