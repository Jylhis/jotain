# LLM.md

Instructions for AI assistants helping users configure and use Jotain Emacs.

## What is Jotain?

Jotain is a modular Emacs 30+ configuration distributed as a Nix flake with a home-manager module. It provides a complete development environment with LSP support, modern completion (Vertico/Corfu), Git integration (Magit), and sensible defaults.

## Installation

Add to the user's flake.nix:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    jotain.url = "github:Jylhis/jotain";
  };

  outputs = { nixpkgs, home-manager, jotain, ... }: {
    homeConfigurations."username" = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [
        jotain.homeModules.default
        {
          programs.jotain.enable = true;
        }
      ];
    };
  };
}
```

## Module Options

### `programs.jotain.enable`
**Type:** boolean
**Default:** `false`

Enable Jotain Emacs configuration.

### `programs.jotain.enableDaemon`
**Type:** boolean
**Default:** `true`

Run Emacs as a systemd/launchd service. When enabled:
- Sets `EDITOR="emacsclient -t -a ''"` and `VISUAL="emacsclient -c -a ''"`
- Creates systemd user service with socket activation (Linux)
- Creates launchd service (macOS)

When disabled:
- Sets `EDITOR="emacs -nw"` and `VISUAL="emacs"`
- No background service

### `programs.jotain.includeRuntimeDeps`
**Type:** boolean
**Default:** `true`

Install runtime dependencies (LSP servers, CLI tools, fonts). Set to `false` to manage these separately.

### `programs.jotain.extraPackages`
**Type:** function
**Default:** `epkgs: []`

Add extra Emacs packages:

```nix
programs.jotain.extraPackages = epkgs: [
  epkgs.pdf-tools
  epkgs.org-roam
  epkgs.evil
];
```

## Common Configuration Examples

### Minimal Installation (no runtime deps)

```nix
programs.jotain = {
  enable = true;
  includeRuntimeDeps = false;  # User manages LSP servers, fonts, etc.
};
```

### Without Daemon (traditional Emacs)

```nix
programs.jotain = {
  enable = true;
  enableDaemon = false;  # No background service
};
```

### With Extra Packages

```nix
programs.jotain = {
  enable = true;
  extraPackages = epkgs: [
    epkgs.evil
    epkgs.evil-collection
    epkgs.org-roam
  ];
};
```

### Custom Runtime Dependencies

```nix
programs.jotain = {
  enable = true;
  includeRuntimeDeps = false;
};

# Install only what's needed
home.packages = with pkgs; [
  ripgrep
  gopls  # Go only
];
```

## Included Runtime Dependencies

When `includeRuntimeDeps = true`, these are installed:

**LSP Servers:**
- `nil` (Nix)
- `gopls` (Go)
- `typescript-language-server`
- `yaml-language-server`
- `dockerfile-language-server`
- `marksman` (Markdown)
- `clang-tools` (C/C++ via clangd)

**CLI Tools:**
- `ripgrep` (fast grep, used by consult-ripgrep)
- `fd` (fast find, used by consult-fd)
- `direnv` (directory environments)
- `zoxide` (frecency-based navigation)
- `multimarkdown` (Markdown rendering)

**Fonts:**
- JetBrains Mono, Fira Code, Iosevka, Cascadia Code, Hack
- Inter (UI font)
- Source Serif Pro, Liberation fonts
- Noto Color Emoji

**Tree-sitter Grammars:**
bash, c, cpp, go, javascript, json, lua, markdown, nix, python, rust, typescript, yaml, tsx, css, html

## Important: Git is NOT Included

Git must be configured separately to avoid conflicts:

```nix
programs.git = {
  enable = true;
  userName = "Your Name";
  userEmail = "you@example.com";
};
```

Or for git with SVN support:
```nix
programs.git = {
  enable = true;
  package = pkgs.gitSVN;
};
```

## Key Bindings

| Key | Action |
|-----|--------|
| `C-x g` | Magit status |
| `C-x p f` | Project find file |
| `C-x b` | Switch buffer (consult) |
| `C-s` | Search in buffer |
| `M-.` | Go to definition |
| `M-,` | Go back |
| `C-c t` | Toggle light/dark theme |
| `C-x j` | Toggle window split direction |
| `C-c u` / `C-c r` | Winner undo/redo |

## Managing the Daemon

```bash
# Check status
systemctl --user status emacs.service

# Restart after config changes
systemctl --user restart emacs.service

# View logs
journalctl --user -u emacs.service

# Connect to running daemon
emacsclient -c           # New GUI frame
emacsclient -t           # Terminal
emacsclient -n file.txt  # Open file, don't wait
```

## Troubleshooting

### Emacs not finding LSP server
Check if `includeRuntimeDeps = true` or install the server manually. Verify with:
```
M-x executable-find RET gopls RET
```

### Fonts not displaying correctly
Ensure `fonts.fontconfig.enable = true` in home-manager config (automatically set when `includeRuntimeDeps = true`).

### Daemon not starting
```bash
systemctl --user status emacs.service
journalctl --user -u emacs.service -n 50
```

### Tree-sitter mode not activating
Check grammar is available:
```
M-x treesit-language-available-p RET python RET
```

### Configuration not applying
Daemon caches config on startup. Restart it:
```bash
systemctl --user restart emacs.service
```

## User Customization

User can add custom elisp to `~/.config/emacs/custom.el` (auto-loaded if exists). This file persists across rebuilds.

For extensive customization, users can:
1. Fork the repository
2. Modify elisp files in `elisp/` directory
3. Point their flake to the fork

## Try Without Installing

```bash
nix run github:Jylhis/jotain
```

Creates a temporary isolated environment without affecting existing Emacs config.
