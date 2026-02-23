# Quickstart: Jotain

**Branch**: `001-baseline` | **Date**: 2026-02-22

## Prerequisites

- Nix with flakes enabled
- Home Manager (standalone or as a NixOS module)
- A terminal and basic familiarity with Emacs keybindings

## Installation

### 1. Add Jotain to your flake inputs

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    jotain.url = "github:Jylhis/jotain";
  };
}
```

### 2. Enable the Home Manager module

```nix
{
  home-manager.users.yourname = {
    imports = [ jotain.homeModules.default ];

    programs.jotain = {
      enable = true;
      # enableDaemon = true;      # default: runs as systemd service
      # includeRuntimeDeps = true; # default: installs LSP, fonts, CLI tools
    };
  };
}
```

### 3. Rebuild

```bash
# NixOS
sudo nixos-rebuild switch

# Standalone Home Manager
home-manager switch --flake .
```

### 4. Launch

```bash
# If daemon is enabled (default):
emacsclient -c

# Or launch directly:
emacs
```

## Verification Checklist

After installation, verify the baseline is working:

- [ ] Emacs starts without errors (`*Messages*` buffer is clean)
- [ ] Dashboard (Enlight) appears on startup
- [ ] Open a `.nix` file — tree-sitter highlighting active
- [ ] Type a partial identifier — Corfu completion appears
- [ ] Press `C-c g` in a Git repo — Magit status opens
- [ ] Press `C-c t` — theme toggles between light and dark
- [ ] Press `C-+` / `C--` — font size adjusts
- [ ] Run `M-x consult-ripgrep` — search works with live narrowing

## Key Keybindings

| Binding | Action |
|---------|--------|
| `C-c g` | Magit status |
| `C-c t` | Toggle light/dark theme |
| `C-+` / `C--` | Increase / decrease font size |
| `C-0` | Reset font size |
| `C-c f i` | Show font configuration info |
| `M-s` | Consult search |
| `C-x P` | Projection (project) map |
| `C-h f` / `C-h v` | Helpful function / variable lookup |

## Configuration Options

| Option | Default | Description |
|--------|---------|-------------|
| `programs.jotain.enable` | `false` | Master switch |
| `programs.jotain.enableDaemon` | `true` | Emacs as systemd/launchd service |
| `programs.jotain.includeRuntimeDeps` | `true` | Install LSP servers, fonts, CLI tools |
| `programs.jotain.extraPackages` | `epkgs: []` | Add extra Emacs packages |

## Customisation

Place personal configuration in `~/.config/emacs/custom.el`. This
file is loaded automatically if present and is not managed by Jotain.

Add extra Emacs packages via the `extraPackages` option:

```nix
programs.jotain = {
  enable = true;
  extraPackages = epkgs: [ epkgs.pdf-tools epkgs.elfeed ];
};
```

## Troubleshooting

**Emacs won't start**: Run `emacs --debug-init` to see the error.
Check `*Messages*` and `*Warnings*` buffers.

**LSP not working**: Verify the server is on PATH:
`M-x shell-command RET which nil RET`. If missing, ensure
`includeRuntimeDeps = true`.

**Fonts look wrong**: Run `C-c f i` to see which fonts were
detected. Ensure `includeRuntimeDeps = true` for bundled fonts.

**Theme blending artifacts**: This should not happen with the
advised `load-theme`. If it does, run `M-x jotain-ui--disable-all-themes`
then reload the theme.

## Development

```bash
just emacs-dev          # Run with local sources (isolated)
just test-smoke         # Quick validation (<1s)
just test-fast          # Broader tests (<5s)
just test               # Full test suite
just compile            # Byte-compile check
```
