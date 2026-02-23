# Contract: Home Manager Module

**Branch**: `001-baseline` | **Date**: 2026-02-22
**Source**: `nix/modules/home/default.nix`

## Overview

The Home Manager module is Jotain's primary external interface. Users
interact with Jotain exclusively through these Nix options and the
resulting Emacs configuration it produces.

## Option Schema

### `programs.jotain.enable`

- **Type**: `bool`
- **Default**: `false`
- **Effect when true**:
  - Deploys `init.el` and `early-init.el` to `~/.config/emacs/`
  - Installs the configured Emacs package via `programs.emacs`
  - Sets EDITOR and VISUAL environment variables
- **Effect when false**:
  - No configuration deployed
  - No Emacs package installed
  - No environment variables set

### `programs.jotain.enableDaemon`

- **Type**: `bool`
- **Default**: `true`
- **Requires**: `enable = true`
- **Effect when true**:
  - Creates systemd user service (Linux) or launchd agent (macOS)
  - Creates systemd socket for emacsclient
  - Creates emacsclient desktop entry
  - Sets EDITOR/VISUAL to `emacsclient`
- **Effect when false**:
  - No service files created
  - Sets EDITOR/VISUAL to direct `emacs`

### `programs.jotain.includeRuntimeDeps`

- **Type**: `bool`
- **Default**: `true`
- **Requires**: `enable = true`
- **Effect when true**:
  - Installs LSP servers to `home.packages`:
    nil, gopls, bash-language-server, typescript-language-server,
    clangd, marksman, yaml-language-server, dockerfile-language-server
  - Installs CLI tools: ripgrep, fd, direnv
  - Installs fonts: JetBrainsMono, FiraCode, Iosevka, CascadiaCode,
    Hack, Inter, Source Serif Pro, Liberation Serif, Noto Color Emoji
  - Enables fontconfig
- **Effect when false**:
  - No LSP servers, CLI tools, or fonts installed
  - Emacs still deploys and functions (graceful degradation)

### `programs.jotain.extraPackages`

- **Type**: `function (epkgs -> list[package])`
- **Default**: `epkgs: []`
- **Requires**: `enable = true`
- **Effect**: Additional Emacs packages merged into the Emacs
  derivation alongside Jotain's packages

## Output Artifacts

When fully enabled (all defaults), the module produces:

| Artifact | Path | Condition |
|----------|------|-----------|
| init.el | `~/.config/emacs/init.el` | `enable = true` |
| early-init.el | `~/.config/emacs/early-init.el` | `enable = true` |
| Emacs binary | `home-path/bin/emacs` | `enable = true` |
| emacsclient | `home-path/bin/emacsclient` | `enable = true` |
| systemd service | `~/.config/systemd/user/emacs.service` | `enableDaemon = true` |
| systemd socket | `~/.config/systemd/user/emacs.socket` | `enableDaemon = true` |
| desktop entry | emacsclient.desktop | `enableDaemon = true` |
| LSP servers | `home-path/bin/{nil,gopls,...}` | `includeRuntimeDeps = true` |
| CLI tools | `home-path/bin/{rg,fd,direnv}` | `includeRuntimeDeps = true` |
| Fonts | `home-path/share/fonts/` | `includeRuntimeDeps = true` |
| EDITOR | `emacsclient` or `emacs` | `enable = true` |
| VISUAL | `emacsclient` or `emacs` | `enable = true` |

## Compatibility Guarantees

- Option names and types MUST NOT change without a constitution
  amendment and MAJOR version bump.
- New options MAY be added (MINOR version bump) but MUST default
  to preserving existing behavior.
- Removing an option requires deprecation notice and migration path.

## NMT Test Coverage

Every option combination has a corresponding NMT test:

| Test | Options |
|------|---------|
| `test-module-enabled` | All defaults (enable=true, daemon=true, deps=true) |
| `test-module-disabled` | enable=false |
| `test-runtime-deps-enabled` | enable=true, includeRuntimeDeps=true |
| `test-runtime-deps-disabled` | enable=true, includeRuntimeDeps=false |
| `test-daemon-disabled` | enable=true, enableDaemon=false |
