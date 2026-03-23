# Jotain

*Finnish for "something"* — a modern, modular Emacs 30 configuration for software development. Jotain favors built-in Emacs capabilities configured opinionatedly, with third-party packages admitted only when justified. Deeply integrated with NixOS and Home Manager for reproducible deployments, yet fully functional without Nix.

## Directory Structure

```
init.el              — Entry point: loads modules, configures load-path
early-init.el        — Pre-init: UI optimization, Nix detection, native-comp
lisp/                — Built-in Emacs features extended (jotain-<feature>.el)
  jotain-defaults.el — Sensible defaults and version check
  jotain-platform.el — Platform detection, Nix awareness, package archives
modules/             — Third-party package configurations (empty in baseline)
tests/               — ERT tests (smoke < 1s, fast < 5s, full suite)
nix/                 — Nix build system, Home Manager & NixOS modules
```

## Quick Start

### With NixOS + Home Manager

```nix
# In your flake inputs:
inputs.jotain.url = "github:Jylhis/jotain";

# In your home-manager configuration:
imports = [ inputs.jotain.homeModules.default ];
programs.jotain.enable = true;
```

### Without Nix

```bash
git clone https://github.com/Jylhis/jotain.git ~/.config/emacs
emacs  # packages auto-install on first launch
```

### Try without installing

```bash
nix run github:Jylhis/jotain
```

## Documentation

See [docs/architecture.md](docs/architecture.md) for the full architecture guide covering the module system, Nix integration, and testing infrastructure.

## License

GPL-3.0-or-later
