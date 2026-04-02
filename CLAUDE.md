# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**Jotain** is a custom GNU Emacs configuration built from scratch. The name is Finnish for "something." The project includes both the Emacs Lisp configuration and a Nix expression for building Emacs from source with fine-grained control over build options.

## Key Commands

```bash
# Build Emacs (Linux)
just linux                # nix-build --argstr system x86_64-linux default.nix

# Run Emacs with this config
just run                  # launches emacs with --debug-init using this directory

# Clean build artifacts
just clean                # removes eln-cache/ and result/

# Development environment
devenv shell              # enter the devenv shell (defined in devenv.nix)
```

### Building Emacs variants via Nix

```bash
nix-build default.nix                                  # Emacs 30.2 + all tree-sitter grammars
nix-build default.nix --arg withTreeSitterGrammars false  # without grammars
nix-build default.nix --arg noGui true                 # terminal-only + grammars
nix-build default.nix --arg withPgtk true              # pure GTK (Wayland) + grammars
nix-build default.nix --arg variant '"git"'             # bleeding-edge master + grammars
nix-build default.nix --arg variant '"igc"'             # incremental GC branch + grammars
nix-build default.nix --arg variant '"macport"'         # macOS macport fork + grammars
nix-build emacs.nix                                    # bare Emacs build (no grammars/packages)
```

For git/unstable/igc variants, the first build will fail and report the correct hash. Re-run with `--argstr hash "sha256-..."`.

## Architecture

- **`early-init.el`** — Loaded before package initialization and GUI setup (Emacs 30+). Currently sets `load-prefer-newer`.
- **`init.el`** — Main Emacs configuration entry point. Currently minimal/scaffold.
- **`default.nix`** — Distribution layer. Imports `emacs.nix` and wraps it with `emacsPackagesFor`/`withPackages` to include all tree-sitter grammars (275 grammars from nixpkgs). The wrapper auto-generates a `site-start.el` that sets `treesit-extra-load-path`.
- **`emacs.nix`** — Comprehensive Nix expression to build GNU Emacs from source. Supports multiple source variants (mainline, git, unstable, macport, igc), GUI toolkits (GTK3, pgtk, NS, X11, no-GUI), native compilation, tree-sitter, and macOS-specific patches. Wraps `nixpkgs` `emacs30` with `override`/`overrideAttrs`.
- **`devenv.nix` / `devenv.yaml`** — Development environment configuration using [devenv](https://devenv.sh).
- **`Justfile`** — Task runner with build, run, and clean commands.
- **`journal/`** — Daily development journal entries (markdown).
- **`docs/`** — Reference documentation and inspiration links.

## Conventions

- Emacs Lisp files use `lexical-binding: t`.
- The `next` branch is used for active development; `main` is the stable branch.
- **Journal** — Update `journal/YYYY-MM-DD.md` when making configuration changes to keep a running log of what was done each day.
