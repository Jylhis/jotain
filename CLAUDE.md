# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Jotain is a GNU Emacs 30+ configuration with a Nix build layer. The repo ships **both** a modular Elisp configuration (`early-init.el`, `init.el`, `lisp/init-*.el`) and the Nix expressions that build Emacs itself (`emacs.nix`, `default.nix`). The two are coupled: `devenv.nix` builds the dev-shell `emacs` from `emacs.nix` so the editor in the shell is exactly what `just build` produces.

## Development environment

All recipes assume the devenv shell is active. `direnv` handles this via `.envrc`; without direnv, prefix any command with `devenv shell --`:

```
devenv shell -- just check
```

CI (`.github/workflows/devenv.yml`) runs `devenv test` on Ubuntu and macOS, which executes the seven `enterTest` assertions in `devenv.nix` (they lock down that `emacs`/`emacsclient`/`etags`/`eask` all come from the jotainEmacs derivation and that Emacs doesn't leak paths outside the Nix store or the project).

## Common commands

All day-to-day work goes through the `Justfile`:

- `just check` — parse every `.el` file (no compile, no package install). Fast sanity check.
- `just compile` — byte-compile the full config with `byte-compile-error-on-warn`. Requires packages to already be installed (run `just run` once first to trigger bootstrap).
- `just test` — run ERT tests under `test/` if the directory exists (no tests yet).
- `just run [ARGS]` — launch Emacs with `--init-directory` pointed at this repo (isolated from `~/.emacs.d`).
- `just debug` — same as `run` but with `--debug-init` and `debug-on-error`.
- `just tty` — `emacs -nw` (exercises kkp + clipetty paths).
- `just fmt` — `treefmt` (Nix formatting via `nixfmt-rfc-style`).
- `just update-pins` / `just update-pin NAME` — refresh `npins/`.
- `just clean` — remove `*.elc`, autosaves, `eln-cache`, `result` symlink.
- `just clean-all` — additionally wipe `elpa/`, `var/`, `etc/`, forcing a full re-fetch. After this, the next `just run` goes through the `jotain--bootstrapping-p` branch in `init.el`.

Build recipes (all via `nix-build`, targeting current system by default; override with `just system=x86_64-linux …`):

- `just build` — full distribution (Emacs + all ~275 tree-sitter grammars) via `default.nix`.
- `just build-bare` — bare Emacs via `emacs.nix`, no grammars.
- `just build-pgtk` / `build-gtk3` / `build-nox` / `build-macport` / `build-git` / `build-igc` / `build-android` — variant builds. Git/unstable/igc/macport variants will fail on first run and report the hash to pass back via `--argstr hash`.
- `just run-built [ARGS]` — auto-detect platform, build, then launch from `./result/bin/emacs`.

There's also a `devenv-emacs-smoke` script (defined in `devenv.nix`) that byte-compiles everything with warnings-as-errors, and `emacs-run` which is the non-Justfile equivalent of `just run`.

## Architecture

### Elisp layer — three parts

1. **`early-init.el`** runs before `package.el`, before the first frame, before `init.el`. It sets the startup GC threshold to `most-positive-fixnum`, disables bidi reordering, sets `use-package-always-ensure = t` and `package-quickstart = t`, disables the menu/tool/scroll bars *before* the first frame draws, redirects `native-comp` eln-cache to `var/eln-cache/`, and aliases `xterm-ghostty` → `xterm-256color` for terminal Emacs under Ghostty.

2. **`init.el`** is deliberately tiny. It registers MELPA/NonGNU as fallback archives, puts `lisp/` on `load-path`, points `custom-file` at `var/custom.el` (**write-only** — never loaded back, so the declarative config in git is the single source of truth), and `require`s each module in order. It also has a **bootstrap guard** (`jotain--bootstrapping-p`) that disables `package-quickstart` on the first run after `just clean-all` because a partially-written quickstart file prevents subsequent package installs from working.

3. **`lisp/init-*.el`** — one file per concern. See load order and responsibilities in `docs/architecture/modules.mdx`.

### Critical module conventions

- **No builtins.el/third-party.el split.** A package that only exists to enhance a built-in lives *in the same file* as the built-in it enhances. Examples: `dirvish` and `dired` are both in `init-navigation.el`; `magit` and `vc` are both in `init-vc.el`; `corfu` and `completion-preview` are both in `init-completion.el`. This is a deliberate design choice — don't refactor toward the split.
- **`setopt`, not `setq`, for `defcustom` variables.** `setopt` runs `:set` callbacks and `:type` validation; `setq` silently bypasses them. Many user options only work correctly under `setopt`.
- **`use-package-always-ensure = t`** means every `use-package` block defaults to "install if missing". Built-ins must opt out with `:ensure nil`. Packages that Nix puts on `load-path` also use `:ensure nil` so `use-package` finds them without touching the network.
- **Module file shape**: start with `-*- lexical-binding: t; -*-` cookie, end with `(provide 'init-<concern>)`, and add a `(require 'init-<concern>)` line in `init.el` at the appropriate point in the load order.
- **LSP wiring lives in `init-prog.el`.** Per-language `eglot-ensure` hooks are centralised there so all LSP config is in one place; `init-lang-*.el` files only hold mode regexes and language-specific tweaks. Formatters are centralised through `apheleia` in `init-prog.el`.
- **Langs: `init-lang-nix/rust/python` get their own files; less-used modes are grouped** (`init-lang-web`, `init-lang-devops`, `init-lang-data`, `init-lang-systems`). Don't split a single language out of a grouped file unless it grows enough to justify its own file.

### Nix build layer — cache-parity invariant

**This is the most important invariant in the repo:** every default in `emacs.nix`'s argument list must match the corresponding default in upstream nixpkgs `make-emacs.nix`. When that holds, `import ./emacs.nix {}` produces the **exact same store path** as `pkgs.emacs30`, so the default build is a binary-cache hit (from `cache.nixos.org` + the `jylhis` cachix cache) and nothing is rebuilt from source.

Divergent variants (`git`, `unstable`, `igc`, `macport`, Darwin patch flags) go through `overrideAttrs` and *intentionally* bust the cache. Any edit to `emacs.nix` that touches the `basePackage.override { … }` block must preserve parity for the mainline variant. Verify with:

```
nix-instantiate --eval --strict -E \
  '(import ./emacs.nix {}).outPath == (import (import ./npins).nixpkgs-unstable {}).emacs30.outPath'
```

`default.nix` wraps `emacs.nix` and, by default, adds `treesit-grammars.with-all-grammars` via `emacsPackagesFor`. It forwards all `emacs.nix` arguments, so variant flags (`withPgtk`, `variant`, `noGui`, …) work the same way whether you go through `default.nix` or `emacs.nix` directly.

### Dev shell's Emacs

`devenv.nix` builds its own `jotainEmacs` (from `emacs.nix` + two MELPA-absent packages via `trivialBuild`: `claude-code-ide` and `combobulate`) and exposes it as the `emacs` binary on `PATH`. The custom `languages.emacs-lisp` module from `nix/devenv-emacs-lisp.nix` provides `eask-cli` alongside; `ellsp` and `elsa` are available but opt-in (both defaulted off in `devenv.nix`).

`module.nix` is a Home Manager module (`services.jotain`) for running Jotain as a user-session Emacs daemon with `emacsclient`; it supports systemd on Linux and launchd on macOS.

### Pinning

Sources are managed with `npins` (not flakes). `npins/` holds at minimum `nixpkgs-unstable`. `devenv.nix` imports `./npins` and uses the pinned nixpkgs as `pinned` for everything that needs "the same nixpkgs" — **prefer `pinned` over the ambient `pkgs`** inside `devenv.nix` when consistency matters.
