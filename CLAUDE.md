# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Jotain is a GNU Emacs 30+ configuration with a Nix build layer. The repo ships **both** a modular Elisp configuration (`early-init.el`, `init.el`, `lisp/init-*.el`) and the Nix expressions that build Emacs itself (`emacs.nix`, `overlay.nix`, `default.nix`, `flake.nix`). The two are coupled: `devenv.nix` builds the dev-shell `emacs` from `emacs.nix` so the editor in the shell is exactly what `just build` produces.

## Development environment

All recipes assume the devenv shell is active. `direnv` handles this via `.envrc`; without direnv, prefix any command with `devenv shell --`:

```
devenv shell -- just check
```

CI (`.github/workflows/ci.yml`) runs two parallel jobs across all four supported platforms (`x86_64-linux`, `aarch64-linux`, `x86_64-darwin`, `aarch64-darwin`): `check` (`nix flake check`, which already builds `packages-default`, `packages-emacs`, `packages-info`, and `options-doc` as flake checks — so a separate `nix build` job would duplicate work) and `test` (`devenv test` — the seven `enterTest` assertions in `devenv.nix`). A workflow-level `concurrency` group cancels superseded runs on PR branches while preserving protected-branch runs so post-merge cache pushes always finish. The `jylhis` cachix cache is used for pulling and pushing artifacts.

## Common commands

All day-to-day work goes through the `Justfile`:

- `just check` — runs `nix flake check`, which covers: formatting (treefmt), statix, deadnix, elisp paren-check, elisp byte-compile (warnings as errors), and package evaluation. Does NOT run `devenv test` (use `devenv test` separately for dev-environment assertions). Use `just check-elisp` for the Elisp-only subset.
- `just compile` — byte-compile the full config with `byte-compile-error-on-warn`. Requires packages to already be installed (run `just run` once first to trigger bootstrap).
- `just test` — run ERT tests under `test/` if the directory exists (no tests yet).
- `just run [ARGS]` — launch Emacs with `--init-directory` pointed at this repo (isolated from `~/.emacs.d`).
- `just debug` — same as `run` but with `--debug-init` and `debug-on-error`.
- `just tty` — `emacs -nw` (exercises kkp + clipetty paths).
- `just fmt` — `nixfmt .` (Nix formatting via `nixfmt-rfc-style`).
- `just update` — update nixpkgs in `flake.lock` (source of truth), then sync `devenv.lock` to match.
- `just verify` — verify that `flake.lock` and `devenv.lock` reference the same nixpkgs revision.
- `just clean` — remove `*.elc`, autosaves, `eln-cache`, `result` symlink.
- `just clean-all` — additionally wipe `elpa/` and `var/`, forcing a full re-fetch. The next `just run` will hit every MELPA package fresh, so expect the first startup to be slow.

Build recipes (all via `nix-build`, targeting current system by default; override with `just system=x86_64-linux …`):

- `just build` — full distribution (Emacs + all ~275 tree-sitter grammars) via plain `nix-build`.
- `just build-bare` — bare Emacs via `emacs.nix`, no grammars.
- `just build-pgtk` / `build-gtk3` / `build-nox` / `build-macport` / `build-git` / `build-igc` / `build-android` — variant builds targeting `emacs.nix` directly (bare Emacs only). Git/unstable/igc/macport variants will fail on first run and report the hash to pass back via `--argstr hash`.
- `just run-built [ARGS]` — auto-detect platform, build, then launch from `./result/bin/emacs`.
- `just docs` / `just info` / `just docs-all` — build the Nix-options HTML reference, the bundled `jotain.info` manual, or both. HTML goes to `result-docs/`; Info goes to `result-info/share/info/jotain.info`.

There's also an `emacs-smoke` script (defined in `devenv.nix`) that byte-compiles everything with warnings-as-errors, and `emacs-run` which is the non-Justfile equivalent of `just run`.

- `just bench [output]` — benchmark startup: launches Emacs through `bench/early-init.el` which wraps `require` and `package-refresh-contents` with timing advice, then prints results sorted by load time. Optionally saves to a file.

## Architecture

### Elisp layer — three parts

1. **`early-init.el`** runs before `package.el`, before the first frame, before `init.el`. It sets the startup GC threshold to `most-positive-fixnum`, disables bidi reordering, sets `use-package-always-ensure = t`, **pins `package-quickstart-file` to `var/package-quickstart.el`** so `startup.el`'s automatic `package-activate-all` actually finds it (the default path is outside `var/`, so without the pin quickstart never gets loaded), disables the menu/tool/scroll bars *before* the first frame draws, redirects `native-comp` eln-cache to `var/eln-cache/`, adds `(package reinitialization)` to `warning-suppress-log-types` to silence the false-positive warning emitted by `package-quickstart-refresh`, and aliases `xterm-ghostty` → `xterm-256color` for terminal Emacs under Ghostty.

2. **`init.el`** is deliberately tiny. It registers MELPA/NonGNU as fallback archives, puts `lisp/` on `load-path`, points `custom-file` at `var/custom.el` (**write-only** — never loaded back, so the declarative config in git is the single source of truth), refreshes archive contents on first run, and `require`s each module in order.

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
nix-instantiate --eval --strict -E '
  let lock = builtins.fromJSON (builtins.readFile ./flake.lock);
      n = lock.nodes.nixpkgs.locked;
      nixpkgs = fetchTarball { url = "https://github.com/${n.owner}/${n.repo}/archive/${n.rev}.tar.gz"; sha256 = n.narHash; };
  in (import ./emacs.nix {}).outPath == (import nixpkgs {}).emacs30.outPath'
```

**`overlay.nix`** is a nixpkgs overlay providing three attributes: `jotainEmacs` (bare Emacs from `emacs.nix`), `jotainInfo` (the `jotain.info` manual generated by `nix/info-manual.nix`), and `jotainEmacsPackages` (full distribution — use-package auto-mapping + tree-sitter grammars + a makeBinaryWrapper that prepends `${jotainInfo}/share/info` to `INFOPATH` so `C-h i d m Jotain RET` works out of the box).

**`default.nix`** is a thin flake-compat wrapper. It reads the pinned `flake-compat` rev from `flake.lock`, evaluates `flake.nix`, and promotes the current system's packages to the top level. `nix-build` builds the full distribution; `nix-build -A emacs` builds bare Emacs. Variant builds still target `emacs.nix` directly (e.g. `nix-build emacs.nix --arg withPgtk true`).

**`flake.nix`** is a thin wrapper — no inline derivations, just wiring. It exposes:
- `packages.<system>.default` — full distribution (`jotainEmacsPackages`)
- `packages.<system>.emacs` — bare Emacs (`jotainEmacs`)
- `packages.<system>.info` — `jotain.info` manual alone (`jotainInfo`), for `just info`
- `packages.<system>.docs` — HTML option reference (for GitHub Pages)
- `overlays.default` — the nixpkgs overlay from `overlay.nix`
- `homeManagerModules.default` — Home Manager module from `module.nix` (per-user daemon, wrappers, desktop entry)
- `nixosModules.default` / `darwinModules.default` — NixOS / nix-darwin module from `module-system.nix` (overlay + system packages)
- `formatter.<system>` — `treefmt` wrapper using shared config from `nix/treefmt.nix`
- `lib` — use-package scanner utilities from `nix/use-package.nix`
- `checks.<system>.*` — defined in `nix/checks.nix`: package builds, formatting (treefmt), statix, deadnix, elisp-lint (paren check), elisp-compile (byte-compile with warnings-as-errors)

### Dev shell's Emacs

`devenv.nix` builds `jotainEmacs` by applying `overlay.nix` to devenv's ambient `pkgs` (two MELPA-absent packages, `claude-code-ide` and `combobulate`, are added via `trivialBuild` in `nix/extra-packages.nix`). This derivation is exposed as the `emacs` binary on `PATH`. The custom `languages.emacs-lisp` module from `nix/devenv-emacs-lisp.nix` provides `eask-cli` alongside; `ellsp` and `elsa` are available but opt-in (both defaulted off in `devenv.nix`). The dev shell also includes `statix` and `deadnix` for Nix linting.

`module.nix` is a Home Manager module (`services.jotain`) for running Jotain as a user-session Emacs daemon with `emacsclient`; it supports systemd on Linux and launchd on macOS. `module-system.nix` is a shared NixOS / nix-darwin module that applies the overlay and adds packages to `environment.systemPackages` — it is intentionally a single file reused by both `nixosModules.default` and `darwinModules.default`.

### Pinning

`flake.lock` is the single source of truth for input revisions. Two lock files must stay in sync: `flake.lock` and `devenv.lock`. `devenv.yaml` pins shared inputs (`nixpkgs` and `treefmt-nix`) to the exact same commits as `flake.lock` to ensure binary cache hits.

Use `just update` to update flake inputs first, then sync `devenv.yaml` URLs and `devenv.lock` to match. Use `just verify` to check that both locks agree on all shared inputs.

`default.nix` and `emacs.nix` read nixpkgs from `flake.lock` directly via `fetchTarball` — no separate pinning tool is needed for non-flake consumers.

### Shared treefmt configuration

`nix/treefmt.nix` defines the formatter programs (currently `nixfmt`, `deadnix`, and `statix`). It is consumed by both `flake.nix` (via `treefmt-nix` for `nix fmt` and the formatting check) and `devenv.nix` (via devenv's treefmt module). Add new formatters to this single file.

### Check / test responsibility split

Flake checks (`nix flake check`, defined in `nix/checks.nix`) validate the **application and configuration**: Nix linting (statix, deadnix, nixfmt), Elisp syntax (balanced parens), and Elisp byte-compilation (warnings as errors). devenv tests (`devenv test`, defined in `devenv.nix` `enterTest`) validate the **dev environment**: binary provenance, isolation from host config, and tooling availability.

### Info manual

`docs/jotain.texi` is the master Texinfo file; `nix/info-manual.nix` pandoc-converts every `docs/*.md(x)` page into a chapter fragment, `@include`s them, and runs `makeinfo` + `install-info` to produce `share/info/{jotain.info,dir}`. The Nix module options appendix comes from `nix/options-doc.nix`, which now emits both `index.html` (for Pages) and `jotain-options.texi` (for the Info manual) from the same CommonMark source. `overlay.nix` wraps `jotainEmacsPackages`' binaries with `INFOPATH=${jotainInfo}/share/info:` so the manual is discoverable for NixOS / nix-darwin / Home Manager users without any Elisp config. For source-checkout (`just run` against a host Emacs that isn't the Jotain wrapper), `lisp/init-docs.el` scans `JOTAIN_INFO_DIR`, `result-info/share/info`, and `result/share/info` and adds the first that exists to `Info-directory-list`.
