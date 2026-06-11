# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Jotain is a GNU Emacs 31+ configuration with a Nix build layer. The repo ships **both** a modular Elisp configuration (`early-init.el`, `init.el`, `lisp/init-*.el`) and the Nix expressions that build Emacs itself (`emacs.nix`, `overlay.nix`, `nix/mk-overlay.nix`, `default.nix`, `flake.nix`). Note: Emacs is **temporarily not installed into the devenv shell** — its ~1 GB closure dominated `direnv allow` time (see the top-of-file note in `devenv.nix`). The Nix-side builds (`just build*`, `nix flake check`, the flake `packages.<system>.*` outputs) are unaffected, and the run/compile/test/bench `just` recipes print a disabled notice until Emacs is wired back into the shell. Use `just run-built` to build Emacs via Nix and launch it.

## Development environment

All recipes assume the devenv shell is active. `direnv` handles this via `.envrc`; without direnv, prefix any command with `devenv shell --`:

```
devenv shell -- just check
```

CI (`.github/workflows/ci.yml`) runs two parallel jobs on `x86_64-linux` (Darwin and aarch64 are not exercised in CI): `check` (`nix flake check`, which already builds `packages-default`, `packages-emacs`, `packages-info`, and `options-doc` as flake checks — so a separate `nix build` job would duplicate work) and `test` (`devenv test` — the seven `enterTest` assertions in `devenv.nix`). Because Emacs is no longer in the dev shell, those seven assertions now check the **dev-shell tooling** (Nix tools, Meson/Ninja, sonarlint-ls, rass, pandoc/makeinfo, eca, tagref) rather than Emacs provenance; the Emacs binary-provenance guarantee moved to the `checks.<system>.emacs-binaries` flake check in `nix/checks.nix`. A workflow-level `concurrency` group cancels superseded runs on PR branches while preserving protected-branch runs so post-merge cache pushes always finish. The `jylhis` cachix cache is used for pulling and pushing artifacts.

## Common commands

All day-to-day work goes through the `Justfile`:

- `just check` — runs `nix flake check`, which covers: formatting (treefmt), statix, deadnix, elisp paren-check, elisp byte-compile (warnings as errors), and package evaluation. Does NOT run `devenv test` (use `devenv test` separately for dev-environment assertions). This is the primary verification entry point now that the Emacs-dependent recipes below are disabled.
- `just fmt` — `nix fmt` (Nix formatting via the shared treefmt config).
- `just update` — update nixpkgs in `flake.lock` (source of truth), then sync `devenv.yaml`/`devenv.lock` to match.
- `just verify` — verify that `flake.lock` and `devenv.lock` agree on every shared input rev (`nixpkgs`, `treefmt-nix`, `emacs-overlay`).
- `just clean` — remove `*.elc`, autosaves, `eln-cache`, `result` symlink.
- `just clean-all` — additionally wipe `elpa/`, `var/`, and `.dev-home/`, forcing a full re-fetch.

The following recipes are **`[DISABLED]` stubs** while Emacs is out of the dev shell — each prints a notice and exits 0: `just compile`, `just test`, `just check-elisp`, `just run`, `just debug`, `just tty`, `just daemon`, `just client`, `just client-tty`, `just quick`, `just bench`, `just bench-open`. Their byte-compile / paren-check coverage now lives in the `elisp-compile` and `elisp-lint` flake checks (`just check`); to launch the editor use `just run-built`.

Build recipes (all via `nix-build`, targeting current system by default; override with `just system=x86_64-linux …`):

- `just build` — full distribution (Emacs + all ~275 tree-sitter grammars) via plain `nix-build`.
- `just build-bare` — bare Emacs via `emacs.nix`, no grammars.
- `just build-jylhis` / `build-jylhis-full` — the `github:jylhis/emacs` Meson fork (bare / full package set; the full set is experimental).
- `just build-pgtk` / `build-gtk3` / `build-nox` / `build-macport` / `build-git` / `build-igc` / `build-android` — variant builds targeting `emacs.nix` directly (bare Emacs only). Git/unstable/igc/macport variants will fail on first run and report the hash to pass back via `--argstr hash`.
- `just run-built [ARGS]` — auto-detect platform, build, then launch from `./result/bin/emacs` (this is how you launch the editor while Emacs is out of the dev shell).
- `just docs` / `just info` / `just docs-all` — build the Nix-options HTML reference, the bundled `jotain.info` manual, or both. HTML goes to `result-docs/`; Info goes to `result-info/share/info/jotain.info`.
- `just build-packages-doc` / `just docs-refresh-packages` — build the per-package reference and regenerate `docs/configuration/package-reference.mdx` from the `;;; @doc` markers (CI's `packages-doc-in-sync` check fails if it drifts).

(The `emacs-smoke` and `emacs-run` devenv scripts, and the `bench/` startup benchmark, are commented out alongside the disabled `just` recipes; re-enable them when Emacs returns to the dev shell.)

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

**This is the most important invariant in the repo:** every default in `emacs.nix`'s argument list must match the corresponding default in `emacs-overlay`'s `emacs-git` derivation (which itself mirrors upstream nixpkgs `make-emacs.nix`). When that holds, `import ./emacs.nix {}` produces the **exact same store path** as `pkgs.emacs-git`, so the default build is a binary-cache hit (from `nix-community.cachix.org` + the `jylhis` cachix cache) and nothing is rebuilt from source.

Emacs 31 is not yet shipped in nixpkgs; the base package comes from [`nix-community/emacs-overlay`](https://github.com/nix-community/emacs-overlay), wired in as a flake input alongside `nixpkgs`. The overlay supplies `emacs-git` (master), `emacs-igc` (feature/igc3), and `emacs-macport`. Divergent variants (`git`, `unstable`, `igc`, `macport`, Darwin patch flags) go through `overrideAttrs` and *intentionally* bust the cache. Any edit to `emacs.nix` that touches the `basePackage.override { … }` block must preserve parity for the mainline variant. Verify with:

```
nix-instantiate --eval --strict -E '
  let lock = builtins.fromJSON (builtins.readFile ./flake.lock);
      n = lock.nodes.nixpkgs.locked;
      ov = lock.nodes.emacs-overlay.locked;
      nixpkgs = fetchTarball { url = "https://github.com/${n.owner}/${n.repo}/archive/${n.rev}.tar.gz"; sha256 = n.narHash; };
      overlay = fetchTarball { url = "https://github.com/${ov.owner}/${ov.repo}/archive/${ov.rev}.tar.gz"; sha256 = ov.narHash; };
      pkgs = import nixpkgs { overlays = [ (import overlay) ]; };
  in (import ./emacs.nix {}).outPath == pkgs.emacs-git.outPath'
```

**`overlay.nix`** is a one-line re-export of **`nix/mk-overlay.nix`** (`import ./nix/mk-overlay.nix { }`) — the actual overlay logic lives in `mk-overlay.nix`. It provides `jotainEmacs` (bare Emacs from `emacs.nix`), `jylhisEmacs` (bare Emacs from the pinned `github:jylhis/emacs` Meson fork), `jotainInfo` (the `jotain.info` manual generated by `nix/info-manual.nix`), `jotainEmacsPackages` (full distribution — use-package auto-mapping + tree-sitter grammars + a makeBinaryWrapper that prepends `${jotainInfo}/share/info` to `INFOPATH` so `C-h i d m Jotain RET` works out of the box), `jylhisEmacsPackages` (the same package/config layer on the fork, currently experimental), `sonarlintLs` (the SonarLint language server from nixpkgs, for `M-x jotain-sonarlint`), and `eca` (the prebuilt ECA server for the `eca-emacs` client). `mk-overlay.nix` references `final.emacs-git` (and the other overlay-supplied variants `emacs.nix` builds on), so it must be composed **after** `emacs-overlay`; the flake's `overlays.default` does exactly that via `lib.composeManyExtensions [ emacs-overlay.overlays.default (mk-overlay) ]`, which makes the exported overlay self-contained for downstream NixOS / nix-darwin / Home Manager consumers (importing `overlay.nix` bare does **not** compose emacs-overlay and only works on a `pkgs` that already has it).

**`default.nix`** is a thin flake-compat wrapper. It reads the pinned `flake-compat` rev from `flake.lock`, evaluates `flake.nix`, and promotes the current system's packages to the top level. `nix-build` builds the full distribution; `nix-build -A emacs` builds bare Emacs. Variant builds still target `emacs.nix` directly (e.g. `nix-build emacs.nix --arg withPgtk true`).

**`flake.nix`** is a thin wrapper — no inline derivations, just wiring. It exposes:
- `packages.<system>.default` — full distribution (`jotainEmacsPackages`)
- `packages.<system>.emacs` — bare Emacs (`jotainEmacs`)
- `packages.<system>.jylhis-emacs` — bare `github:jylhis/emacs` Meson fork
- `packages.<system>.jylhis-emacs-packages` — full Jotain setup on the fork (experimental)
- `packages.<system>.info` — `jotain.info` manual alone (`jotainInfo`), for `just info`
- `packages.<system>.docs` — HTML option reference (for GitHub Pages)
- `packages.<system>.packages-doc` — per-package HTML/texi/mdx reference (`nix/packages-doc.nix`)
- `overlays.default` — the nixpkgs overlay, `emacs-overlay` composed with `nix/mk-overlay.nix` (self-contained; supplies `emacs-git` itself)
- `homeManagerModules.default` — Home Manager module from `module.nix` (per-user daemon, wrappers, desktop entry)
- `nixosModules.default` / `darwinModules.default` — NixOS / nix-darwin module from `module-system.nix` (overlay + system packages)
- `formatter.<system>` — `treefmt` wrapper using shared config from `nix/treefmt.nix`
- `lib` — use-package scanner utilities from `nix/use-package.nix`
- `checks.<system>.*` — defined in `nix/checks.nix`: package builds, formatting (treefmt), statix, deadnix, elisp-lint (paren check), elisp-compile (byte-compile with warnings-as-errors)

### Dev shell

Emacs is **currently not in the dev shell** — applying `jotainEmacsPackages` added a ~1 GB closure that dominated `direnv allow` time, so `devenv.nix` comments out the `jotainEmacs` let-binding, the `languages.emacs-lisp` module (which would supply `eask-cli`/`ellsp`/`elsa`), and the `emacs-smoke`/`emacs-run` scripts (see the top-of-file note for the exact re-enable steps). What the shell *does* provide on `PATH`: the Nix toolchain (`nil`, `nixfmt-rfc-style`, `statix`, `deadnix`), Meson/Ninja (for `meson-mode` + apheleia + compile-multi), `sonarlint-ls` (`M-x jotain-sonarlint`), `rass` (the rassumfrassum LSP multiplexer, built inline from PyPI), `eca` (the ECA server, built from `nix/eca-server.nix`), `tagref`, `docker-langserver`, the docs chain (`pandoc`, `texinfo`), and the fonts `init-ui.el` looks up by name.

The extra Emacs packages that aren't on any archive are built via `trivialBuild` in `nix/extra-packages.nix` — currently **four**: `jylhis-emacs-themes`, `claude-code-ide`, `combobulate`, and `tagref` (the Elisp client for the `tagref` CLI). `mk-overlay.nix` layers these in via its `override`, plus `nix-ts-mode` and the tree-sitter grammars through `extraEmacsPackages`.

`module.nix` is a Home Manager module (`services.jotain`) for running Jotain as a user-session Emacs daemon with `emacsclient`; it supports systemd on Linux and launchd on macOS. The optional `services.jotain.sonarlint.enable` adds the SonarLint language server to the Emacs wrapper's `PATH` (opt-in because `sonarlint-ls` pulls in JDK 21). `module-system.nix` is a shared NixOS / nix-darwin module that applies the overlay and adds packages to `environment.systemPackages` — it is intentionally a single file reused by both `nixosModules.default` and `darwinModules.default`.

### Pinning

`flake.lock` is the single source of truth for input revisions. Two lock files must stay in sync: `flake.lock` and `devenv.lock`. `devenv.yaml` pins shared inputs (`nixpkgs` and `treefmt-nix`) to the exact same commits as `flake.lock` to ensure binary cache hits.

Use `just update` to update flake inputs first, then sync `devenv.yaml` URLs and `devenv.lock` to match. Use `just verify` to check that both locks agree on all shared inputs.

`default.nix` and `emacs.nix` read nixpkgs from `flake.lock` directly via `fetchTarball` — no separate pinning tool is needed for non-flake consumers.

### Shared treefmt configuration

`nix/treefmt.nix` defines the formatter programs (currently `nixfmt`, `deadnix`, and `statix`). It is consumed by both `flake.nix` (via `treefmt-nix` for `nix fmt` and the formatting check) and `devenv.nix` (via devenv's treefmt module). Add new formatters to this single file.

### Check / test responsibility split

Flake checks (`nix flake check`, defined in `nix/checks.nix`) validate the **application and configuration**: Nix linting (statix, deadnix, nixfmt), Elisp syntax (balanced parens), Elisp byte-compilation (warnings as errors), and — now that Emacs is out of the dev shell — Emacs binary provenance (the `emacs-binaries` check builds `jotainEmacs` and verifies the binaries run cleanly without leaking host config). devenv tests (`devenv test`, defined in `devenv.nix` `enterTest`) validate the **dev environment**: that the shell tooling (Nix tools, Meson/Ninja, sonarlint-ls, rass, pandoc/makeinfo, eca, tagref) is present and resolves into the Nix store.

### Info manual

`docs/jotain.texi` is the master Texinfo file; `nix/info-manual.nix` pandoc-converts every `docs/*.md(x)` page into a chapter fragment, `@include`s them, and runs `makeinfo` + `install-info` to produce `share/info/{jotain.info,dir}`. The Nix module options appendix comes from `nix/options-doc.nix`, which now emits both `index.html` (for Pages) and `jotain-options.texi` (for the Info manual) from the same CommonMark source. `nix/mk-overlay.nix` wraps `jotainEmacsPackages`' binaries with `INFOPATH=${jotainInfo}/share/info:` so the manual is discoverable for NixOS / nix-darwin / Home Manager users without any Elisp config. For a source checkout (running a host Emacs that isn't the Jotain wrapper — e.g. via `just run-built`), `lisp/init-docs.el` scans `JOTAIN_INFO_DIR`, `result-info/share/info`, and `result/share/info` and adds the first that exists to `Info-directory-list`.
