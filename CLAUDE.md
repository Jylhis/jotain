# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Jotain is a GNU Emacs 31 configuration (floor: Emacs 30.1, per `init.el`'s `Package-Requires`) with a Nix build layer. The repo ships **both** a modular Elisp configuration (`early-init.el`, `init.el`, `lisp/init-*.el`) and the Nix expressions that build Emacs itself (`emacs.nix`, `nix/mk-overlay.nix`, `overlay.nix`, `default.nix`, `flake.nix`). The default build is the emacs-overlay `unstable` variant — the Emacs 31 release branch, currently the 31.0.90 pretest — with nixpkgs' Emacs 30 kept as the `mainline` variant. The dev shell provides tooling only; Emacs itself is **not** in the shell — build and launch the editor with `just run-built`.

## Development environment

All recipes assume the devenv shell is active. `direnv` handles this via `.envrc`; without direnv, prefix any command with `devenv shell --`:

```
devenv shell -- just check
```

The shell contains Nix/lint/docs tooling and the language servers the config shells out to (see "Dev shell" below) but **no `emacs` binary** — the ~1 GB `jotainEmacsPackages` closure dominated `direnv allow` time, so it was removed (top-of-file note in `devenv.nix`). Every Justfile recipe that needs a live Emacs is a `[DISABLED]` echo-and-exit-0 stub.

CI is split across two workflows, both x86_64-linux only (Darwin and aarch64 are not exercised):

- **PR CI** (`.github/workflows/ci.yml`, pull requests only) runs a lightweight per-check subset as separate named steps — formatting, statix, deadnix, module-eval, packages-doc-in-sync, packages-doc, options-doc, elisp-lint, elisp-compile — plus a parallel `devenv test` job. Cachix is **pull-only** here (pulls `jylhis` and `nix-community`; no `CACHIX_AUTH_TOKEN` is exposed to untrusted PR code). Superseded runs on the same PR are cancelled.
- **Full validation** (`.github/workflows/deploy.yml`, pushes to `main`/`next` plus manual dispatch) runs the complete `nix flake check` — including the heavy package builds and `elisp-test` that PR CI skips — plus `devenv test`, pushes newly built artifacts to the `jylhis` cachix cache, and (on `main` only) builds and deploys the GitHub Pages docs. Its concurrency group never cancels in-progress runs, so post-merge cache pushes always finish.

Consequence: a local `nix flake check` is strictly *heavier* than PR CI — it can fail on things PR CI never exercises, and the deploy-only checks gate `main` invisibly to PRs.

## Common commands

All day-to-day work goes through the `Justfile`:

- `just check` — `nix flake check`: every check defined in `nix/checks.nix` (see "Check / test responsibility split" for the list). Does NOT run `devenv test` (use `devenv test` separately for dev-environment assertions).
- `just test` — build the `elisp-test` flake check (the ERT tests under `test/`).
- `just fmt` — `nix fmt` (the treefmt wrapper: `nixfmt-rfc-style`, deadnix, statix — not a plain `nixfmt .`).
- `just update` — `nix flake update`, then sync `devenv.yaml`/`devenv.lock` to the same revisions for the shared inputs (nixpkgs, treefmt-nix, emacs-overlay).
- `just verify` — assert `flake.lock` and `devenv.lock` agree on every shared input's rev (nodes resolved through each lock's root input map).
- `just clean` — remove `*.elc`, autosaves, `eln-cache`, the `result` symlink.
- `just clean-all` — additionally wipe `elpa/`, `var/`, and `.dev-home/`, forcing a full re-fetch; expect the next first startup to be slow.

**Disabled stubs** (Emacs is not in the dev shell; each prints a notice and exits 0): `just run`, `debug`, `tty`, `daemon`, `client`, `client-tty`, `quick`, `check-elisp`, `compile`, `compile-native`, `bench`, and `bench-open`. Their coverage lives in the `elisp-lint`/`elisp-compile`/`elisp-test` flake checks; the launch path is `just run-built`. The former `emacs-smoke`/`emacs-run` devenv scripts are likewise commented out in `devenv.nix`.

Build & launch recipes (targeting the current system by default; override with `just system=x86_64-linux …`):

- `just run-built [ARGS]` — **the way to launch this config**: auto-detect the platform, build the full distribution (on aarch64-linux, the full terminal-only distribution), then launch `./result/bin/emacs` with `--init-directory` pointed at this repo (isolated from `~/.emacs.d`).
- `just run-built-debug [ARGS]` — same, plus `--debug-init` and `debug-on-error`.
- `just build` — full distribution (`jotainEmacsPackages`: Emacs + all ~275 tree-sitter grammars) via plain `nix-build`.
- `just build-lite` — full distribution with the curated ~26-grammar subset (`packages.emacs-lite`). Opt-in.
- `just build-nox-full` — full terminal-only distribution (`packages.emacs-nox`, the same attribute the nix-on-droid module ships).
- `just build-bare` — bare Emacs via `emacs.nix` (the bare file's own default is the `mainline` variant, Emacs 30); `just build-bare-lite` additionally trims xwidgets/mailutils.
- `just build-pgtk` / `build-gtk3` / `build-nox` / `build-macport` / `build-git` / `build-igc` / `build-android` — bare-Emacs variant builds targeting `emacs.nix` directly. Default revisions are binary-cache hits; only when pinning a custom commit via `--argstr rev` does the first run fail and report the hash to pass back via `--argstr hash`.
- `just build-jylhis` — the bare `github:jylhis/emacs` Meson fork.
- `just screenshot [out]` — headless capture: build, launch under Xvfb with this config, write a PNG via `jotain-screenshot` (Linux only; needs `xvfb-run` from the shell).
- `just docs` / `just info` / `just docs-all` — build the Nix-options HTML reference (→ `result-docs/`), the bundled `jotain.info` manual (→ `result-info/share/info/jotain.info`), or both.
- `just build-packages-doc` / `just docs-refresh-packages` — build the per-package reference / regenerate `docs/configuration/package-reference.mdx` from the `;;; @doc` markers in `lisp/`. Run the latter after editing any `@doc` block, or CI's `packages-doc-in-sync` check fails.

## Emacs & Elisp knowledge base and skills

Two on-demand skills in `.claude/skills/` hold source-cited reference material
for this Emacs 30/31 config; `.claude/knowledge/emacs/README.md` indexes them.
**Use them instead of answering Emacs questions from memory** — each reference
traces back to the GNU Emacs Lisp Reference Manual, `src/` commentary, or NEWS.

- **`emacs-internals`** — GC & object representation, the gap buffer /
  markers / overlays, redisplay, the command loop & keymaps, byte & native
  compilation, threads, and build/dump. Reach for it when explaining or
  debugging core behavior, tuning performance (GC, redisplay, startup), or
  reasoning about native-comp / eln caches. Repo anchors it already documents:
  the `gc-cons-threshold` startup dance and eln-cache redirect in
  `early-init.el`, and the `igc` build variant.
- **`elisp-dev`** — naming/file conventions, lexical binding, macro hygiene,
  hooks vs. advice, `defcustom`/`setopt`, `use-package` patterns, modern
  libraries (pcase/seq/map/cl-lib/rx), debugging & ERT, and an Emacs 30/31
  changes digest. Reach for it when writing or reviewing any Elisp.

The skills encode the repo's hard rules (`setopt` not `setq` for options,
`:ensure nil` for built-ins/Nix packages, no builtins/third-party split,
warning-clean byte-compilation, LSP/formatters centralized in `init-prog.el`).
When a note conflicts with the installed Emacs, trust the running Emacs
(`C-h f`/`C-h v`/`C-h S`) and fix the reference in the same change.

## Architecture

### Elisp layer — three parts

1. **`early-init.el`** runs before `package.el`, before the first frame, before `init.el`. It sets the startup GC threshold to `most-positive-fixnum`, disables bidi reordering, sets `use-package-always-ensure = t`, **pins `package-quickstart-file` to `var/package-quickstart.el`** so `startup.el`'s automatic `package-activate-all` actually finds it (the default path is outside `var/`, so without the pin quickstart never gets loaded) — and, because the quickstart file caches absolute `/nix/store` load-path entries, deletes it whenever an `EMACSLOADPATH` hash stamp shows the Nix deployment changed — disables the menu/tool/scroll bars *before* the first frame draws, redirects `native-comp` eln-cache to `var/eln-cache/`, adds `(package reinitialization)` to `warning-suppress-log-types` to silence the false-positive warning emitted by `package-quickstart-refresh`, and aliases `xterm-ghostty` → `xterm-256color` for terminal Emacs under Ghostty.

2. **`init.el`** is deliberately tiny. It registers MELPA/NonGNU as fallback archives, puts `lisp/` on `load-path`, points `custom-file` at `var/custom.el` (**write-only** — never loaded back, so the declarative config in git is the single source of truth), and `require`s each module in order. Package archive refresh is *not* on the startup path: a staleness-gated background refresh runs on an idle timer only when the newest cached `archive-contents` is older than 7 days (or an archive has no cache at all), with a `JOTAIN_NO_PACKAGE_REFRESH` env-var opt-out.

3. **`lisp/init-*.el`** — one file per concern. See load order and responsibilities in `docs/architecture/modules.mdx`.

### Critical module conventions

- **No builtins.el/third-party.el split.** A package that only exists to enhance a built-in lives *in the same file* as the built-in it enhances. Examples: `dirvish` and `dired` are both in `init-navigation.el`; `magit` and `vc` are both in `init-vc.el`. This is a deliberate design choice — don't refactor toward the split.
- **`setopt`, not `setq`, for `defcustom` variables.** `setopt` runs `:set` callbacks and `:type` validation; `setq` silently bypasses them. Many user options only work correctly under `setopt`.
- **`use-package-always-ensure = t`** means every `use-package` block defaults to "install if missing". Built-ins must opt out with `:ensure nil`. Packages that Nix puts on `load-path` also use `:ensure nil` so `use-package` finds them without touching the network.
- **Module file shape**: start with `-*- lexical-binding: t; -*-` cookie, end with `(provide 'init-<concern>)`, and add a `(require 'init-<concern>)` line in `init.el` at the appropriate point in the load order.
- **LSP wiring lives in `init-prog.el`.** Per-language `eglot-ensure` hooks are centralised there so all LSP config is in one place; `init-lang-*.el` files only hold mode regexes and language-specific tweaks. Formatters are centralised through `apheleia` in `init-prog.el`.
- **Langs: `init-lang-nix/rust/python/go` get their own files; less-used modes are grouped** (`init-lang-web`, `init-lang-devops`, `init-lang-data`, `init-lang-systems`). Don't split a single language out of a grouped file unless it grows enough to justify its own file (Go earned its file once it grew a gopls workspace config, `go-tag`/`gotest` helpers, and dape debugging).

### Nix build layer — cache-parity invariant

**This is the most important invariant in the repo:** every default in `emacs.nix`'s argument list must match the corresponding default in upstream nixpkgs `make-emacs.nix` (and the explicit args `emacs-overlay` passes to its prebuilt attrs). When that holds, `import ./emacs.nix {}` produces the **exact same store path** as `pkgs.emacs`, and the `git`/`unstable`/`igc` variants the store paths of `pkgs.emacs-git`/`emacs-unstable`/`emacs-igc`, so every default-rev build is a binary-cache hit and nothing is rebuilt from source. The **distribution default is the `unstable` variant**: `nix/mk-overlay.nix` builds `jotainEmacs` with `variant = "unstable"` — emacs-overlay's `emacs-unstable`, the Emacs 31 release branch (currently the 31.0.90 pretest) — so the caches that matter day-to-day are `nix-community.cachix.org` (which carries `emacs-unstable` and the other overlay variants) plus the `jylhis` cachix cache; Hydra covers the `mainline` variant.

The base package map: `unstable` (the distribution default, set in `nix/mk-overlay.nix`) plus `git`/`igc` come from [`nix-community/emacs-overlay`](https://github.com/nix-community/emacs-overlay), wired in as a flake input alongside `nixpkgs`; `mainline` (bare `emacs.nix`'s own default, exposed as `packages.<system>.emacs-mainline`) is nixpkgs' default `pkgs.emacs` attribute (currently the Emacs 30 release); `macport` is nixpkgs' `emacs-macport` alias (→ `emacs30-macport`, the jdtsmith/emacs-mac fork — emacs-overlay does not ship a macport). Only custom `rev` pins and the Darwin patch flags go through `overrideAttrs` and *intentionally* bust the cache. Any edit to `emacs.nix` that touches the `basePackage.override { … }` block must preserve parity for the `unstable` variant (the shipped default) — and ideally every other variant. Verify with:

```
nix-instantiate --eval --strict -E '
  let lock = builtins.fromJSON (builtins.readFile ./flake.lock);
      n  = lock.nodes.${lock.nodes.root.inputs.nixpkgs}.locked;
      ov = lock.nodes.${lock.nodes.root.inputs.emacs-overlay}.locked;
      nixpkgs = fetchTarball { url = "https://github.com/${n.owner}/${n.repo}/archive/${n.rev}.tar.gz"; sha256 = n.narHash; };
      overlay = fetchTarball { url = "https://github.com/${ov.owner}/${ov.repo}/archive/${ov.rev}.tar.gz"; sha256 = ov.narHash; };
      pkgs = import nixpkgs { overlays = [ (import overlay) (import ./overlay.nix) ]; };
  in {
    default  = pkgs.jotainEmacs.outPath == pkgs.emacs-unstable.outPath;
    mainline = (import ./emacs.nix {}).outPath == pkgs.emacs.outPath;
    git      = (import ./emacs.nix { variant = "git"; }).outPath == pkgs.emacs-git.outPath;
    unstable = (import ./emacs.nix { variant = "unstable"; }).outPath == pkgs.emacs-unstable.outPath;
    igc      = (import ./emacs.nix { variant = "igc"; }).outPath == pkgs.emacs-igc.outPath;
  }'
```

**Older-nixpkgs portability.** The override arg set is filtered through `lib.intersectAttrs (lib.functionArgs basePackage.override) overrideArgs`, so only the arguments the base `make-emacs.nix` actually defines are forwarded. This lets a downstream flake override `nixpkgs` with an older release (24.05+, e.g. via `inputs.jotain.inputs.nixpkgs.follows`) still **evaluate and build** — flags newer `make-emacs.nix` versions added (`noGui`, `srcRepo`, `withGcMarkTrace`, `withGlibNetworking`, …) are dropped instead of throwing "called with unexpected argument". On the pinned unstable every argument is accepted (`functionArgs` returns the make-emacs formals), so the intersection is a no-op and cache parity is unchanged — verified the mainline/git/unstable/igc store paths stay byte-identical to the explicit full override. Caveat: on 24.05 `pkgs.emacs` is Emacs 29, but the Elisp config targets Emacs 30/31 — the Nix layer builds, full runtime correctness on 29 is not guaranteed.

**`nix/mk-overlay.nix`** is the overlay implementation, parameterized on `variant` (default `"unstable"`) and `curatedGrammars`; **`overlay.nix`** is a thin standalone wrapper that composes nix-community/emacs-overlay (pinned via `flake.lock`'s root input map, same discipline as `emacs.nix`) underneath it, so `import ./overlay.nix` works without the flake. The overlay provides: `jotainEmacs` / `jotainEmacsNoGui` (bare Emacs from `emacs.nix`, unstable variant; the `noGui` twin is used by the nix-on-droid module), `jylhisEmacs` (bare Emacs from the `github:jylhis/emacs` Meson fork), `jotainInfo` (the `jotain.info` manual generated by `nix/info-manual.nix`), `jotainEmacsPackages` / `jotainEmacsPackagesNoGui` (full distributions — use-package auto-mapping + tree-sitter grammars + a makeBinaryWrapper that appends `${jotainInfo}/share/info` to `INFOPATH` so `C-h i d m Jotain RET` works out of the box), `jylhisEmacsPackages` (the same package/config layer on the fork, currently experimental), `eca` (the prebuilt ECA server binary for `lisp/init-ai.el`), and `sonarlintLs` (the SonarLint language server from nixpkgs, for `M-x jotain-sonarlint`).

**`default.nix`** is a thin flake-compat wrapper. It reads the pinned `flake-compat` rev from `flake.lock`, evaluates `flake.nix`, and promotes the current system's packages to the top level. `nix-build` builds the full distribution; `nix-build -A emacs` builds bare Emacs. Variant builds still target `emacs.nix` directly (e.g. `nix-build emacs.nix --arg withPgtk true`).

**`flake.nix`** is a thin wrapper — no inline derivations, just wiring. It exposes:
- `packages.<system>.default` — full distribution (`jotainEmacsPackages`)
- `packages.<system>.emacs-lite` — full distribution with the curated ~26-grammar subset (opt-in)
- `packages.<system>.emacs` — bare Emacs (`jotainEmacs`, unstable variant)
- `packages.<system>.emacs-mainline` — bare Emacs on nixpkgs' default attr (Emacs 30, Hydra-cached escape hatch)
- `packages.<system>.emacs-nox` — full terminal-only distribution (`jotainEmacsPackagesNoGui`)
- `packages.<system>.emacs-jylhis` / `jylhis-emacs` — bare `github:jylhis/emacs` Meson fork
- `packages.<system>.info` — `jotain.info` manual alone (`jotainInfo`), for `just info`
- `packages.<system>.docs` — HTML option reference (for GitHub Pages)
- `packages.<system>.packages-doc` — per-package reference (HTML + texi + Mintlify `.mdx`)
- `overlays.default` — the overlay from `nix/mk-overlay.nix`
- `homeManagerModules.default` — Home Manager module from `module.nix` (per-user daemon, wrappers, desktop entry)
- `nixosModules.default` / `darwinModules.default` — NixOS / nix-darwin module from `module-system.nix` (overlay + system packages)
- `nixOnDroidModules.default` — nix-on-droid module from `module-nix-on-droid.nix` (terminal-only Emacs into `environment.packages`, headless Android under proot)
- `nixOnDroidConfigurations.default` — example nix-on-droid config (aarch64-linux; not built by CI/`nix flake check`)
- `formatter.<system>` — `treefmt` wrapper using shared config from `nix/treefmt.nix`
- `lib` — use-package scanner utilities from `nix/use-package.nix`
- `checks.<system>.*` — defined in `nix/checks.nix`; see "Check / test responsibility split" for the full list

All four module outputs receive the jotain overlay composed with emacs-overlay (`moduleOverlay` in `flake.nix`), so module-installed distributions resolve the same `emacs-unstable` base and epkgs snapshot that CI builds and caches.

### Dev shell

`devenv.nix` provides tooling only — there is **no Emacs in the dev shell** (see its top-of-file note; the `languages.emacs-lisp` module from `nix/devenv-emacs-lisp.nix`, the `emacs-smoke`/`emacs-run` scripts, and the Emacs `enterTest` assertions are all commented out pending re-enablement). What it does ship: Nix tooling and linters (`nil`, `nixfmt`, `statix`, `deadnix`), build tools that modes shell out to (`meson`, `ninja`, `buildifier`), language servers and CLIs the Elisp config invokes (`sonarlint-ls`, `rassumfrassum`/`rass`, `eca`, `tagref`, `dockerfile-language-server`), the docs chain (`pandoc`, `texinfo`), the fonts `init-ui.el` probes (`nerd-fonts.jetbrains-mono`, `nerd-fonts.iosevka`, a Literata-only `google-fonts` subset), and — on Linux — `xvfb-run` for `just screenshot`. Cachix pulls from `jylhis` and `nix-community`. The archive-absent packages the distribution needs (`ghostel`, `jylhis-emacs-themes`, `claude-code-ide`, `combobulate`, `majutsu`, `tagref`) are built via `trivialBuild` in `nix/extra-packages.nix`, consumed by `nix/mk-overlay.nix`.

`module.nix` is a Home Manager module (`services.jotain`) for running Jotain as a user-session Emacs daemon with `emacsclient`; it supports systemd on Linux and launchd on macOS, and byte-compiles the config for the deployment so the daemon loads `.elc` instead of interpreted source. The optional `services.jotain.sonarlint.enable` adds the SonarLint language server to the Emacs wrapper's `PATH` (opt-in because `sonarlint-ls` pulls in JDK 21); further options cover the `devenv` CLI (`services.jotain.devenv.enable`) and jinx spell-check dictionaries (`services.jotain.spell.dictionaries`). The optional `services.jotain.openrouter.enable` installs `config/eca/config.json` to `~/.config/eca/config.json`, defining the OpenRouter (`openai-chat`) provider for the eca server (opt-in so it never clobbers a user's own eca config; the key is read from `$OPENROUTER_API_KEY` at runtime via eca's `${env:…}` syntax). gptel itself defaults to the OpenRouter backend (`lisp/init-ai.el`) regardless of this option. `module-system.nix` is a shared NixOS / nix-darwin module that applies the overlay and adds packages to `environment.systemPackages` — it is intentionally a single file reused by both `nixosModules.default` and `darwinModules.default`. `module-nix-on-droid.nix` is the nix-on-droid counterpart: a trimmed module (no systemd/launchd/`fonts.packages`) that `pkgs.extend`s the overlay and adds a **terminal-only** Jotain Emacs (`jotainEmacsPackagesNoGui`, a `noGui` build) plus an `emacsclient` EDITOR wrapper to `environment.packages`, with `EDITOR`/`VISUAL` set via `environment.sessionVariables` — Android under proot is headless, so a GUI build would only bloat the closure.

### Pinning

`flake.lock` is the single source of truth for input revisions. Two lock files must stay in sync: `flake.lock` and `devenv.lock`. `devenv.yaml` pins the shared inputs (`nixpkgs`, `treefmt-nix`, and `emacs-overlay`) to the exact same commits as `flake.lock` to ensure binary cache hits.

Use `just update` to update flake inputs first, then sync `devenv.yaml` URLs and `devenv.lock` to match. Use `just verify` to check that both locks agree on all shared inputs.

`default.nix`, `emacs.nix`, and `overlay.nix` read their pins from `flake.lock` directly via `fetchTarball`, resolving nodes through the lock's root input map — no separate pinning tool is needed for non-flake consumers.

### Shared treefmt configuration

`nix/treefmt.nix` defines the formatter programs (currently `nixfmt`, `deadnix`, and `statix`). It is consumed by both `flake.nix` (via `treefmt-nix` for `nix fmt` and the formatting check) and `devenv.nix` (via devenv's treefmt module). Add new formatters to this single file.

### Check / test responsibility split

Flake checks (`nix flake check`, defined in `nix/checks.nix`) validate the **application and configuration**: package builds (`packages-default`, `packages-emacs`, `packages-jylhis-emacs`, `packages-info`), docs (`options-doc`, `packages-doc`, and `packages-doc-in-sync`, which byte-diffs the checked-in package reference against the freshly generated one), module evaluation without the host frameworks (`module-eval`, `nix-on-droid-module-eval`), binary smoke tests (`jylhis-emacs-smoke`, and `emacs-binaries`, which runs the built Emacs under a sandboxed HOME and asserts no host-config leakage), Nix linting (`formatting` via treefmt, `statix`, `deadnix`), and Elisp (`elisp-lint` balanced-paren check, `elisp-compile` byte-compile with warnings as errors, `elisp-test` — the ERT suite under `test/`). devenv tests (`devenv test`, defined in `devenv.nix` `enterTest`) validate the **dev environment**: the shell tooling is on `PATH` and resolves into the Nix store (the former Emacs-provenance assertions moved into the `emacs-binaries` flake check when Emacs left the shell).

### Info manual

`docs/jotain.texi` is the master Texinfo file; `nix/info-manual.nix` pandoc-converts every `docs/*.md(x)` page into a chapter fragment, `@include`s them, and runs `makeinfo` + `install-info` to produce `share/info/{jotain.info,dir}`. The Nix module options appendix comes from `nix/options-doc.nix`, which now emits both `index.html` (for Pages) and `jotain-options.texi` (for the Info manual) from the same CommonMark source. `nix/mk-overlay.nix` wraps `jotainEmacsPackages`' binaries to append `${jotainInfo}/share/info` to `INFOPATH` (with a trailing `:` so Emacs's `info-initialize` still appends `Info-default-directory-list` and the built-in manuals stay visible), making the manual discoverable for NixOS / nix-darwin / Home Manager users without any Elisp config. For a source checkout (`just run-built`, or any host Emacs that isn't the Jotain wrapper), `lisp/init-docs.el` scans `JOTAIN_INFO_DIR`, `result-info/share/info`, and `result/share/info` and adds the first that exists to `Info-additional-directory-list`.
