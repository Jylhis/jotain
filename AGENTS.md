# AGENTS.md

Always-loaded context for AI coding agents (Claude Code, Pi, etc.) and the
human contributor guide. This file is the single source of truth for both.

## What this is

This is a GNU Emacs 31 configuration (floor: Emacs 31.1, per `init.el`'s
`Package-Requires`) plus the Nix expressions that build Emacs itself. The
default build is emacs-overlay's `unstable` variant (the newest Emacs
release or pretest tag, currently 31.1). nixpkgs' Emacs 30 is the `mainline`
variant: it stays as the cache-parity canary, but the Elisp config no longer
supports it. The dev shell has
tooling only, **no `emacs`**: build and launch with `just run-built`.

`journal/` is a set of working notes and may be stale; when it
disagrees with the code, the code wins. Finding numbers cited there refer to
the reports under `docs/reviews/`. Older notes and docs may still mention the
retired standalone repo (`flake.nix`, `flake.lock`, GitHub workflows); the
in-tree build replaces them.

## Project Structure & Module Organization

- `early-init.el`, `init.el`, and `lisp/init-*.el` (feature modules,
  `require`d from `init.el` in order).
- Nix at the root: `emacs.nix`, `overlay.nix`, `default.nix`, `module.nix`,
  `module-system.nix`, `module-nix-on-droid.nix`. There is no `flake.nix`: the
  repo root `nix/inputs.nix` builds the `emacs-config` attrset (overlay,
  modules, checks, packages) from these files. Helpers live in `nix/`
  (`mk-overlay.nix` is the overlay implementation, `checks.nix` the checks).
- `docs/` (documentation sources), `bench/` (startup benchmarks), `test/` (ERT;
  every `test/*.el` is loaded by the test check), `etc/` (debug harness,
  vendored `elisp-doc`, lang-eval registry), `scripts/`.

## Development environment

Run recipes from `etc/emacs`, inside the project shell (`direnv allow`, or
prefix a command, e.g. `just _with_nix treefmt .`). The shell has Nix/lint/docs
tooling and the language servers the config shells out to, but no Emacs (its
~1 GB closure made `direnv allow` slow). Paren, compile and test coverage
comes from the `elisp-lint`, `elisp-compile` and `elisp-test` checks. The
root `AGENTS.md` covers the tree-wide commands, commits, changelogs and
pins; this file covers only what is specific to the Emacs project.

### CI

No workflow is specific to this project. The checks in `nix/checks.nix` appear
as `emacs-<name>` in the root `checks` (`outputs.nix`), so `just nix-check` at
the repo root builds them with everything else. The site that serves the
generated docs lives in `projects/services/emacs-docs` (see `emacs-docs(8)`).

## Common commands

All recipes below run in `etc/emacs`. The shared recipes (`nix-build`,
`nix-check`, `info`, ...) come from `platform/just/project.just`.

- `just test`: build every `emacs-*` check of the tree (elisp-lint,
  elisp-compile, scanner-fidelity, packages-doc-in-sync, config-startup,
  elisp-test, the doc and module checks). Build one with
  `nix build -f . local.checks.emacs-<name>` at the repo root.
- `just fmt`: treefmt (nixfmt, deadnix, statix) through the project shell.
  `just lint` is a no-op: `etc/emacs` is excluded from the root treefmt.
- `just build`: the full distribution (`emacsConfigPackages`: Emacs,
  packages, every tree-sitter grammar) via `nix-build`.
- `just run-built [ARGS]`: **the way to launch this config.** Builds the
  distribution and runs `./result/bin/emacs --init-directory=<etc/emacs>`,
  isolated from `~/.emacs.d`.
- `just docs-refresh-packages`: regenerate
  `docs/configuration/package-reference.mdx` from the `;;; @doc` blocks in
  `lisp/`. Run it after editing any `@doc` block, or `packages-doc-in-sync`
  fails.
- `just cache-parity`: the cache-parity check below.
- `just clean`: remove `*.elc`, `var/eln-cache` and `result`.
- Info manual: `nix build -f . local.packages.info-manual -o etc/emacs/result-info`
  at the repo root; root `just info` opens it in `info`.
- Options HTML: `nix build -f . local.checks.emacs-options-doc`.

Other variants build `emacs.nix` directly, e.g.
`nix-build emacs.nix --arg noGui true` (see the header of `emacs.nix` for the
full list of arguments).

## Emacs & Elisp knowledge base

Two archived reference sets in `attic/skills/local/emacs/` hold source-cited
notes for Emacs 30/31 (index: `.claude/knowledge/emacs/README.md`). They are
no longer loaded as skills: read their `SKILL.md` and `references/` files
directly. **Use them instead of answering Emacs questions from memory.**

- **`emacs-internals`**: GC and objects, buffers/markers/overlays,
  redisplay, the command loop and keymaps, byte and native compilation,
  threads, build/dump. Use for core behaviour, performance (GC, redisplay,
  startup), and native-comp/eln caches.
- **`elisp-dev`**: conventions, lexical binding, macros, hooks vs. advice,
  `defcustom`/`setopt`, `use-package`, modern libraries, debugging and ERT,
  an Emacs 30/31 changes digest. Use when writing or reviewing Elisp.

When a note conflicts with the installed Emacs, trust the running Emacs
(`C-h f`/`C-h v`/`C-h S`) and fix the reference in the same change.

## Architecture

### Elisp layer

1. **`early-init.el`** runs before `package.el` and the first frame. It sets
   the startup GC threshold to `most-positive-fixnum`, disables bidi
   reordering, sets `use-package-always-ensure = t`, turns off the tool and
   scroll bars before the first frame, redirects the native-comp eln-cache
   to `var/eln-cache/`, silences the false-positive
   `(package reinitialization)` warning, and aliases `xterm-ghostty` to
   `xterm-256color`. It also pins `package-quickstart-file` to
   `var/package-quickstart.el`, the only place `startup.el`'s automatic
   `package-activate-all` will find it. That file caches absolute
   `/nix/store` load-path entries, so it is deleted whenever an
   `EMACSLOADPATH` hash stamp shows the Nix deployment changed.
2. **`init.el`** is tiny: it registers MELPA/NonGNU as fallback archives,
   puts `lisp/` on `load-path`, points `custom-file` at `var/custom.el`
   (**write-only**, never loaded, so git stays the single source of truth),
   and `require`s each module. Archives are **never fetched at startup**; a
   download happens only when `package-install` finds the cache empty or on
   an explicit `M-x package-refresh-contents` / `list-packages`.
3. **`lisp/init-*.el`**: one file per concern. Load order and
   responsibilities: `docs/architecture/modules.mdx`.

### Critical module conventions

- **No builtins.el/third-party.el split.** A package that enhances a
  built-in lives in the same file as that built-in (`dirvish` with `dired`
  in `init-navigation.el`, `magit` with `vc` in `init-vc.el`). This is
  deliberate; don't refactor toward a split.
- **`setopt`, not `setq`, for `defcustom` variables.** `setq` skips the
  `:set` callback and `:type` validation, and many options only work through
  them.
- **`use-package-always-ensure = t`**, so built-ins and packages Nix puts on
  `load-path` need `:ensure nil` (no network access).
- **Module file shape**: `-*- lexical-binding: t; -*-` cookie, ends with
  `(provide 'init-<concern>)`, plus a `(require 'init-<concern>)` at the
  right point in `init.el`.
- `lisp/devenv.el`, `lisp/repo-review.el` and `lisp/magit-land.el` are the
  only non-`init-*` files in `lisp/`: standalone packages (own `devenv-` /
  `repo-review-` / `magit-land-` namespaces, no `my-` dependencies) wired in
  by `lisp/init-devenv.el` and `lisp/init-review.el`.
- **LSP and formatters live in `init-prog.el`.** Per-language `eglot-ensure`
  hooks and `apheleia` formatters are centralised there; `init-lang-*.el`
  holds only mode regexes and language tweaks.
- **Language files**: `init-lang-nix/rust/python/go` have their own files;
  the rest are grouped (`init-lang-web`, `-devops`, `-data`, `-systems`).
  Only split a language out once it grows enough (Go did, with its gopls
  workspace config, `go-tag`/`gotest` helpers, and dape debugging).

### Nix build layer

**Cache-parity invariant, the most important rule in the repo:** every
default in `emacs.nix`'s argument list must match upstream nixpkgs'
`make-emacs.nix` (and the explicit args emacs-overlay passes to its prebuilt
attrs). Then `import ./emacs.nix {}` yields the exact store path of the
prebuilt package, and every default-rev build is a binary-cache hit. The
distribution uses the `unstable` variant (`nix/mk-overlay.nix`), cached on
`nix-community.cachix.org` and the `jylhis` cachix; Hydra covers `mainline`.

- Variants: `unstable` (default), `git`, and `igc` come from
  nix-community/emacs-overlay; `mainline` is nixpkgs' `pkgs.emacs`, kept as
  the parity canary.
- **Build matrix**: two shipped builds on {x86_64, aarch64}
  Linux: pgtk/Wayland GUI and terminal-only. `emacs.nix` asserts every other toolkit away.
- With `withPgtk = true` (the Linux GUI default), `basePackage` selects the
  prebuilt `*-pgtk` sibling (`emacs-unstable-pgtk`, `emacs-git-pgtk`,
  `emacs-igc-pgtk`, `emacs-pgtk`). Overriding `withPgtk` on the non-pgtk base
  builds identical content under a different derivation name, so it misses
  the cache.
- Expected cache misses: custom `rev` pins, `useCcache = true`, and
  `cpuTune`.
- Flag trimming (mailutils/gpm/selinux) applies only to builds already off
  parity (the noGui builds), never to the Linux pgtk
  build. See the policy comment in `nix/mk-overlay.nix`.

Any edit to the `basePackage` / `basePackage.override { ... }` block must keep
parity for the `unstable` pgtk sibling and the noGui overrides. Verify with
`just cache-parity` (`nix/cache-parity.nix`, `nix-instantiate` over the tree's
nixpkgs and emacs-overlay pins); every value must be `true`.

**Older-nixpkgs portability.** The override args are filtered through
`lib.intersectAttrs (lib.functionArgs basePackage.override) overrideArgs`, so
a consumer that overrides `nixpkgs` with an older release (24.05+) still
evaluates: args an older `make-emacs.nix` lacks are dropped instead of
throwing. On the pinned nixpkgs the filter is a no-op, so parity holds. The
pgtk sibling lookups are `or`-guarded and fall back to a from-source
override. On 24.05 `pkgs.emacs` is Emacs 29, which the Elisp config does not
target.

**Overlay** (`nix/mk-overlay.nix`, parameterised on `variant`;
`overlay.nix` is a standalone wrapper that composes emacs-overlay underneath it):

- `emacsConfig` / `emacsConfigNoGui`: bare Emacs from `emacs.nix`
  (pgtk; the noGui twin is for nix-on-droid).
- `emacsConfigInfo`: the `emacs-config.info` manual (`nix/info-manual.nix`).
- `emacsConfigPackages` / `emacsConfigPackagesNoGui`: full distributions
  (use-package auto-mapping, all tree-sitter grammars). A
  `makeBinaryWrapper` re-wrap adds the runtime deps (`nix/runtime-deps.nix`)
  to `PATH`, `emacsConfigInfo` to `INFOPATH` (so `C-h i d m Emacs config RET` works),
  and a default `ASPELL_CONF` for the bundled en/fi/de/fr dictionaries.
- `eca`: prebuilt ECA server for `lisp/init-ai.el`.
- `likec4Lsp`: the `@likec4/lsp` server for `likec4-mode`, on every
  distribution's `PATH`.

**`default.nix`** selects `emacs-config.packages.<system>.default` from the
repo root `nix/inputs.nix`: `nix-build` builds the distribution. Variant builds
target `emacs.nix` directly.

**Outputs** (the `emacs-config` attrset in the root `nix/inputs.nix`):

- `packages.<system>`: `default` (`emacsConfigPackages`), `emacs`
  (`emacsConfig`), `emacs-nox` (`emacsConfigPackagesNoGui`), `docs` (options
  HTML), `packages-doc` (per-package reference), `ds-assets`, `info`.
- `overlays.default`; `homeManagerModules.default` (`module.nix`);
  `nixosModules.default` (`module-system.nix`).
- `formatter` (treefmt) and `checks` (`nix/checks.nix`).

Module outputs get the overlay composed over emacs-overlay
(`emacsConfigModuleOverlay`), so module installs resolve the same Emacs base
and epkgs snapshot that the checks build and cache.

`emacs-api-doc` (`nix/emacs-api-doc.nix`, forking the vendored
`etc/elisp-doc/`) runs a batch Emacs over the package closure and emits the
per-symbol HTML that `projects/services/emacs-docs/site.nix` mounts at `/help/api/`. It is heavy and
deploy-path-only: only the site and its own check import it, never
`nix/info-manual.nix` or `nix/mk-overlay.nix`, so editor builds stay cache
hits. The site check builds without it (`withApiDoc = false`). Its
`emacs-config-elisp-api.texi` fragment is produced but not yet included in the
Info manual.

### Dev shell

`shell.nix` provides tooling only: Nix tooling and linters (`nil`,
`nixfmt`, `statix`, `deadnix`), build tools that modes shell out to
(`meson`, `ninja`, `buildifier`), the language servers and CLIs the config
invokes (`sonarlint-ls`, `rass`, `eca`, `tagref`,
`dockerfile-language-server`), the docs chain (`pandoc`, `texinfo`), the
fonts `init-ui.el` probes, and `xvfb-run` on Linux. Cachix pulls `jylhis`
and `nix-community`.

`nix/extra-packages.nix` builds the archive-absent packages
(`jylhis-emacs-themes`, `claude-code-ide`, `combobulate`, `majutsu`,
`tagref`, …) with `trivialBuild`, plus overrides such as built-in shims and
an Elisp-only `ghostel`. `jylhis-emacs-themes` is built from the in-tree
design system (`projects/libs/design/nix/emacs.nix`), and the emacs-docs site copies its
CSS from the same tree (`nix/ds-assets.nix`, a wrapper over
`projects/libs/design/nix/web-assets.nix`), so editor and site never
diverge. The Emacs Lisp devenv module (`languages.emacs-lisp`, with ellsp)
lives in [devenv-extra](https://github.com/Jylhis/devenv-extra), its packages
in nixpkgs-extra.

### Modules

- **`module.nix`** (Home Manager, `services.emacsConfig`): runs the configuration as a user
  Emacs daemon with `emacsclient` (systemd) and
  deploys the byte-compiled config (`nix/config-compiled.nix`).
  `services.emacsConfig.package` swaps the distribution (default
  `emacsConfigPackages`).
  - Wrapper-`PATH` toggles (`<name>.enable`): `sonarlint` (pulls JDK 21),
    `devenv`, `dockerfileLsp`, `onePassword` (`op`, backs
    `auth-source-1password`), `sops`, `claudeCode`. `onePassword`, `sops`,
    and `claudeCode` are unfree. `spell.dictionaries` lists aspell
    dictionaries.
  - Secrets: `services.emacsConfig.environmentFile` is a runtime file loaded via
    systemd `EnvironmentFile`, inherited
    by every subprocess and never copied to the store.
    Alternatively, `services.emacsConfig.authSources` lists
    authinfo/netrc files that are passed as `EMACS_CONFIG_AUTH_SOURCES` and
    prepended to `auth-sources` by `lisp/init-systems.el`;
    `lisp/init-ai.el` exports the eca provider keys from auth-source before
    each eca session, since the eca server reads only the environment.
  - `services.emacsConfig.eca` generates `~/.config/eca/config.json` via
    `nix/eca-config.nix` (defaults from `config/eca/config.json`, which the
    `eca-models-in-sync` check also reads).
    `eca.openrouter.enable` adds the OpenRouter provider; `eca.settings` is
    deep-merged over it. gptel defaults to OpenRouter regardless.
- **`module-system.nix`** (NixOS module): applies the
  overlay and adds packages to `environment.systemPackages`, with the same
  toggles and `spell.dictionaries`. ECA config and `environmentFile` stay
  Home-Manager-only (no per-user daemon or `~/.config` here).
- **`module-nix-on-droid.nix`**: adds the terminal-only distribution
  (`emacsConfigPackagesNoGui`) and an `emacsclient` EDITOR wrapper to
  `environment.packages`, with `EDITOR`/`VISUAL` in
  `environment.sessionVariables`. Android under proot is headless, so a GUI
  build would only bloat the closure.

### Pinning

The tree's nixtamal pins (`nix/tamal/`, see the root `AGENTS.md`) are the
source of truth for nixpkgs and emacs-overlay; `just update` at the repo root
bumps them. Nothing in `etc/emacs` carries its
own `flake.lock`.

#### Hand-pinned upstreams

Not bumped by `just update`: the GitHub-built Emacs packages
(`nix/extra-packages.nix`), the ECA server (`nix/eca-server.nix`) and the
vendored npm language server (`nix/likec4-lsp.nix`). `scripts/update-pins.sh`
bumps them with [nix-update](https://github.com/Mic92/nix-update) (`--list`
prints the pin names). It and `scripts/verify-locks.sh` date from the
standalone flake: check them against the in-tree build before relying on them.
Two pins are manual by design: `ghostel` (a temporary Elisp-only rebuild of
emacs-overlay's epkg; revert it, don't bump) and `etc/elisp-doc` (vendored
verbatim from a Codeberg fork).

### Shared treefmt configuration

`nix/treefmt.nix` defines the formatters (`nixfmt`, `deadnix`, `statix`) for
both the root `nix/inputs.nix` (the `formatting` check and formatter) and `shell.nix`. Add
new formatters there.

### Check / test responsibility split

Checks (`nix/checks.nix`) cover the application and configuration:

- **Package builds**: `packages-default`, `packages-emacs`, `packages-info`.
- **Docs**: `options-doc`, `packages-doc`, `packages-doc-in-sync` (fix:
  `just docs-refresh-packages`), and `emacs-api-doc` (build-only).
- **In-sync gates**: `eca-models-in-sync`, which compares the OpenRouter models in
  `config/eca/config.json` with gptel's `:models` in `lisp/init-ai.el`
  (reconcile both by hand).
- **Module evaluation** without the host frameworks: `module-eval`,
  `nix-on-droid-module-eval`.
- **Nix linting**: `formatting`, `statix`, `deadnix`.
- **Elisp**: `elisp-lint` (balanced parens), `elisp-compile` (byte-compile,
  warnings are errors), `elisp-test` (ERT), and `config-startup`, which
  loads `early-init.el` + `init.el` against the real closure and fails on
  any `Error (use-package)` (the only check that evaluates `:config`
  blocks).
- **Scanner and package set**: `scanner-fidelity` compares
  `nix/use-package.nix`'s regex scanner against Emacs' reader over `lisp/`,
  so commented-out or string-embedded forms can't be miscounted;
  `emacs-packages-eval` (eval only) checks that every scanned and
  Nix-provided package resolves under `legacyPackages.emacs-packages`.
- **Binary smoke test**: `emacs-binaries` runs the built Emacs under a
  sandboxed HOME and checks for host-config leakage.

`elisp-compile` is the same `nix/config-compiled.nix` derivation that
`module.nix` deploys as `compiledConfig`, so on the default configuration
CI's artifact is what `home-manager switch` installs. The shell has no
test of its own: `checks.<system>.emacs-binaries` covers the binaries.

### Info manual

`docs/emacs-config.texi` is the master file. `nix/info-manual.nix` converts each
`docs/*.md(x)` page into a chapter with pandoc, `@include`s them, and runs
`makeinfo` + `install-info`. The options appendix comes from
`nix/options-doc.nix`, which emits both `index.html` and
`emacs-config-options.texi` from one source. The distribution wrapper appends
`${emacsConfigInfo}/share/info` to `INFOPATH` without a trailing `:`
(`makeBinaryWrapper` rejects empty segments); nixpkgs' Emacs `site-start.el`
adds the separator, so the built-in manuals stay visible. For a source
checkout, `lisp/init-docs.el` adds the first existing directory among
`EMACS_CONFIG_INFO_DIR`, `result-info/share/info`, and `result/share/info` to
`Info-additional-directory-list`.

## Coding Style & Naming Conventions

Follow the module conventions above. Global Elisp symbols use the `my-`
prefix (`my--` for internals); `lisp/devenv.el` uses `devenv-`. Nix
formatting is enforced by `nix/treefmt.nix`. For Emacs internals and Elisp
practice, use the `attic/skills/local/emacs/` references rather than memory.

## Testing Guidelines

Put ERT files in `test/`: the `elisp-test` check loads every `test/*.el`
(with `-L lisp`), so new files need no registration. Test module behaviour
or helper functions. Run `just test` before committing.

## Commit & Pull Request Guidelines

Follow the root `AGENTS.md` (Conventional Commits, scope `emacs`, a changie
fragment for project `etc/emacs`).

## Security & Configuration Tips

- Generated files: edit the source, then regenerate.
  `docs/configuration/package-reference.mdx` comes from `;;; @doc` blocks
  (`just docs-refresh-packages`).
- Never commit generated state: `elpa/`, `var/`, `result*`, `*.elc`.
