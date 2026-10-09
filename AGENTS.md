# AGENTS.md

Always-loaded context for AI coding agents (Claude Code, Pi, etc.) and the
human contributor guide. This file is the single source of truth for both.

## What this is

Jotain is a GNU Emacs 31 configuration (floor: Emacs 30.1, per `init.el`'s
`Package-Requires`) plus the Nix expressions that build Emacs itself. The
default build is emacs-overlay's `unstable` variant (the newest Emacs
release or pretest tag, currently 31.1); nixpkgs' Emacs 30 is the `mainline` variant. The dev shell has
tooling only, **no `emacs`**: build and launch with `just run-built`.

`journal/` and `TODO.md` are working notes and may be stale; when they
disagree with the code, the code wins. Finding numbers cited there refer to
the reports under `docs/reviews/`.

## Project Structure & Module Organization

- `early-init.el`, `init.el`, and `lisp/init-*.el` (feature modules,
  `require`d from `init.el` in order).
- Nix at the root: `flake.nix`, `emacs.nix`, `overlay.nix`, `default.nix`,
  `module.nix`, `module-system.nix`, `module-nix-on-droid.nix`. Helpers live
  in `nix/` (`mk-overlay.nix` is the overlay implementation, `checks.nix` the
  flake checks).
- `docs/` (documentation sources), `website/` (shell of the
  page.jylhis.com/jotain site), `bench/` (startup benchmarks), `test/` (ERT;
  every `test/*.el` is loaded by the test check), `etc/` (debug harness,
  vendored `elisp-doc`, lang-eval registry), `scripts/`.

## Development environment

Run recipes inside the devenv shell: `devenv shell`, or prefix a command,
e.g. `devenv shell -- just check`. No `.envrc` is tracked; direnv users keep
their own untracked one (`eval "$(devenv direnvrc)"` + `use devenv`).

The shell has Nix/lint/docs tooling and the language servers the config
shells out to, but no Emacs (its ~1 GB closure made `direnv allow` slow).
Paren/compile/test coverage comes from the `elisp-lint`, `elisp-compile`,
and `elisp-test` flake checks.

### Fresh container / no Nix (AI agent sessions)

A bare container has no `nix`, `just`, `devenv`, or `emacs`. Run
`scripts/bootstrap-agent-env.sh` as root (Debian/Ubuntu). It installs distro
`nix-bin` via apt, writes `/etc/nix/nix.conf` with flakes and the three
caches (`cache.nixos.org`, `nix-community.cachix.org`, `jylhis.cachix.org`),
installs current `nix` + `just` from nixpkgs, and pre-fetches every
`flake.lock` source from the caches by narHash. The tracked
`.claude/settings.json` runs it as a SessionStart hook in remote Claude Code
sessions; in the Claude Code cloud, putting it in the environment's setup
script is better (that result is cached).

If the egress proxy blocks github.com tarballs (403), only **flake-CLI**
builds work offline: `nix build .#default -o result`, or
`nix build --no-link .#checks.x86_64-linux.<name>`. Recipes built on
`nix-build` (`just build`, `just screenshot`, `just run-built`) go through
flake-compat's `builtins.fetchTree`, which re-downloads by URL; use the
flake-CLI equivalent and run the remaining steps against `./result` by hand.

Cost guide: `formatting`/`statix`/`deadnix`/`module-eval` take seconds;
`elisp-lint` needs the bare Emacs from the nix-community cache;
`elisp-compile`/`elisp-test` need the distribution closure. All three have
`lib.fileset`-narrowed inputs, so when a PR touches neither `lisp/` nor
`test/` they substitute from the jylhis cache without realising Emacs. Build
checks one at a time, as PR CI does.

### CI

All workflows run on x86_64-linux only (Darwin and aarch64 are not
exercised). Every `uses:` must be GitHub-owned or `cachix/*`: the repo
blocks other actions, and the run then fails at startup with no logs.

- **PR CI** (`ci.yml`, `pull_request` + `workflow_dispatch`). Job `check`:
  `just verify` (lock sync), then one named step per check: formatting,
  statix, deadnix, module-eval, packages-doc-in-sync, eca-models-in-sync,
  ds-in-sync, packages-doc, options-doc, elisp-lint, scanner-fidelity,
  elisp-compile, config-startup. Job `site`: builds `.#site-preview`. Job
  `test`: `devenv test`. Cachix is pull-only (no writer token for PR code).
  Superseded runs are cancelled. Pushes from the Claude Code GitHub App do
  not trigger `pull_request` runs; start CI with
  `gh workflow run ci.yml --ref <branch>`.
- **Full validation** (`deploy.yml`, push to `main`/`next` + dispatch): full
  `nix flake check` (including the heavy package builds, `elisp-test`, and
  `emacs-api-doc`) plus `devenv test`, pushing results to the jylhis cache.
  Runs are never cancelled, so cache pushes finish. On `main`, `build-pages`
  builds the full `.#site` and `deploy-pages` publishes it to GitHub Pages
  (Actions source) at **page.jylhis.com/jotain/** (`baseHref = "/jotain"` in
  `nix/site.nix`).
- **PR previews** (`preview.yml`; Pages hosts one deployment, so previews
  are artifacts): build the full `.#site` and upload it as
  the artifact `site-preview-pr-<N>` holding `site.tgz` (tarred because
  `upload-artifact` rejects the `:` in some `/help/api/` filenames). A bot
  comment links it; serve it under `/jotain/`, as `just serve-site` does.
  Same-repo PRs get the cachix writer token (so a cold `/help/api/` build
  warms the cache); fork PRs stay pull-only and get no comment.
- **Dependabot lock sync** (`sync-devenv.yml`, `dependabot/nix/**` branches
  only): Dependabot bumps `flake.lock` but not `devenv.yaml`/`devenv.lock`,
  so `just verify` would fail. This job runs `just sync-devenv` and pushes
  the fix with the `DEVENV_SYNC_TOKEN` PAT (a `GITHUB_TOKEN` push starts no
  new CI run). The secret must exist in both the Actions and the Dependabot
  secret stores: Dependabot-triggered runs can't read Actions secrets, and
  the follow-up `synchronize` run can't read Dependabot ones. The commit carries `[dependabot skip]` so Dependabot keeps
  rebasing.

A local `nix flake check` is heavier than PR CI and can fail on checks PR CI
never runs. PRs target `main`; `next` is a second full-validation branch.

## Common commands

Checks and maintenance:

- `just check`: `nix flake check` (every check in `nix/checks.nix`; not
  `devenv test`).
- `just test`: build the `elisp-test` check.
- `just fmt`: `nix fmt` (treefmt: nixfmt-rfc-style, deadnix, statix).
- `just update`: `nix flake update`, then `just sync-devenv all`.
- `just sync-devenv [shared|all]`: rewrite `devenv.yaml`'s shared-input URLs
  (nixpkgs, treefmt-nix, emacs-overlay) to the revs in `flake.lock` and
  re-lock `devenv.lock`. Never runs `nix flake update`, so it is safe on a
  Dependabot PR. `shared` (default) re-locks only the shared inputs; `all`
  also re-resolves the unpinned `devenv` input.
- `just verify`: assert both locks agree on every shared input
  (`scripts/verify-locks.sh`, the same script as the `locks-in-sync` check).
- `just update-pins [names]`: bump the hand-pinned upstreams (see Pinning).
- `just site` / `just serve-site`: build `.#site` to `result-site/` / serve it
  at `http://localhost:8080/jotain/`.
- `just ds-sync`: re-vendor `website/public/ds` from `nix/design-pin.nix`
  (the fix for a failing `ds-in-sync`).
- `just clean`: remove `*.elc`, autosaves, eln-cache, `result`.
  `just clean-all` also wipes `elpa/`, `var/`, `.dev-home/` (next startup is
  slow).

Build and launch (current system by default; override with
`just system=x86_64-linux …`):

- `just run-built [ARGS]`: **the way to launch this config**. Builds the full
  distribution (terminal-only on aarch64-linux) and runs
  `./result/bin/emacs --init-directory=<repo>`, isolated from `~/.emacs.d`.
- `just run-built-debug`: adds `--debug-init` and `debug-on-error`.
- `just run-built-debug-log`: every debug facility on; loads
  `etc/debug-init.el` and mirrors messages, warnings, backtraces, and stderr
  into `var/debug/<timestamp>/`. `M-x jotain-debug-dump-now` flushes mid-run.
- `just run-built-fast`: launch the AOT-compiled config (`.#config-compiled`)
  from `var/fast-home`. It reflects the last build, so use `run-built` while
  editing.
- `just build`: full distribution (`jotainEmacsPackages`: Emacs, packages,
  every tree-sitter grammar) via `nix-build`. `just build-nox-full`: the
  terminal-only distribution (`.#emacs-nox`).
- `just build-bare` / `build-nox` / `build-git` / `build-igc` /
  `build-igc-ccache` / `build-perf` / `build-android`: bare Emacs from
  `emacs.nix`. Default revisions are cache hits on Linux, except
  `build-igc-ccache` and `build-perf` (`cpuTune`: `-O3 -march/-mtune`),
  which build from source by design. A custom `--argstr rev` fails once and
  reports the hash to pass as `--argstr hash`.
- `just screenshot [out]`: headless PNG under Xvfb (Linux; needs `xvfb-run`
  from the shell; first run is slow).
- `just bench-built [output]`: startup benchmark via `bench/`; needs a
  display, so prefix `xvfb-run` when headless.
- `just lang-matrix` / `just lang-eval-live`: live language-support matrix /
  end-to-end LSP probe.
- `just docs` / `just info` / `just docs-all`: options HTML (`result-docs/`),
  `jotain.info` (`result-info/share/info/`), or both.
  `just build-api-doc`: the generated API reference (`result-api-doc/`).
- `just docs-refresh-packages`: regenerate
  `docs/configuration/package-reference.mdx` from the `;;; @doc` blocks in
  `lisp/`. Run it after editing any `@doc` block, or `packages-doc-in-sync`
  fails.
- `just docs-refresh-lang-matrix`: regenerate
  `docs/reference/language-support.mdx` from
  `etc/lang-eval/jotain-lang-registry.el` (edit the registry, not the page).

## Emacs & Elisp knowledge base and skills

Two skills in `.claude/skills/` hold source-cited references for Emacs 30/31
(index: `.claude/knowledge/emacs/README.md`). **Use them instead of answering
Emacs questions from memory.**

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
- **`lisp/devenv.el`** is the only non-`init-*` file in `lisp/`: a
  standalone package (`devenv-` namespace, no `jotain-` dependencies) wired
  in by `lisp/init-devenv.el`.
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
  the parity canary (not a flake output).
- **Build matrix**: four shipped builds on {x86_64, aarch64} ×
  {Linux, Darwin}: pgtk/Wayland GUI + terminal-only on Linux, patched NS GUI
  + terminal-only on Darwin. `emacs.nix` asserts every other toolkit away.
- With `withPgtk = true` (the Linux GUI default), `basePackage` selects the
  prebuilt `*-pgtk` sibling (`emacs-unstable-pgtk`, `emacs-git-pgtk`,
  `emacs-igc-pgtk`, `emacs-pgtk`). Overriding `withPgtk` on the non-pgtk base
  builds identical content under a different derivation name, so it misses
  the cache.
- Expected cache misses: custom `rev` pins, `useCcache = true`, `cpuTune`,
  and every Darwin GUI build (its nix-giant patches are on by default via
  `overrideAttrs`; CI is Linux-only).
- Flag trimming (mailutils/gpm/selinux) applies only to builds already off
  parity (both noGui builds and the Darwin GUI), never to the Linux pgtk
  build. See the policy comment in `nix/mk-overlay.nix`.

Any edit to the `basePackage` / `basePackage.override { … }` block must keep
parity for the `unstable` pgtk sibling and the noGui overrides. Verify:

```
nix-instantiate --eval --strict -E '
  let lock = builtins.fromJSON (builtins.readFile ./flake.lock);
      n  = lock.nodes.${lock.nodes.root.inputs.nixpkgs}.locked;
      ov = lock.nodes.${lock.nodes.root.inputs.emacs-overlay}.locked;
      nixpkgs = fetchTarball { url = "https://github.com/${n.owner}/${n.repo}/archive/${n.rev}.tar.gz"; sha256 = n.narHash; };
      overlay = fetchTarball { url = "https://github.com/${ov.owner}/${ov.repo}/archive/${ov.rev}.tar.gz"; sha256 = ov.narHash; };
      pkgs = import nixpkgs { overlays = [ (import overlay) (import ./overlay.nix) ]; };
  in {
    # x86_64-linux: every GUI build maps to its *-pgtk sibling.
    default       = pkgs.jotainEmacs.outPath == pkgs.emacs-unstable-pgtk.outPath;
    bare-default  = (import ./emacs.nix {}).outPath == pkgs.emacs-unstable-pgtk.outPath;
    mainline-pgtk = (import ./emacs.nix { variant = "mainline"; }).outPath == pkgs.emacs-pgtk.outPath;
    git-pgtk      = (import ./emacs.nix { variant = "git"; }).outPath == pkgs.emacs-git-pgtk.outPath;
    igc-pgtk      = (import ./emacs.nix { variant = "igc"; }).outPath == pkgs.emacs-igc-pgtk.outPath;
  }'
```

**Older-nixpkgs portability.** The override args are filtered through
`lib.intersectAttrs (lib.functionArgs basePackage.override) overrideArgs`, so
a consumer that overrides `nixpkgs` with an older release (24.05+) still
evaluates: args an older `make-emacs.nix` lacks are dropped instead of
throwing. On the pinned nixpkgs the filter is a no-op, so parity holds. The
pgtk sibling lookups are `or`-guarded and fall back to a from-source
override. On 24.05 `pkgs.emacs` is Emacs 29, which the Elisp config does not
target.

**Overlay** (`nix/mk-overlay.nix`, parameterised on `variant`;
`overlay.nix` is a standalone wrapper that composes emacs-overlay, pinned
from `flake.lock`, underneath it):

- `jotainEmacs` / `jotainEmacsNoGui`: bare Emacs from `emacs.nix`
  (pgtk on Linux, NS on Darwin; the noGui twin is for nix-on-droid).
- `jotainInfo`: the `jotain.info` manual (`nix/info-manual.nix`).
- `jotainEmacsPackages` / `jotainEmacsPackagesNoGui`: full distributions
  (use-package auto-mapping, all tree-sitter grammars). A
  `makeBinaryWrapper` re-wrap adds the runtime deps (`nix/runtime-deps.nix`)
  to `PATH`, `jotainInfo` to `INFOPATH` (so `C-h i d m Jotain RET` works),
  and a default `ASPELL_CONF` for the bundled en/fi/de/fr dictionaries.
- `eca`: prebuilt ECA server for `lisp/init-ai.el`.
- `likec4Lsp`: the `@likec4/lsp` server for `likec4-mode`, on every
  distribution's `PATH`.

**`default.nix`** wraps the flake via flake-compat (rev from `flake.lock`):
`nix-build` builds the distribution, `nix-build -A emacs` bare Emacs.
Variant builds target `emacs.nix` directly.

**`flake.nix`** is wiring only. Outputs:

- `packages.<system>`: `default` (`jotainEmacsPackages`), `emacs`
  (`jotainEmacs`), `emacs-nox` (`jotainEmacsPackagesNoGui`), `likec4-lsp`,
  `info`, `docs` (options HTML), `packages-doc` (per-package reference),
  `emacs-api-doc`, `site`, `site-preview`, `ds-assets`.
- `legacyPackages.<system>` (not built by `nix flake check`):
  `emacs-packages.<name>` (every bundled package), `config-compiled` (for
  `run-built-fast`), `lang-eval-doc` / `lang-eval-matrix` / `lang-eval-live`.
- `overlays.default`; `homeManagerModules.default` (`module.nix`);
  `nixosModules.default` and `darwinModules.default` (both
  `module-system.nix`); `nixOnDroidModules.default`
  (`module-nix-on-droid.nix`); an example `nixOnDroidConfigurations.default`
  (aarch64, not built by CI).
- `formatter` (treefmt), `lib` (use-package scanner, `nix/use-package.nix`),
  `checks` (`nix/checks.nix`).

All module outputs get the jotain overlay composed over emacs-overlay
(`moduleOverlay`), so module installs resolve the same Emacs base and epkgs
snapshot that CI builds and caches.

`emacs-api-doc` (`nix/emacs-api-doc.nix`, forking the vendored
`etc/elisp-doc/`) runs a batch Emacs over the package closure and emits the
per-symbol HTML that `nix/site.nix` mounts at `/help/api/`. It is heavy and
deploy-path-only: only `nix/site.nix` and its own check import it, never
`nix/info-manual.nix` or `nix/mk-overlay.nix`, so editor builds stay cache
hits. PR CI builds `.#site-preview` (`withApiDoc = false`) instead. Its
`jotain-elisp-api.texi` fragment is produced but not yet included in the
Info manual.

### Dev shell

`devenv.nix` provides tooling only: Nix tooling and linters (`nil`,
`nixfmt`, `statix`, `deadnix`), build tools that modes shell out to
(`meson`, `ninja`, `buildifier`), the language servers and CLIs the config
invokes (`sonarlint-ls`, `rass`, `eca`, `tagref`,
`dockerfile-language-server`), the docs chain (`pandoc`, `texinfo`), the
fonts `init-ui.el` probes, and `xvfb-run` on Linux. Cachix pulls `jylhis`
and `nix-community`.

`nix/extra-packages.nix` builds the archive-absent packages
(`jylhis-emacs-themes`, `claude-code-ide`, `combobulate`, `majutsu`,
`tagref`, …) with `trivialBuild`, plus overrides such as built-in shims and
an Elisp-only `ghostel`. `jylhis-emacs-themes` takes its revision from
`nix/design-pin.nix`, the single pin for github.com/Jylhis/design, shared
with the CSS vendored in `website/public/ds` so editor and site never
diverge. `nix/devenv-emacs-lisp.nix` is no longer imported by the shell; it
remains only as an options-doc source.

### Modules

- **`module.nix`** (Home Manager, `services.jotain`): runs Jotain as a user
  Emacs daemon with `emacsclient` (systemd on Linux, launchd on macOS) and
  deploys the byte-compiled config (`nix/config-compiled.nix`).
  `services.jotain.package` swaps the distribution (default
  `jotainEmacsPackages`).
  - Wrapper-`PATH` toggles (`<name>.enable`): `sonarlint` (pulls JDK 21),
    `devenv`, `dockerfileLsp`, `onePassword` (`op`, backs
    `auth-source-1password`), `sops`, `claudeCode`. `onePassword`, `sops`,
    and `claudeCode` are unfree. `spell.dictionaries` lists aspell
    dictionaries.
  - Secrets: `services.jotain.environmentFile` is a runtime file loaded via
    systemd `EnvironmentFile` (sourced before exec under launchd), inherited
    by every subprocess and never copied to the store (`eca.environmentFile`
    is an alias). Alternatively, `services.jotain.authSources` lists
    authinfo/netrc files that are passed as `JOTAIN_AUTH_SOURCES` and
    prepended to `auth-sources` by `lisp/init-systems.el`;
    `lisp/init-ai.el` exports the eca provider keys from auth-source before
    each eca session, since the eca server reads only the environment.
  - `services.jotain.eca` generates `~/.config/eca/config.json` via
    `nix/eca-config.nix` (defaults from `config/eca/config.json`, which the
    `eca-models-in-sync` check also reads).
    `eca.openrouter.enable` adds the OpenRouter provider (old alias:
    `services.jotain.openrouter.enable`); `eca.settings` is deep-merged over
    it. gptel defaults to OpenRouter regardless.
- **`module-system.nix`** (shared NixOS / nix-darwin module): applies the
  overlay and adds packages to `environment.systemPackages`, with the same
  toggles and `spell.dictionaries`. ECA config and `environmentFile` stay
  Home-Manager-only (no per-user daemon or `~/.config` here).
- **`module-nix-on-droid.nix`**: adds the terminal-only distribution
  (`jotainEmacsPackagesNoGui`) and an `emacsclient` EDITOR wrapper to
  `environment.packages`, with `EDITOR`/`VISUAL` in
  `environment.sessionVariables`. Android under proot is headless, so a GUI
  build would only bloat the closure.

### Pinning

`flake.lock` is the source of truth for input revisions. `devenv.yaml` pins
the shared inputs (`nixpkgs`, `treefmt-nix`, `emacs-overlay`) to the same
commits so the shell hits the same caches, and `devenv.lock` must agree. Use
`just update` to bump, `just verify` to check, and `just sync-devenv` when
`flake.lock` moved on its own (`sync-devenv.yml` does this for Dependabot
PRs). Drift fails the `locks-in-sync` check and PR CI. `default.nix`,
`emacs.nix`, and `overlay.nix` read their pins from `flake.lock` via
`fetchTarball`, resolving nodes through the root input map.

#### Hand-pinned upstreams (not in `flake.lock`)

Not bumped by `just update` or Dependabot: the GitHub-built Emacs packages
(`nix/extra-packages.nix`), the ECA server (`nix/eca-server.nix`), the
vendored npm language servers (`nix/likec4-lsp.nix`, `nix/ellsp.nix`), and
the design pin (`nix/design-pin.nix`). `just update-pins` bumps them via
`scripts/update-pins.sh`: [nix-update](https://github.com/Mic92/nix-update)
for the plain `fetchFromGitHub` packages (tagged repos follow the newest
tag, untagged ones the branch HEAD), and bespoke steps for eca (four-platform
hash table), the two npm wrappers (regenerated `package-lock.json`), and the
design pin (which also re-runs `just ds-sync`). Scope it with pin names
(`just update-pins combobulate eca`); `--list` prints them. Two pins are
manual by design: `ghostel` (a temporary Elisp-only rebuild of
emacs-overlay's epkg; revert it, don't bump)
and `etc/elisp-doc` (vendored verbatim from a Codeberg fork).

### Shared treefmt configuration

`nix/treefmt.nix` defines the formatters (`nixfmt`, `deadnix`, `statix`) for
both `flake.nix` (`nix fmt`, the `formatting` check) and `devenv.nix`. Add
new formatters there.

### Check / test responsibility split

Flake checks (`nix/checks.nix`) cover the application and configuration:

- **Package builds**: `packages-default`, `packages-emacs`, `packages-info`.
- **Docs**: `options-doc`, `packages-doc`, `packages-doc-in-sync` (fix:
  `just docs-refresh-packages`), and `emacs-api-doc` (build-only, deploy
  path only).
- **In-sync gates**: `ds-in-sync` (fix: `just ds-sync`);
  `lang-eval-doc-in-sync` (fix: `just docs-refresh-lang-matrix`; not in PR
  CI); `eca-models-in-sync`, which compares the OpenRouter models in
  `config/eca/config.json` with gptel's `:models` in `lisp/init-ai.el`
  (reconcile both by hand).
- **Locks**: `locks-in-sync` (fix: `just sync-devenv`).
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
CI's artifact is what `home-manager switch` installs. `devenv test`
(`enterTest` in `devenv.nix`) only checks that the shell tooling is on
`PATH` and lives in the Nix store.

### Info manual

`docs/jotain.texi` is the master file. `nix/info-manual.nix` converts each
`docs/*.md(x)` page into a chapter with pandoc, `@include`s them, and runs
`makeinfo` + `install-info`. The options appendix comes from
`nix/options-doc.nix`, which emits both `index.html` (Pages) and
`jotain-options.texi` from one source. The distribution wrapper appends
`${jotainInfo}/share/info` to `INFOPATH` without a trailing `:`
(`makeBinaryWrapper` rejects empty segments); nixpkgs' Emacs `site-start.el`
adds the separator, so the built-in manuals stay visible. For a source
checkout, `lisp/init-docs.el` adds the first existing directory among
`JOTAIN_INFO_DIR`, `result-info/share/info`, and `result/share/info` to
`Info-additional-directory-list`.

## Coding Style & Naming Conventions

Follow the module conventions above. Global Elisp symbols use the `jotain-`
prefix (`jotain--` for internals); `lisp/devenv.el` uses `devenv-`. Nix
formatting is enforced by `nix/treefmt.nix`. For Emacs internals and Elisp
practice, use the `.claude/skills/` skills rather than memory.

## Testing Guidelines

Put ERT files in `test/`: the `elisp-test` check loads every `test/*.el`
(with `-L lisp`), so new files need no registration. Test module behaviour
or helper functions. Run `just test` for ERT alone and `just check` before
opening a PR.

## Commit & Pull Request Guidelines

Commits use `scope: subject` with a short imperative subject, e.g.
`elisp: default to the unstable variant`, `ci: gate lock-file sync`. Common
scopes: `docs`, `elisp`, `nix`, `ci`, `site`, `ui`, `completion`, `themes`,
`build(deps)`. PRs include a summary, issue links, check results, and
screenshots only for UI-facing changes.

## Security & Configuration Tips

- Keep the locks aligned (see Pinning).
- Generated files: edit the source, then regenerate.
  `docs/configuration/package-reference.mdx` comes from `;;; @doc` blocks
  (`just docs-refresh-packages`); `docs/reference/language-support.mdx` from
  `etc/lang-eval/jotain-lang-registry.el` (`just docs-refresh-lang-matrix`);
  `website/public/ds` from `nix/design-pin.nix` (`just ds-sync`).
- Never commit generated state: `elpa/`, `var/`, `result*`, `*.elc`.
