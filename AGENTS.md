# AGENTS.md

Always-loaded context for AI coding agents (Claude Code, Pi, etc.) and the
human contributor guide. This file is the single source of truth for both.

## What this is

Jotain is a GNU Emacs 31 configuration (floor: Emacs 31.1, per `init.el`'s
`Package-Requires`) plus the Nix expressions that build Emacs itself. There
is one system (`x86_64-linux`) and one build, the pgtk GUI. The default
build is emacs-overlay's `unstable` variant (the newest Emacs release or
pretest tag, currently 31.1). The dev shell has tooling only, **no
`emacs`**: build and launch with `just run-built`.

Finding numbers cited in comments refer to the reports under
`docs/reviews/`.

## Project Structure & Module Organization

- `early-init.el`, `init.el`, and `lisp/init-*.el` (feature modules,
  `require`d from `init.el` in order).
- Nix at the root: `flake.nix`, `emacs.nix`, `overlay.nix`, `default.nix`,
  `shell.nix`, `module.nix`, `module-system.nix`. Helpers live in `nix/`
  (`mk-overlay.nix` is the overlay implementation, `checks.nix` the flake
  checks, `dev-shell.nix` the dev shell).
- `docs/` (documentation sources: `.mdx` pages plus the `.7` man pages),
  `jotain.7` (the top-level man page), `release.json` (version, repo and
  support address for the landing page), `website/` (shell of the
  page.jylhis.com/jotain site), `bench/` (startup benchmarks), `test/` (ERT;
  every `test/*.el` is loaded by the test check), `etc/` (debug harness,
  vendored `elisp-doc`, lang-eval registry), `scripts/`, `templates/`.
- `.changes/` (changie fragments, `unreleased/` holds the pending ones),
  `.changie.yaml`, `CHANGELOG.md`, `NEWS`; `.convco`,
  `.pre-commit-config.yaml`, `.gitleaks.toml` and `.editorconfig` for the
  commit workflow.

## Development environment

Run recipes inside the dev shell: `nix develop`, or prefix a command, e.g.
`nix develop -c just check`. `nix-shell` works too (`shell.nix` returns the
flake's dev shell). No `.envrc` is tracked; direnv users keep their own
untracked one (`use flake`).

The shell (`nix/dev-shell.nix`) has Nix/lint/docs tooling, the language
servers the config shells out to, `just`, `changie`, `convco`, `prek`,
`gitleaks`, `mandoc` and `xvfb-run`, but no Emacs (its ~1 GB closure would
make entering the shell slow). Paren/compile/test coverage comes from the
`elisp-lint`, `elisp-compile` and `elisp-test` flake checks.

Recipes built on `nix-build` (`just build`, `just run-built`,
`just screenshot`) go through `default.nix`, which loads the flake via
flake-compat. flake-compat fetches inputs by URL, so where only the flake
CLI works (for example behind a proxy that blocks github.com tarballs), use
`nix build .#default -o result` and run the remaining steps against
`./result` by hand. Checks build with
`nix build --no-link .#checks.x86_64-linux.<name>`.

Cost guide: `formatting`/`statix`/`deadnix`/`module-eval` take seconds;
`elisp-lint` needs the bare Emacs from the nix-community cache;
`elisp-compile`/`elisp-test` need the distribution closure. All three have
`lib.fileset`-narrowed inputs, so when a PR touches neither `lisp/` nor
`test/` they substitute from the jylhis cache without realising Emacs. Build
checks one at a time, as PR CI does.

### CI

All workflows run on x86_64-linux only. Every `uses:` must be GitHub-owned
or `cachix/*` (the repo blocks other actions, and the run then fails at
startup with no logs), and is pinned by commit SHA with the version in a
trailing comment.

- **PR CI** (`ci.yml`, `pull_request` + `workflow_dispatch`). Job `check`:
  one named step per check: formatting, statix, deadnix, dev-shell,
  module-eval, cache-parity, packages-doc-in-sync, eca-models-in-sync,
  ds-in-sync, packages-doc, options-doc, elisp-lint, scanner-fidelity,
  elisp-compile, config-startup, elisp-test. Job `site`: builds
  `.#site-preview`. Cachix is pull-only (no writer token for PR code).
  Superseded runs are cancelled. Pushes from the Claude Code GitHub App do
  not trigger `pull_request` runs; start CI with
  `gh workflow run ci.yml --ref <branch>`.
- **Full validation** (`deploy.yml`, push to `main`/`next` + dispatch): full
  `nix flake check` (including the heavy package builds, `elisp-test`, and
  `emacs-api-doc`), pushing results to the jylhis cache. Runs are never
  cancelled, so cache pushes finish. On `main`, `build-pages` builds the
  full `.#site` and `deploy-pages` publishes it to GitHub Pages (Actions
  source) at **page.jylhis.com/jotain/** (`baseHref = "/jotain"` in
  `nix/site.nix`).
- **PR previews** (`preview.yml`; Pages hosts one deployment, so previews
  are artifacts): build `.#site-preview` (the site without the heavy
  `/help/api/` reference) and upload it as the artifact
  `site-preview-pr-<N>` holding `site.tgz` (tarred because
  `upload-artifact` rejects the `:` in some `/help/api/` filenames). Serve
  it under `/jotain/`, as `just serve-site` does. Same-repo PRs get the
  cachix writer token (so a cold build warms the cache); fork PRs stay
  pull-only.

A local `nix flake check` is heavier than PR CI and can fail on checks PR CI
never runs. PRs target `main`; `next` is a second full-validation branch.

## Common commands

Checks and maintenance:

- `just check`: `nix flake check` (every check in `nix/checks.nix`; this
  includes the heavy `emacs-api-doc`).
- `just test`: build the `elisp-test` check.
- `just cache-parity`: build the `cache-parity` check (see the invariant
  under Nix build layer).
- `just fmt`: `nix fmt` (treefmt: nixfmt, deadnix, statix).
- `just update`: `nix flake update`.
- `just update-pins [names]`: bump the hand-pinned upstreams (see Pinning).
- `just ds-sync`: re-vendor `website/public/ds` from `nix/design-pin.nix`
  (the fix for a failing `ds-in-sync`).
- `just site` / `just serve-site`: build `.#site` to `result-site/` / serve it
  at `http://localhost:8080/jotain/`.
- `just clean`: remove `*.elc`, autosaves, `var/eln-cache`, `result*`.
  `just clean-all` also wipes `elpa/` and `var/` (next startup is
  slow).

Build and launch (`system` defaults to `x86_64-linux`):

- `just run-built [ARGS]`: **the way to launch this config**. Builds the full
  distribution and runs `./result/bin/emacs --init-directory=<repo>`,
  isolated from `~/.emacs.d`.
- `just run-built-debug`: adds `--debug-init` and `debug-on-error`.
- `just run-built-debug-log`: every debug facility on; loads
  `etc/debug-init.el` and mirrors messages, warnings, backtraces, and stderr
  into `var/debug/<timestamp>/`. `M-x jotain-debug-dump-now` flushes mid-run.
- `just run-built-fast`: launch the AOT-compiled config (`.#config-compiled`)
  from `var/fast-home`. It reflects the last build, so use `run-built` while
  editing.
- `just build`: full distribution (`jotainEmacsPackages`: Emacs, packages,
  every tree-sitter grammar) via `nix-build`.
- `just build-bare` / `build-git` / `build-igc` / `build-igc-ccache` /
  `build-perf`: bare Emacs from `emacs.nix`. Default revisions are cache
  hits, except `build-igc-ccache` and `build-perf` (`cpuTune`: `-O3
  -march/-mtune`), which build from source by design. A custom `--argstr
  rev` fails once and reports the hash to pass as `--argstr hash`.
- `just screenshot [out]`: headless PNG under Xvfb (needs `xvfb-run` from
  the shell; first run is slow).
- `just bench-built [output]`: startup benchmark via `bench/`; needs a
  display, so prefix `xvfb-run` when headless.
- `just lang-matrix` / `just lang-eval-live`: live language-support matrix /
  end-to-end LSP probe.
- `just docs` / `just info` / `just docs-all`: options HTML (`result-docs/`),
  `jotain.info` (`result-info/share/info/`), or both.
  `just build-packages-doc` builds the per-package reference;
  `just build-api-doc` the generated API reference (`result-api-doc/`).
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
   scroll bars before the first frame, and redirects the native-comp
   eln-cache to `var/eln-cache/`. It also silences the false-positive
   `(package reinitialization)` warning and pins `package-quickstart-file`
   to `var/package-quickstart.el`, the only place `startup.el`'s automatic
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
- **`lisp/devenv.el`** and **`lisp/repo-review.el`** are the two
  non-`init-*` files in `lisp/`: standalone packages (own `devenv-` /
  `repo-review-` namespaces, no `jotain-` dependencies) wired in by
  `lisp/init-devenv.el` and `lisp/init-review.el`. `devenv.el` integrates
  the devenv.sh tool into Emacs for projects that use it; this repository
  itself no longer uses devenv.
- **No version guards.** The floor and the default build are both 31.1, so
  use 31 APIs directly; `init.el` ends by enabling Emacs 31's
  `newcomers-presets` theme (`jotain-ui-enable-newcomers-presets`), which
  supplies savehist, recentf, which-key, electric pairs and similar
  defaults. Don't restate a setting the theme already provides.
- **LSP and formatters live in `init-prog.el`.** Per-language `eglot-ensure`
  hooks and `apheleia` formatters are centralised there; `init-lang-*.el`
  holds only mode regexes and language tweaks.
- **Language files**: `init-lang-nix/rust/python/go/kotlin/qml` have their own
  files; the rest are grouped (`init-lang-web`, `-devops`, `-data`,
  `-systems`). Only split a language out once it grows enough (Go did, with
  its gopls workspace config, `go-tag`/`gotest` helpers, and dape
  debugging). `likec4-mode` is defined in-tree as `jotain-likec4-mode`
  (`init-lang-devops.el`).
- **Linux only.** The config has no macOS branches.

### Nix build layer

**Cache-parity invariant, the most important rule in the repo:** every
default in `emacs.nix`'s argument list must match upstream nixpkgs'
`make-emacs.nix` (and the explicit args emacs-overlay passes to its prebuilt
attrs). Then `import ./emacs.nix {}` yields the exact store path of the
prebuilt package, and every default-rev build is a binary-cache hit. The
distribution uses the `unstable` variant (`nix/mk-overlay.nix`), cached on
`nix-community.cachix.org` and the `jylhis` cachix.

- Variants: `unstable` (default), `git`, and `igc` come from
  nix-community/emacs-overlay; `mainline` is nixpkgs' `pkgs.emacs`, kept as
  the parity canary (it is Emacs 30, below the Elisp floor, and not a flake
  output).
- **One build**: the pgtk (GTK / Wayland) GUI on `x86_64-linux`. `emacs.nix`
  asserts every other toolkit away. `basePackage` selects the prebuilt
  `*-pgtk` sibling (`emacs-unstable-pgtk`, `emacs-git-pgtk`,
  `emacs-igc-pgtk`, `emacs-pgtk`). Overriding `withPgtk` on the non-pgtk
  base builds identical content under a different derivation name, so it
  misses the cache. The pgtk build keeps upstream's feature flags; do not
  trim them.
- Expected cache misses: custom `rev` pins, `useCcache = true`, and
  `cpuTune`.

Any edit to the `basePackage` / `basePackage.override { … }` block must keep
parity for the `unstable` pgtk sibling. Verify:

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

- `jotainEmacs`: bare Emacs from `emacs.nix` (pgtk).
- `jotainInfo`: the `jotain.info` manual (`nix/info-manual.nix`).
- `jotainEmacsPackages`: the full distribution (use-package auto-mapping,
  all tree-sitter grammars). A `makeBinaryWrapper` re-wrap adds the runtime
  deps (`nix/runtime-deps.nix`, including `delta` for magit-delta and
  `mergiraf` for `smerge-mode`) to `PATH`, `jotainInfo` to `INFOPATH` (so
  `C-h i d m Jotain RET` works), and a default `ASPELL_CONF` for the
  bundled en/fi/de/fr dictionaries (`nix/spell-conf.nix`,
  `nix/wrap-runtime-deps.nix`).
- `eca`: prebuilt ECA server for `lisp/init-ai.el`.
- `likec4Lsp`: the `@likec4/lsp` server for `jotain-likec4-mode`, on the
  distribution's `PATH`.

**`default.nix`** wraps the flake via flake-compat (rev from `flake.lock`):
`nix-build` builds the distribution, `nix-build -A emacs` bare Emacs.
Variant builds target `emacs.nix` directly. `shell.nix` exposes the flake's
dev shell for `nix-shell`.

**`flake.nix`** is wiring only, for the single system `x86_64-linux`.
Outputs:

- `packages.<system>`: `default` (`jotainEmacsPackages`), `emacs`
  (`jotainEmacs`), `likec4-lsp`, `info`, `docs` (options HTML),
  `packages-doc` (per-package reference), `emacs-api-doc`, `site`,
  `site-preview`, `ds-assets`.
- `legacyPackages.<system>` (not built by `nix flake check`):
  `emacs-packages.<name>` (every bundled package), `config-compiled` (for
  `run-built-fast`), `lang-eval-doc` / `lang-eval-matrix` / `lang-eval-live`.
- `devShells.<system>.default` (`nix/dev-shell.nix`).
- `overlays.default`; `homeManagerModules.default` (`module.nix`);
  `nixosModules.default` (`module-system.nix`).
- `formatter` (treefmt), `lib` (use-package scanner, `nix/use-package.nix`),
  `checks` (`nix/checks.nix`).

Both module outputs get the jotain overlay composed over emacs-overlay
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

`nix/dev-shell.nix` provides tooling only (`nix develop`, `nix-shell`):
Nix tooling and linters (`nil`, `nixfmt`, `statix`, `deadnix`, `treefmt`),
build tools that modes shell out to (`meson`, `ninja`, `buildifier`), the
language servers and CLIs the config invokes (`sonarlint-ls`, `rass`, `eca`,
`tagref`, `dockerfile-language-server`), the docs chain (`pandoc`,
`texinfo`, `mandoc`), the repository workflow tools (`just`, `jq`,
`changie`, `convco`, `prek`, `gitleaks`), the fonts `init-ui.el` probes, and
`xvfb-run`. The `dev-shell` check builds it.

`nix/extra-packages.nix` builds the archive-absent packages
(`jylhis-emacs-themes`, `claude-code-ide`, `combobulate`, `majutsu`,
`tagref`, …) with `trivialBuild`, plus overrides such as built-in shims and
an Elisp-only `ghostel`. `jylhis-emacs-themes` takes its revision from
`nix/design-pin.nix`, the single pin for github.com/Jylhis/design, shared
with the CSS vendored in `website/public/ds` so editor and site never
diverge.

### Modules

- **`module.nix`** (Home Manager, `services.jotain`): runs Jotain as a user
  Emacs daemon with `emacsclient` under systemd and deploys the
  byte-compiled config (`nix/config-compiled.nix`).
  `services.jotain.package` swaps the distribution (default
  `jotainEmacsPackages`).
  - Wrapper-`PATH` toggles (`<name>.enable`): `sonarlint` (pulls JDK 21),
    `dockerfileLsp`, `onePassword` (`op`, backs `auth-source-1password`),
    `sops`, `claudeCode`. `onePassword`, `sops`, and `claudeCode` are
    unfree.
  - `services.jotain.spell.dictionaries`: aspell dictionary packages for
    jinx. A non-empty list replaces the bundled en/fi/de/fr set; the
    wrapped Emacs gets an `ASPELL_CONF` that points at an `aspellWithDicts`
    built from them (`nix/spell-conf.nix`). The empty default keeps the
    bundled set. `module-eval` checks both cases.
  - Secrets: `services.jotain.environmentFile` is a runtime file loaded via
    systemd `EnvironmentFile`, inherited by every subprocess and never
    copied to the store. Alternatively, `services.jotain.authSources` lists
    authinfo/netrc files that are passed as `JOTAIN_AUTH_SOURCES` and
    prepended to `auth-sources` by `lisp/init-systems.el`;
    `lisp/init-ai.el` exports the eca provider keys from auth-source before
    each eca session, since the eca server reads only the environment.
  - `services.jotain.eca` generates `~/.config/eca/config.json` via
    `nix/eca-config.nix` (defaults from `config/eca/config.json`, which the
    `eca-models-in-sync` check also reads).
    `eca.openrouter.enable` adds the OpenRouter provider; `eca.settings` is
    deep-merged over it. gptel defaults to Claude on the direct Anthropic
    API, with OpenRouter, Gemini and Ollama as further backends.
- **`module-system.nix`** (NixOS): applies the overlay and adds packages to
  `environment.systemPackages`, with the same toggles. ECA config and
  `environmentFile` stay Home-Manager-only (no per-user daemon or
  `~/.config` here). `services.jotain.users.<name>` builds the programs of
  one user (the wrapped `emacs`, EDITOR and VISUAL scripts, `jotctl`, the
  dictionaries; `nix/user-programs.nix`) and installs them in that user's
  profile. The Home Manager module of the same user then takes them from
  `osConfig.services.jotain.users.<name>.built`, so both modules produce the
  same store paths.

### Pinning

`flake.lock` is the single source of truth for input revisions. Use
`just update` to bump it. `default.nix`, `emacs.nix`, and `overlay.nix` read
their pins from `flake.lock` via `fetchTarball`, resolving nodes through the
root input map.

#### Hand-pinned upstreams (not in `flake.lock`)

Not bumped by `just update` or Dependabot: the GitHub-built Emacs packages
(`nix/extra-packages.nix`), the ECA server (`nix/eca-server.nix`), the
vendored npm language server (`nix/likec4-lsp.nix`), and the design pin
(`nix/design-pin.nix`). `just update-pins` bumps them via
`scripts/update-pins.sh`: [nix-update](https://github.com/Mic92/nix-update)
for the plain `fetchFromGitHub` packages (tagged repos follow the newest
tag, untagged ones the branch HEAD), and bespoke steps for eca (the
x86_64-linux asset hash), the npm wrapper (regenerated `package-lock.json`),
and the design pin (which also re-runs `just ds-sync`). Scope it with pin
names (`just update-pins combobulate eca`); `--list` prints them. Two pins
are manual by design: `ghostel` (a temporary Elisp-only rebuild of
emacs-overlay's epkg; revert it, don't bump) and `etc/elisp-doc` (vendored
verbatim from a Codeberg fork).

### Shared treefmt configuration

`nix/treefmt.nix` defines the formatters (`nixfmt`, `deadnix`, `statix`) for
both `flake.nix` (`nix fmt`, the `formatting` check) and the dev shell. Add
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
  `config/eca/config.json` with gptel's OpenRouter `:models` in
  `lisp/init-ai.el` (reconcile both by hand).
- **Module evaluation** without the host framework: `module-eval` (also
  covers `spell.dictionaries`).
- **Cache parity**: `cache-parity` (eval-only) asserts the invariant above
  via `nix/cache-parity.nix`; `just cache-parity` builds it.
- **Dev shell**: `dev-shell` builds `nix/dev-shell.nix`.
- **Nix linting**: `formatting`, `statix`, `deadnix`.
- **Elisp**: `elisp-lint` (balanced parens over all of `lisp/`),
  `elisp-compile` (byte-compile, warnings are errors), `elisp-test` (ERT; it
  runs through `ert-junit` and writes a JUnit report, `junit.xml`, next to
  the log, available as `.#checks.x86_64-linux.elisp-test.report`), and
  `config-startup`, which loads `early-init.el` + `init.el` against the real
  closure and fails on any `Error (use-package)` (the only check that
  evaluates `:config` blocks).
- **Scanner and package set**: `scanner-fidelity` compares
  `nix/use-package.nix`'s regex scanner against Emacs' reader over `lisp/`,
  so commented-out or string-embedded forms can't be miscounted;
  `emacs-packages-eval` (eval only) checks that every scanned and
  Nix-provided package resolves under `legacyPackages.emacs-packages`.
- **Binary smoke test**: `emacs-binaries` runs the built Emacs under a
  sandboxed HOME and checks for host-config leakage.

`elisp-compile` is the same `nix/config-compiled.nix` derivation that
`module.nix` deploys as `compiledConfig`, so on the default configuration
CI's artifact is what `home-manager switch` installs.

### Info manual and man pages

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

The man pages are mdoc: `jotain.7` at the root, `docs/jotain-*.7`, and the
design and review notes under `docs/design/` and `docs/reviews/`.
`nix/site.nix` renders them with mandoc into `/man/`. Lint a page with
`mandoc -Tlint <file>`. `release.json` supplies the version, repo and
support address shown on the landing page.

## Coding Style & Naming Conventions

Follow the module conventions above. Global Elisp symbols use the `jotain-`
prefix (`jotain--` for internals); `lisp/devenv.el` uses `devenv-`. Nix
formatting is enforced by `nix/treefmt.nix`; `.editorconfig` sets the basic
whitespace rules. For Emacs internals and Elisp practice, use the
`.claude/skills/` skills rather than memory.

## Testing Guidelines

Put ERT files in `test/`: the `elisp-test` check loads every `test/*.el`
(with `-L lisp`), so new files need no registration. Test module behaviour
or helper functions. Run `just test` for ERT alone and `just check` before
opening a PR.

## Commit & Pull Request Guidelines

Commits follow Conventional Commits, as configured in `.convco`:
`type(scope): subject` with a short imperative subject, e.g.
`feat(elisp): add a Kotlin module`, `fix(nix): keep ASPELL_CONF out of the
default wrapper`, `ci: pin actions by SHA`. Types: `feat`, `fix`,
`security`, `revert`, `build`, `chore`, `ci`, `docs`, `style`, `refactor`,
`perf`, `test`. Common scopes: `elisp`, `nix`, `ci`, `site`, `docs`, `ui`,
`completion`, `themes`.

A `feat`, `fix`, `refactor`, `perf` or `revert` commit also adds a changie
fragment under `.changes/unreleased/`, created with `changie new` (kinds:
Added, Changed, Deprecated, Removed, Fixed, Security). `CHANGELOG.md` is
assembled from the fragments at release time.

Install the hooks once per clone, inside the dev shell:
`prek install -t pre-commit -t commit-msg`. The `pre-commit` hook runs
`gitleaks git --staged`, and the `commit-msg` hook runs `convco check` on the
message (`.pre-commit-config.yaml`).

PRs include a summary, issue links, check results, and screenshots only for
UI-facing changes.

## Security & Configuration Tips

- Bump `flake.lock` with `just update`.
- Do not commit secrets: `gitleaks` scans staged files (`.gitleaks.toml`
  extends the default rules).
- Generated files: edit the source, then regenerate.
  `docs/configuration/package-reference.mdx` comes from `;;; @doc` blocks
  (`just docs-refresh-packages`); `docs/reference/language-support.mdx` from
  `etc/lang-eval/jotain-lang-registry.el` (`just docs-refresh-lang-matrix`);
  `website/public/ds` from `nix/design-pin.nix` (`just ds-sync`);
  `CHANGELOG.md` from `.changes/` with changie.
- Never commit generated state: `elpa/`, `var/`, `result*`, `*.elc`.
