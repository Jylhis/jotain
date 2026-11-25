# AGENTS.md

Always-loaded context for AI coding agents (Claude Code, Pi, etc.) and the
human contributor guide. The manual chapters `docs/architecture.org` and
`docs/development.org` cover the same ground for users.

## What this is

Jotain is a GNU Emacs configuration (floor: Emacs 31.1, per `init.el`'s
`Package-Requires`) plus the Nix expressions that build Emacs and its
packages. One system (`x86_64-linux`), one build: emacs-overlay's
`emacs-unstable-pgtk`, exposed as `jotainEmacs`. Linux only.

## Layout

- `early-init.el`, `init.el`, `lisp/init-*.el` (feature modules, `require`d
  from `init.el` in order; load order in `docs/architecture.org`).
- `lisp/devenv.el` and `lisp/repo-review.el`: standalone packages with their
  own namespaces and no `jotain-` dependencies, wired in by
  `lisp/init-devenv.el` and `lisp/init-review.el`.
- Nix at the root: `flake.nix` (wiring only), `default.nix` (flake-compat),
  `shell.nix`, `overlay.nix`, `module.nix` (Home Manager), `module-system.nix`
  (NixOS). Everything else is in `nix/`: `mk-overlay.nix` (the overlay),
  `checks.nix`, `dev-shell.nix`, `extra-packages.nix`,
  `nix-provided-packages.nix`, `module-options.nix` (options shared by both
  modules), `user-programs.nix`, `wrap-runtime-deps.nix`, `configured.nix`, `runtime-deps.nix`,
  `info-manual.nix`, `site.nix`, `options-doc.nix`, `lang-eval.nix`.
- `docs/*.org`: the manual, with `docs/jotain.org` as master file.
  `docs/design/` holds working notes that are not chapters. `README.org`,
  `release.json` and `website/` (page shell, see `website/README.md`) feed
  the site.
- `test/` (ERT), `etc/` (debug harness, language registry in
  `etc/lang-eval/`), `bench/`, `templates/` (tempel), `config/eca/`.
- `.changes/` (changie), `.changie.yaml`, `CHANGELOG.md`, `.convco`,
  `.pre-commit-config.yaml`, `.gitleaks.toml`, `.editorconfig`.

## Development environment

Run commands inside the dev shell (`nix develop`, or `nix develop -c <cmd>`).
It has tooling only and no Emacs. Build and launch the config with
`just run-built`, which runs the built Emacs with `--init-directory=<repo>`,
isolated from `~/.emacs.d`. The Justfile documents the other recipes
(`just --list`). Bump pinned upstreams that are not in `flake.lock` with
`just update-pins [names]` (nix-update, see the Justfile).

Plain Nix commands: `nix flake check`, `nix fmt`, `nix flake update`, and
`nix build --no-link .#checks.x86_64-linux.<name>` for one check.
`default.nix` loads the flake through flake-compat, which fetches inputs by
URL; where only the flake CLI works, use `nix build .#emacs-with-packages -o result` and
run `./result/bin/emacs` by hand (`.#default` is the same Emacs with the
config linked into `$XDG_STATE_HOME/jotain`).

The checks in `nix/checks.nix` are `packages-default`, `packages-emacs-with-packages`, `options-doc`,
`eca-models-in-sync`, `module-eval`, `dev-shell`, `formatting`, `statix`,
`deadnix`, `elisp-compile`, `elisp-test`, `config-startup` and `default-launcher`.
`eca-models-in-sync` compares the OpenRouter models in
`config/eca/config.json` with gptel's list in `lisp/init-ai.el`; reconcile
both by hand. `elisp-compile` is the same derivation `module.nix` deploys, so
CI's artifact is what `home-manager switch` installs.

## CI

Three workflows in `.github/workflows/`, x86_64-linux only. `ci.yml` runs one
step per check on `pull_request` and `workflow_dispatch`. `deploy.yml` runs
`nix flake check` on pushes to `main` and `next`, and on `main` builds
`.#site` and deploys it with wrangler to the `emacs-docs` Worker at
emacs.j10s.io (repo secrets `CLOUDFLARE_API_TOKEN`, `CLOUDFLARE_ACCOUNT_ID`;
by hand: `just deploy`).
`preview.yml` uploads `.#site` as the artifact `site-preview-pr-<N>`.

Every `uses:` must be GitHub-owned or `cachix/*` (the repo blocks other
actions, and the run then fails at startup with no logs) and pinned by commit
SHA with the version in a trailing comment. Pushes from the Claude Code
GitHub App do not trigger `pull_request` runs; start CI with
`gh workflow run ci.yml --ref <branch>`.

## Emacs and Elisp references

Two skills in `.claude/skills/` hold source-cited references for Emacs 30/31
(index: `.claude/knowledge/emacs/README.md`). Use them instead of answering
Emacs questions from memory: `emacs-internals` for core behaviour,
performance and compilation, `elisp-dev` for writing and reviewing Elisp.
When a note conflicts with the installed Emacs, trust the running Emacs and
fix the reference in the same change.

## Elisp conventions

- No builtins/third-party split. A package that enhances a built-in lives in
  the same file as that built-in (`dirvish` with `dired` in
  `init-navigation.el`, `magit` with `vc` in `init-vc.el`).
- `setopt`, not `setq`, for `defcustom` variables.
- Do not set an option to its upstream default. Comments only for a
  non-obvious why.
- `use-package-always-ensure` is `t` (`early-init.el`), so built-ins and
  packages Nix puts on `load-path` need `:ensure nil`.
- Module shape: `-*- lexical-binding: t; -*-` cookie, ends with
  `(provide 'init-<concern>)`, and a `(require ...)` at the right point in
  `init.el`.
- No version guards. The floor is the build, so use 31 APIs directly.
  `init.el` ends by enabling the `newcomers-presets` theme
  (`jotain-ui-enable-newcomers-presets`), which supplies savehist, recentf,
  which-key, electric pairs and similar. Do not restate what it provides.
- LSP and formatters live in `init-prog.el` (per-language `eglot-ensure`
  hooks, `apheleia` formatters). `init-lang-*.el` holds mode regexes and
  language tweaks. Tree-sitter modes are enabled through
  `treesit-enabled-modes` there.
- Language files: `init-lang-nix/rust/python/go/kotlin/qml` have their own;
  the rest are grouped in `init-lang-web`, `-devops`, `-data`, `-systems`.
  Split a language out only once it grows enough.
- Global symbols use the `jotain-` prefix (`jotain--` for internals);
  `lisp/devenv.el` uses `devenv-`, `lisp/repo-review.el` `repo-review-`.
- `var/custom.el` is the write-only `custom-file`; never load it.

## Nix layer

- Packages come from emacs-overlay's `emacsWithPackagesFromUsePackage`,
  applied in `nix/mk-overlay.nix` to the text of `lisp/init-*.el`. Every
  bundled package therefore needs a literal `(use-package NAME` form.
  Packages no form names go in `nix/nix-provided-packages.nix`. Packages
  absent from every archive, and the shims and overrides, are in
  `nix/extra-packages.nix`.
- The overlay defines `jotainEmacs`, `jotainEmacsPackages` (the
  distribution: runtime deps on `PATH`, the Info manual on `INFOPATH`, a
  default `ASPELL_CONF`, via `nix/wrap-runtime-deps.nix`),
  `jotainEmacsConfigured` (the distribution plus the compiled config, with a
  launcher that links it into `$JOTAIN_INIT_DIRECTORY` or
  `$XDG_STATE_HOME/jotain`; `packages.default`, `nix/configured.nix`), `jotainInfo`,
  `eca` and `likec4Lsp`. Both modules apply it over emacs-overlay.
- `services.jotain` (Home Manager) delegates the daemon to home-manager's
  `services.emacs` (unit `emacs.service`). Options shared with the NixOS
  module are in `nix/module-options.nix`. `nix/user-programs.nix` builds the
  per-user programs for both modules, so they yield the same store paths.
- Pins outside `flake.lock` (the packages in `nix/extra-packages.nix`, the
  ECA server, the likec4 language server) are bumped with
  `just update-pins`.

## Docs

The manual is Org. `docs/jotain.org` `#+INCLUDE`s each chapter, and the
include order is the reading order. A batch Emacs (`nix/org-export.el`,
`nix/site-export.el`) exports it to Info (`.#info`) and to the site
(`.#site`, the only site output). The Module Options and Language Support
appendices are generated at build time from the module sources
(`nix/options-doc.nix`) and from `etc/lang-eval/jotain-lang-registry.el`
(`nix/lang-eval.nix`); edit the sources, not the output. The design-system
CSS and fonts are a vendored copy of the Jylhis design system 7.0.0 in
`website/public/ds`. `just serve-site` serves the site at the root of localhost:8080.

## Testing

ERT files go in `test/`. The `elisp-test` check loads every `test/*.el` with
`-L lisp`, so new files need no registration. Test module behaviour or
helper functions, not restated literals. Prefix test names with the module
name. Run `nix build --no-link .#checks.x86_64-linux.elisp-test` for ERT
alone and `nix flake check` before opening a PR.

## Commits and pull requests

Conventional Commits, as configured in `.convco`: `type(scope): subject`
with a short imperative subject, for example `feat(elisp): add a Kotlin
module`. Types: `feat`, `fix`, `security`, `revert`, `build`, `chore`, `ci`,
`docs`, `style`, `refactor`, `perf`, `test`. A `feat`, `fix`, `refactor`,
`perf` or `revert` commit also adds a changie fragment under
`.changes/unreleased/` (`changie new`; kinds: Added, Changed, Deprecated,
Removed, Fixed, Security). `CHANGELOG.md` is assembled from the fragments at
release time. Install the hooks once per clone inside the dev shell:
`prek install -t pre-commit -t commit-msg`. PRs target `main` and include a
summary, issue links and check results.

## Security and generated state

Do not commit secrets; `gitleaks` scans staged files. Bump `flake.lock` with
`nix flake update`. Never commit generated state: `elpa/`, `var/`,
`result*`, `*.elc`. `CHANGELOG.md` comes from `.changes/` with changie.
