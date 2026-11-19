# Changelog

All notable changes to jotain are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
The project has no tagged releases yet. Each version below stands for one era
of the history, ending at the matching `era/*` git tag, with the date of the
last commit of the era. Pending changes live as fragments in
`.changes/unreleased/` and are managed with [changie](https://changie.dev).

## 0.9.0 - 2026-11-19

Era 9: port back, 2026-11-10 to 2026-11-19 (`era/9-port-back`). The work of
0.8.1 returns to this repository, and the repository shrinks to one system
and one build.

### Added

- The 0.8.1 features arrive in this repository: repo-review, `devenv-shell`, Astro, Kotlin, the new language modes and the mergiraf and delta integrations
- `services.jotain.users.<name>` (`module-system.nix`) builds the wrapped Emacs, the EDITOR and VISUAL scripts, `jotctl` and the dictionaries for one user and installs them in the user's profile. The Home Manager module takes them from there and keeps the config files and the daemon unit.
- A flake dev shell (`nix develop`, or `nix-shell` through `shell.nix`) with the Nix tooling, linters, docs chain and language servers the config shells out to, plus `prek`, `convco` and `gitleaks`
- `release.json` on the landing page of the website
- Changelog tooling: fragments in `.changes/unreleased/` managed with changie, rendered into `CHANGELOG.md`, with user-visible highlights in `NEWS`
- The `elisp-test` check writes a JUnit report of the ERT run
- PR CI runs the ERT suite and the `cache-parity` check
- ERT coverage for pull request review in repo-review

### Changed

- The Elisp floor is Emacs 31.1
- The LikeC4 major mode is `jotain-likec4-mode`, so it follows the `jotain-` prefix rule
- Commits follow Conventional Commits. `.pre-commit-config.yaml` runs gitleaks on staged files and a convco commit-msg check, and `.convco` maps the types to changelog sections.
- GitHub Actions are pinned to full commit SHAs with the tag in a trailing comment
- The PR preview builds `.#site-preview`, the site without the generated `/help/api/` reference, instead of the full `.#site`
- The man page `jotain.7` and the design and review notes are written in mdoc
- `AGENTS.md` and the README describe the reduced repository

### Removed

- Darwin support: the macOS Nix builds, `darwinModules.default`, the launchd service and the nix-darwin handling in the modules
- aarch64-linux: the flake builds for x86_64-linux only
- The terminal-only build: `emacs-nox`, `jotainEmacsPackagesNoGui` and `jotainEmacsNoGui`; the pgtk build is the only one
- devenv: `devenv.nix`, `devenv.yaml`, `devenv.lock` and the `sync-devenv` workflow, replaced by the flake dev shell
- `services.jotain.devenv.enable`: put `devenv` on the PATH with `home.packages` if you need the CLI
- The nix-on-droid module (`module-nix-on-droid.nix`) and the `nixOnDroidModules` flake output
- The ellsp and Elsa derivations
- `scripts/bootstrap-agent-env.sh` and its SessionStart hook, together with `scripts/verify-locks.sh`
- `TODO.md` and the tracked Claude Code settings

### Fixed

- `services.jotain.spell.dictionaries` reaches jinx: a non-empty list sets `ASPELL_CONF` in the wrapped Emacs and the default is empty (bundled en/fi/de/fr). The option was a no-op before.
- Links from the Info manual to docs-site routes resolve
## 0.8.1 - 2026-11-19

Side branch: development inside a monorepo, 2026-10-11 to 2026-11-19
(`era/8-side-monorepo`). The configuration lived in a monorepo next to other
projects and for a while without the Jotain name. Only changes to the Emacs
configuration and its docs are listed; commits to the other projects of the
monorepo are left out.

### Added

- repo-review (`lisp/repo-review.el`), wired into the config: review the files of a repository as a queue of read-only diffs
- repo-review: review a Forge pull request or merge request (GitHub or GitLab) with `repo-review-pullreq`, bound to `C-x p V`
- repo-review: save draft review comments on pull request diffs (`c`) and post them to GitHub or GitLab after a confirmation (`P`)
- repo-review: mark files as viewed (`m`, `u`, `v`), show the count in the queue header, skip viewed files with `N`, and clear the mark of a file that changed since; `repo-review-mark-viewed-on-visit`
- repo-review: optional gptel commands that draft a review comment for a diff line (`a`) and check a draft (`A`)
- `devenv-shell` in `devenv.el` for per-buffer activation without trust
- Astro support (astro-ts-mode and astro-ls), registered in the language matrix
- Full Kotlin support. Eglot uses kotlin-lsp or kotlin-language-server, ktfmt formats on save, ktlint and detekt report through Flymake, dape debugs through kotlin-debug-adapter, `C-c C-t` runs Gradle tests and `C-c C-z` opens a REPL
- KDL major mode for .kdl files, jq-mode for .jq files, and modes for GLSL, Gherkin, MDX, JSONC and build.zig.zon files
- Flymake diagnostics from mandoc in man pages
- `C-c ^ S` in smerge-mode re-resolves a conflicted file with mergiraf (`jotain-vc-mergiraf-solve`); mergiraf joins the wrapper PATH
- Optional magit-delta (`jotain-magit-delta-enable`, off by default) renders magit diffs through delta
- magit-land, a landing summary buffer with a queue section in magit-status. It stayed in the monorepo and is not part of 0.9.0.
- `just info` builds the combined Info manual
- A GPL-3.0-or-later header in every Lisp file
- A standalone flake for the configuration, with the off-flake pins read from `flake.lock`
- Install command, version and support section on the landing page of the docs site

### Changed

- The configuration is Linux-only
- The Elisp floor is Emacs 31.1, and the `newcomers-presets` theme is enabled; settings equal to their defaults are dropped
- Fonts follow the design system: Fira Sans for body text and IBM Plex Mono for code
- The vendored design system assets move through versions 4.0 to 6.0
- Claude models route to the direct Anthropic API with OpenRouter twins as fallback, and the gptel and eca model lists move to the 5.5 models
- gptel defaults to the direct Claude backend (`claude-sonnet-5-5`, `ANTHROPIC_API_KEY`). The OpenRouter backend stays defined and selectable from the `C-c S` menu.
- likec4-lsp fetches its npm dependencies with `importNpmLock` from the vendored lockfile, so it no longer pins an `npmDepsHash`
- The NixOS and Home Manager modules reuse `pkgs` when it already has the overlay, instead of `pkgs.extend`, which built another nixpkgs instance per module
- Open .h files as C, format Python with ruff, use lua-ts-mode for Lua
- The Markdown notes and the README become mdoc pages
- The docs site builds without the third-party API reference by default
- ERT runs natively in `just test`

### Removed

- macOS-only Elisp: the `system-type` branches and the macOS paths and settings
- The nix-on-droid module
- Flake-era scripts and unused code

### Fixed

- Lists no longer shake in Markdown and Org; mixed-pitch faces are extended after load
- The HTML manual ships its own `manual.css`, so it has a stylesheet where it is served outside the project site
- Generated API reference pages get URL-safe file names, so symbols such as `consult--keep!` are served correctly
- API docs: percent-encoded text fragments, unique shortdoc group anchors, rainbow depths 6 to 9 coloured
- The `module-eval` check asserts that `EDITOR` is set and the graphical start target is wired; it used to compute both and pass regardless
- `elisp-lint` covers every file in `lisp/`, including `repo-review.el` and any new Lisp file
- `lisp/devenv.el` declares `devenv-env--pending` before `devenv-revoke` uses it, so the file byte-compiles cleanly on its own
- devenv: log truncation no longer signals on multibyte output; pending buffers with `devenv-env-mode` off are skipped
- Structurizr mode no longer stays inside a comment after a # line
- exec-path-from-shell runs the login shell (`-l`), so the profile PATH is imported
- Restarting an Org Babel session kills the old session buffer
- The live language probe connects eglot under batch instead of queueing a hook, and the probes write JSON null instead of the string json-null
- The benchmark and debug harnesses use named hooks, setopt and provide; nested loads are no longer double counted
- Completion preview adopts corfu sort order after corfu loads; the `C-x j` which-key label is correct
- Nix blank-line indentation ignores a trailing comment when it looks for an opener
- Git status jump skips files deleted in the work tree; jj renames open the new path; magit-todos is enabled as documented; the git-stats modeline skips remote files
- repo-review: deleted tracked files are not queued, and a rebuild checks the file list and the buffer kill before it touches the queue
- A background dired refresh no longer asks to kill the diff-hl status process, and a prompt opened without a frame no longer locks the keyboard of the daemon
- The modeline icon switch follows the selected frame, so a tty frame no longer disables icons on GUI frames of a mixed daemon
- .scss files open in scss-mode; only plain CSS uses css-ts-mode, whose grammar cannot parse SCSS
- IBM Plex Mono is selected on installs that have only the NixOS font package

### Security

- repo-review refuses absolute paths, entries outside the review root and symlinks that lead out of it, and says why
## 0.8.0 - 2026-10-09

Era 8: Jotain, current generation, 2026-04-07 to 2026-10-09
(`era/8-jotain-current-generation`). The rewrite from `next` becomes main
and grows into the full distribution.

### Added

- The current layout: `early-init.el`, a small `init.el` and one `lisp/init-*.el` module per concern
- Emacs built from `emacs.nix` with defaults that match nixpkgs and emacs-overlay, so default builds are binary-cache hits; a `cache-parity` check guards it
- The full distribution (`jotainEmacsPackages`) with every tree-sitter grammar, and `nix/use-package.nix`, which maps `use-package` forms to `emacsPackages` automatically
- Home Manager module: Jotain as a user daemon with `emacsclient`, a byte-compiled config, separate EDITOR and VISUAL scripts and the `jotctl` daemon command
- NixOS, nix-darwin and nix-on-droid modules, and compatibility with nixpkgs 24.05 and newer
- Module options for the ECA config, a secrets environment file, auth-source files and the SonarLint, Dockerfile LSP, 1Password, sops and Claude Code toggles
- Flake checks: formatting, statix, deadnix, Elisp lint, compile and test, `module-eval`, `scanner-fidelity`, `eca-models-in-sync`, `emacs-packages-eval` and a failure on unresolved Emacs packages
- `legacyPackages.emacs-packages` exposes every bundled Emacs package
- Docs: module option reference, the `jotain.info` Info manual wired into the Emacs help system, a package reference generated from `;;; @doc` blocks, a keybindings page, an ergonomics chapter and a per-package Emacs Lisp API reference
- A generated website on GitHub Pages with a landing page, a package search page and PR previews as artifacts
- A per-language IDE feature evaluation engine with a generated language-support matrix (`just lang-matrix`, `just lang-eval-live`)
- Jylhis design system themes (paper and roast), followed up to version 3.0.0
- `devenv.el`: native devenv.sh integration with allow and revoke trust, a mode-line status and automatic use of language servers a project provides
- Languages: Go (go-ts-mode, gopls tuning, dape, gotest), Zig, Bazel and Starlark, Meson, OCaml, Haskell, Just, QML for Quickshell, Robot Framework, Structurizr DSL, LikeC4, CUDA and C/C++ on tree-sitter
- TypeScript, TSX and Python Eglot sessions routed through rassumfrassum; SonarLint; eglot-booster; rust-analyzer config and dape for cargo
- AI: eca, tagref, gptel with a curated OpenRouter model list
- Version control: magit, full Jujutsu (jj) support, git-stats counters in the modeline, `jotain-switch-git-status-buffer`, global diff-hl
- Completion: Tempel snippets, cape capfs, vertico in a posframe, Embark workflows
- Editing and UI: an emoji picker, a docker TUI with a podman or docker backend switch, async dired copies and dired-rsync, a window-resize repeat-map, which-key prefix labels, a DWIM `keyboard-quit`, mixed-pitch prose, `jotain-font-scale`
- Spell checking with jinx and bundled aspell dictionaries
- Org Babel as a notebook environment, a `C-c m` keymap for markdown-mode, and opt-in verb and casual modules
- A Ghostty-based terminal through ghostel
- `project-nix-store`, a project.el backend for the Nix store
- `jotain-screenshot` and `just screenshot` for headless captures
- Startup tooling: a benchmark recipe, the compiled fast launch (`just run-built-fast`) and the `run-built-debug-log` harness
- Opt-in builds: ccache, the `cpuTune` perf build and lighter builds with curated grammars
- `just update-pins` for the hand-pinned upstreams
- The Emacs internals and Elisp knowledge base with its two skills
- README, LICENSE and a contributor guide

### Changed

- The baseline moves to Emacs 31: package defaults in May, the emacs-overlay `unstable` variant as the default in July, the 31.1 line in August
- The Linux GUI build defaults to pgtk
- npins gives way to flake-compat; `flake.lock` is the single source of input revisions
- Startup drops from about 10 s to about 3 s by deferring eager loads; later passes defer more packages and built-ins
- Package archives are never fetched at startup
- treesit-auto is replaced by `major-mode-remap-alist`
- envrc is replaced by the native devenv integration
- Primary entry points move into the user-reserved `C-c` key space
- TAB indents and completes, RET inserts a newline, `C-M-i` completes
- The GNU Emacs `newcomers-presets` user options are adopted
- GC is restored after startup with a guarded idle collection
- CI splits into lean PR checks and a full validation run on main, on x86_64-linux only
- `CLAUDE.md` merges into `AGENTS.md`
- The docs site moves from Mintlify to a site generated from the repository

### Removed

- wakatime and activity-watch tracking; keyfreq stays
- The jylhis Emacs fork input and the legacy GUI variants
- treesit-auto, envrc, diminish and no-littering
- The psychotherapist, games, mail and news menu items
- The devenv Emacs stubs and old planning files

### Fixed

- Opening a file was eight times slower with `global-treesit-auto-mode`
- The daemon crashed at startup on a void `vc-git-root`
- Clean installs bootstrap without `:vc` fallbacks or stale archives
- Auto-dark picks the right theme order on macOS and enables a theme in terminal frames
- Nerd icons show in dired; dired works on macOS without GNU ls
- git-stats no longer races foreground git operations and counts changes independent of the locale
- The doom-modeline worktree icon no longer causes a redisplay storm
- `C-M-i` commits the selected candidate; the zoxide echo on find-file is silenced
- Stale ELPA core packages no longer shadow Emacs 31 built-ins
- The Emacs wrapper drops the trailing INFOPATH colon
- devenv loads the shell environment, not the derivation environment, and activates without a subprocess
- Eglot workspace configuration applies through the global default; eglot-booster leaves remote servers alone
- Org list items and wrapped lines keep their indentation
- emacsclient frames get the right variable-pitch font size
- `custom.el` writes no longer raise a supersession prompt

### Security

- GnuTLS verification and the Network Security Manager are enforced
- The editor wrappers refuse option injection
- Org Babel asks before it evaluates code
- Risky JavaScript LSP companions are opt-in
- git-stats hardens its subprocess calls and its `.git` parsing
- savehist no longer persists ring contents
- CI: Cachix auth only on push events, hardened Pages workflow permissions, no PAT persisted in `.git/config`
## 0.7.0 - 2026-04-07

Era 7: the rewrite on `next`, 2026-04-01 to 2026-04-07
(`era/7-next-rewrite`). A new configuration started from vanilla Emacs.

### Added

- A minimal `early-init.el` and `init.el`: no tool bar, no startup screen, `custom-file`, a Modus theme and nix-mode
- `emacs.nix`, a source build of Emacs with Just targets for every cached variant, and a distribution layer with all tree-sitter grammars
- A devenv shell with Nix formatting, treefmt and an Emacs Lisp module
- Nix derivations for the ellsp language server and the Elsa CLI, disabled by default
- `jotain-telemetry`, a PostHog usage telemetry module, disabled again within the week
- Editor instrumentation with org-clock for a self-tracking stack
- nix-ts-mode, markdown-mode, magit and a richer consult setup
- Font preferences with fallbacks and Ghostty terminal compatibility
- The first ERT tests
- Mintlify docs under `docs/`, a guide to setting variables and a development journal
- Claude Code agents, skills and a lint hook

### Changed

- `default.nix` is renamed `emacs.nix`
- `defcustom` variables are set with `setopt`

### Fixed

- A void `package-archives` at startup
- A consult autoload error from a deleted preview hook
- Darwin build defaults and the macport variant
## 0.6.0 - 2026-04-01

Era 6: Jotain, first generation, 2025-09-28 to 2026-04-01
(`era/6-jotain-first-generation`). The configuration leaves the NixOS
repository and gets its own flake; the repository is renamed to jotain on
2025-10-20.

### Added

- A flake with a formatter, a dev shell, overlays, a Home Manager module and a config package
- CI that runs `nix flake check`, rolling FlakeHub publishing and Renovate
- ERT tests and Home Manager (NMT) module tests, with suites for the programming and completion modules
- Automatic extraction of package dependencies from `use-package` forms
- All tree-sitter grammars from `pkgs.tree-sitter.allGrammars`
- Completion: cape, kind-icon and history sorting
- kkp, elec-pair, Gited, csv-mode, jinja2-mode, pdf-tools and a world clock
- Docker and SonarLint language servers, Vue support and React through typescript-ts-mode
- The Enlight dashboard
- A theme that follows the system setting
- An `ai.el` module with the claude-code-ide integration
- A window split toggle and automatic creation of missing directories
- Worktrees in the magit status buffer
- macOS support through darwin-emacs and a multi-platform CI
- A Mintlify documentation site
- Claude Code workflows, agents turned into skills, MCP servers for the dev shell and speckit
- `.editorconfig` and `.dir-locals.el`

### Changed

- The minimum Emacs version is 30.2, and the config adopts the Emacs 30 built-ins
- Customizable variables are set with `setopt`
- Symbols move from the `my/` and `jotain/` prefixes to `jotain-`
- Themes: Modus themes 4, then Doom themes with doom-nord for dark mode
- expand-region is replaced by expreg, and the dashboard by Enlight
- The default `fill-column` is 100
- The modules are self-contained and take an `emacsPackage` option
- The Emacs fallback is the unstable pgtk variant
- Yes-or-no prompts no longer use dialog boxes

### Removed

- Obsidian support
- bash-language-server
- `config.nix` and the devJot CLI
- The beads issue tracker and the org-mcp server
- emacs-lsp-booster, which broke the Darwin build

### Fixed

- A typo in `early-init.el` that broke startup
- Themes no longer blend when one is loaded over another
- Child frames inherit the font; Nerd Font variants restore the icons
- Infinite recursion in the Home Manager module
- `platform-gui-p` and `platform-terminal-p` evaluate at call time
- sops keybindings apply only in sops-mode
- Faster Org agenda file discovery and font lookups

### Security

- A directory traversal in the file trust check
## 0.5.0 - 2025-09-28

Era 5: config inside the NixOS repository, 2025-05-20 to 2025-09-28
(`era/5-nixos-repo`). The configuration is deployed with Home Manager from
the owner's NixOS repository.

### Added

- An Emacs package and Home Manager module in the NixOS repository, with a GitHub workflow for Emacs
- A modular config: `config/*.el` per concern (core, ui, completion, programming, git, writing, systems, ai) and helpers in `lisp/`
- Org agenda files and Org tweaks
- avy, dired omit, zoxide, compile-multi, dash-docs, web-mode and a vterm module build
- Eglot with diagnostics integration and Flymake settings
- AI tools: aider, GitHub Copilot with keybindings and Claude
- A platform support system with an Android config
- An application launcher
- 1Password as an auth-source and Obsidian notes through obsidian.el
- A minibuffer depth indicator
- ERT tests

### Changed

- `init.el` becomes a literate `init.org` in June and a modular `init.el` again in July
- GC is tuned for startup and reset afterwards
- The default theme is light, with auto-dark switching
- orderless is removed and magit-delta disabled

### Fixed

- Middle mouse paste lands at the cursor (`mouse-yank-at-point`)
- The minibuffer no longer uses `cursor-intangible-mode`
- Font setup and several byte-compile warnings
## 0.4.0 - 2024-07-20

Era 4: standalone emacs.d, 2024-05-28 to 2024-07-20 (`era/4-emacs.d`).

### Added

- A new `~/.emacs.d` from scratch, with packages from straight.el and one `personal/setup-*.el` file per topic
- LSP and DAP, tree-sitter, flycheck, projectile, treemacs and yasnippet
- Setup files for Rust, Python, Go, Nix, PowerShell, JSON, Markdown, GitLab CI, Docker and Jupyter
- A theme and a script that starts the server on macOS
## 0.3.0 - 2017-08-14

Era 3: own config again, 2017-08-13 to 2017-08-14 (`era/3-own-config`).

### Added

- GLSL support

### Changed

- The fork is replaced by a single `init.el` with a package list, a menu-bar file and a debugger file in `lisp/`
## 0.2.0 - 2017-03-31

Era 2: fork of an upstream emacs.d, 2017-03-31 (`era/2-upstream-fork`).

### Changed

- The config becomes a fork of Purcell's emacs.d with the old settings merged in
- The README lists the fonts
- Desktop save is disabled
## 0.1.0 - 2017-03-30

Era 1: dotfiles-era config, 2016-07-10 to 2017-03-30 (`era/1-dotfiles`).

### Added

- The first `init.el`, kept with the dotfiles
- gitignore-mode, dockerfile-mode, fic-mode, web-mode and evil-mode
- C and C++: rtags, C++14 and clang-format
- Python: autocompletion, linting and Python 3
- Erlang support
- emacsclient setup, copyright snippets, themes and helper scripts

### Changed

- Packages come from melpa-stable
- A new base for the config on 2016-11-27, split into `lisp/init-*.el` files
- The desktop session is disabled

### Removed

- ggtags, replaced by rtags
- `.travis.yml`

### Fixed

- Opening files from the command line
- A `cl` workaround and the `lexical-let` use
