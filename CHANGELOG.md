# Changelog

All notable changes to emacs are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
This project has no versioned releases, so entries are grouped by month.
Entries up to 2026-11 were reconstructed from git history (first commit 2024-05-28).

## [Unreleased]

### Added

- `just info` builds and opens a combined Info manual covering the jylhis man pages and the Emacs configuration. ([cb838529c6](https://github.com/Jylhis/monorepo/commit/cb838529c6))

### Changed

- Claude models route to the direct Anthropic API on the desktop with OpenRouter twins as fallback, and the gptel and eca model lists move to the 5.5 models. ([645d85fc1d](https://github.com/Jylhis/monorepo/commit/645d85fc1d))
- The documentation site is renamed `emacs-docs` and deployed to docs.example.org from its own workflow. ([aab05e8d5d](https://github.com/Jylhis/monorepo/commit/aab05e8d5d))

### Fixed

- The HTML manual ships its own `manual.css`, so it has a stylesheet where it is served outside the emacs-docs site.
- Generated API reference pages get URL-safe file names, so symbols such as `consult--keep!` are served correctly. ([dbaf2352c4](https://github.com/Jylhis/monorepo/commit/dbaf2352c4))

## 2026-10

### Added

- **Repo review:** `C-x p R` opens a whole-repository review queue with per-project Org queues and org-remark notes beside them. Entries whose `REVIEW_PATH` is absolute or escapes the review root are refused. ([71bc44254e](https://github.com/Jylhis/monorepo/commit/71bc44254e), [04280269f1](https://github.com/Jylhis/monorepo/commit/04280269f1), [eeeb3bd902](https://github.com/Jylhis/monorepo/commit/eeeb3bd902))
- `devenv-shell` applies a project's dev environment to the current buffer, or to all project buffers with a prefix, without recording auto-activation trust. ([20cff724b6](https://github.com/Jylhis/monorepo/commit/20cff724b6), [b9a9eaf4b9](https://github.com/Jylhis/monorepo/commit/b9a9eaf4b9))
- Minibuffer completion for `M-x`, buffer switching and symbol help floats in a vertico-posframe child frame, toggled with `M-P`; terminals keep the plain minibuffer. ([af7c76c3ea](https://github.com/Jylhis/monorepo/commit/af7c76c3ea))
- Astro support: `.astro` files open in `astro-ts-mode` with `astro-ls` wired into Eglot. ([3c41c320d2](https://github.com/Jylhis/monorepo/commit/3c41c320d2), [a3e11e597f](https://github.com/Jylhis/monorepo/commit/a3e11e597f))
- Opt-in `cpuTune` option and `just build-perf` build Emacs with `-O3` and CPU-specific flags. ([6cd1f8d0e4](https://github.com/Jylhis/monorepo/commit/6cd1f8d0e4))

### Changed

- The configuration is renamed to emacs-config: the Elisp prefix, environment variables and Nix module options change, and the old OpenRouter and eca option aliases are removed. ([515c4ee27a](https://github.com/Jylhis/monorepo/commit/515c4ee27a))
- Emacs 31.1 is now the minimum version; Emacs 30 guards are dropped, many settings that merely restated defaults are deleted, and the Emacs 31 newcomers presets are enabled last so local overrides still win. ([b5f6d070a9](https://github.com/Jylhis/monorepo/commit/b5f6d070a9), [a001b129a7](https://github.com/Jylhis/monorepo/commit/a001b129a7), [3114d8e017](https://github.com/Jylhis/monorepo/commit/3114d8e017))
- Fonts: Geist is the body font, GeistMono Nerd Font leads the default list, Fira Sans is preferred for variable-pitch text, and NixOS-only installs get BlexMono Nerd Font instead of falling back to DejaVu Sans Mono. ([6b45bec504](https://github.com/Jylhis/monorepo/commit/6b45bec504), [5f6e3e289c](https://github.com/Jylhis/monorepo/commit/5f6e3e289c), [41629215af](https://github.com/Jylhis/monorepo/commit/41629215af))
- The Emacs themes and docs-site styles build from the in-tree design system instead of a separately pinned revision. ([7a5e40a333](https://github.com/Jylhis/monorepo/commit/7a5e40a333), [50d12f56db](https://github.com/Jylhis/monorepo/commit/50d12f56db))
- Website hosting moves from GitHub Pages to a Cloudflare Workers site. ([cef5b12c12](https://github.com/Jylhis/monorepo/commit/cef5b12c12))
- Init-prog options live in a `jotain-prog` customize group, Eglot workspace settings are merged through one helper, and the idle garbage collection timer collects only after a quarter of the threshold has been consed. ([07e83b8ab3](https://github.com/Jylhis/monorepo/commit/07e83b8ab3), [1f3a76e532](https://github.com/Jylhis/monorepo/commit/1f3a76e532), [c48f5da136](https://github.com/Jylhis/monorepo/commit/c48f5da136))
- Dependency updates (1 commit).

### Removed

- macOS support: the NS build, Homebrew patches, launchd agent and Darwin LSP entries are removed, leaving the pgtk and no-GUI Linux builds. ([8af9ec0c77](https://github.com/Jylhis/monorepo/commit/8af9ec0c77))

### Fixed

- Devenv auto-activation no longer blocks `find-file`; unknown trust is resolved by a background probe. ([61d846f591](https://github.com/Jylhis/monorepo/commit/61d846f591))
- The Emacs wrapper no longer appends a trailing colon to `INFOPATH`, which the hardened binary wrapper rejected. ([82861d90e4](https://github.com/Jylhis/monorepo/commit/82861d90e4))
- Markdown buffers stop shaking list items and keep markers and code fixed-pitch under mixed-pitch, and a void-variable error that left them without font lock is fixed. ([2f1e4e889c](https://github.com/Jylhis/monorepo/commit/2f1e4e889c), [3d58fdda95](https://github.com/Jylhis/monorepo/commit/3d58fdda95))
- `vc-dir-auto-hide-up-to-date` no longer triggers a type warning at startup on Emacs 31.1. ([9c363d1ee2](https://github.com/Jylhis/monorepo/commit/9c363d1ee2))

## 2026-09

### Added

- Eglot wraps local language servers in emacs-lsp-booster, and rust-analyzer gets clippy-on-save settings. ([7d9b8f82ea](https://github.com/Jylhis/monorepo/commit/7d9b8f82ea), [0c617e385c](https://github.com/Jylhis/monorepo/commit/0c617e385c))
- Robot Framework support through `robot-mode`, with Eglot and formatting on save. ([bd11466c4c](https://github.com/Jylhis/monorepo/commit/bd11466c4c))
- A `project-nix-store` backend treats each store directory as a project root, so `project-find-file` works inside the Nix store. ([d33a7a0abb](https://github.com/Jylhis/monorepo/commit/d33a7a0abb))
- Each bundled Emacs package is individually buildable (for example `nix build .#emacs-packages.magit`), with an eval-only check that every declared package resolves. ([e39cc2d061](https://github.com/Jylhis/monorepo/commit/e39cc2d061), [e364b992d3](https://github.com/Jylhis/monorepo/commit/e364b992d3))
- `just update-pins` bumps the hand-pinned upstreams outside the lock file. ([7edd18cc74](https://github.com/Jylhis/monorepo/commit/7edd18cc74))
- The site gains a `/packages/` page with live search over the generated API reference. ([92f9c9e6ad](https://github.com/Jylhis/monorepo/commit/92f9c9e6ad))

### Changed

- Emacs themes move to the design v2 survey themes and then to the single `jylhis-light` and `jylhis-dark` themes from design system 3.0.0. ([61e3ed9a50](https://github.com/Jylhis/monorepo/commit/61e3ed9a50), [2f5e5760df](https://github.com/Jylhis/monorepo/commit/2f5e5760df))
- TAB now indents and completes, with a second TAB accepting the selected candidate; `jotain-completion-free-tab` restores strict indent-only behaviour, and `C-M-i` inserts the preselected candidate. ([cd7274545f](https://github.com/Jylhis/monorepo/commit/cd7274545f), [513e6ebd31](https://github.com/Jylhis/monorepo/commit/513e6ebd31))
- Emacs 31's newcomers presets are ported into the config, and ffap, shr, bookmark, re-builder and apropos load lazily. ([bfd0da7779](https://github.com/Jylhis/monorepo/commit/bfd0da7779), [630c651ef0](https://github.com/Jylhis/monorepo/commit/630c651ef0))
- C-ish and markup modes move to tree-sitter: OCaml uses neocaml, Haskell uses haskell-ts-mode, Just uses just-ts-mode and HTML uses mhtml-ts-mode. ([13ec44e9bd](https://github.com/Jylhis/monorepo/commit/13ec44e9bd))
- The documentation site moves from Cloudflare Workers to GitHub Pages, and pull request previews become downloadable workflow artifacts. ([3c5b3b9d39](https://github.com/Jylhis/monorepo/commit/3c5b3b9d39), [5f6419b9b0](https://github.com/Jylhis/monorepo/commit/5f6419b9b0))
- Dependency updates (1 commit).

### Fixed

- Stale ELPA copies of xref, project, eglot and flymake no longer shadow Emacs 31's built-ins, fixing a startup error. ([dd31464071](https://github.com/Jylhis/monorepo/commit/dd31464071))
- Eglot settings for rust-analyzer, gopls and yaml-language-server actually reach the servers, and eglot-booster no longer wraps remote TRAMP servers. ([0c617e385c](https://github.com/Jylhis/monorepo/commit/0c617e385c), [98f651cc9a](https://github.com/Jylhis/monorepo/commit/98f651cc9a))
- The doom-modeline version control segment no longer errors on every redisplay in a git worktree. ([871b787e3a](https://github.com/Jylhis/monorepo/commit/871b787e3a))
- Org buffers no longer jump while typing list items, `go.work` is no longer a project root marker, and sops mode only starts when the binary exists. ([fcebaa35db](https://github.com/Jylhis/monorepo/commit/fcebaa35db), [cfb472b4f9](https://github.com/Jylhis/monorepo/commit/cfb472b4f9), [0e4ae84843](https://github.com/Jylhis/monorepo/commit/0e4ae84843))

### Security

- TLS connections verify server certificates and reject weak Diffie-Hellman primes, and the Network Security Manager runs at its strictest level. ([398292cb72](https://github.com/Jylhis/monorepo/commit/398292cb72))

## 2026-08

### Added

- **Org notebook:** Org Babel is set up with 18 languages, a trust-based evaluation prompt, inline image refresh and a `C-c b` prefix. ([993a8e7fac](https://github.com/Jylhis/monorepo/commit/993a8e7fac))
- **Language support:** QML (`qml-ts-mode`, qmlls, qmlformat), CUDA (`cuda-ts-mode`), Structurizr DSL, and LikeC4 modes, plus a per-language registry, probes and a generated Language Support page. ([2a857fb836](https://github.com/Jylhis/monorepo/commit/2a857fb836), [c183425b2f](https://github.com/Jylhis/monorepo/commit/c183425b2f), [1dd977075a](https://github.com/Jylhis/monorepo/commit/1dd977075a))
- Markdown-mode commands get a `C-c m` prefix for hiding markup, links, code blocks, preview and heading promotion. ([b26628ea94](https://github.com/Jylhis/monorepo/commit/b26628ea94))
- An Org-based verb HTTP client under `C-c C-r`, and casual transient menus for dired, calc, isearch, ibuffer and Info. ([a7b76205ac](https://github.com/Jylhis/monorepo/commit/a7b76205ac))
- A browsable per-package and per-symbol Emacs Lisp API reference generated from docstrings and served on the site. ([7228a9a2a3](https://github.com/Jylhis/monorepo/commit/7228a9a2a3), [0de1bbb5dd](https://github.com/Jylhis/monorepo/commit/0de1bbb5dd))
- `just showcase` headlessly captures UI scenes under both themes; per-file compile checks and a staged build are added. ([c30c79bc61](https://github.com/Jylhis/monorepo/commit/c30c79bc61), [c776768d22](https://github.com/Jylhis/monorepo/commit/c776768d22))
- Home Manager options `eca.settings`, `eca.environmentFile`, `services.jotain.authSources` and opt-in toggles for the 1Password, sops and Claude Code CLIs on the wrapper PATH. ([59b2373553](https://github.com/Jylhis/monorepo/commit/59b2373553), [24181f3d9d](https://github.com/Jylhis/monorepo/commit/24181f3d9d), [2b477c7311](https://github.com/Jylhis/monorepo/commit/2b477c7311))

### Changed

- The default Emacs moves from the 31.0.91 pretest to the 31.1 release candidate. ([1912e70815](https://github.com/Jylhis/monorepo/commit/1912e70815))
- Corfu no longer commits a candidate while typing and gains documentation popup and inline preview toggles; scrolling keeps point visible. ([52f4e7c38a](https://github.com/Jylhis/monorepo/commit/52f4e7c38a), [b78d28f4e6](https://github.com/Jylhis/monorepo/commit/b78d28f4e6))
- Dired confirms deletes with y or n, uses natural version sort and omits `.DS_Store`; C and C++ open in the tree-sitter modes. ([4b07e6d92b](https://github.com/Jylhis/monorepo/commit/4b07e6d92b), [64fb9d285b](https://github.com/Jylhis/monorepo/commit/64fb9d285b))
- Startup defers the login-shell PATH lookup and other eager work, and the menu bar stays visible. ([2620ef047b](https://github.com/Jylhis/monorepo/commit/2620ef047b), [6b58fdac21](https://github.com/Jylhis/monorepo/commit/6b58fdac21))
- Distribution builds fail and list any unresolved Emacs package, and the Emacs build matrix narrows to four variants. ([a55bd4dac7](https://github.com/Jylhis/monorepo/commit/a55bd4dac7), [44157ff4ca](https://github.com/Jylhis/monorepo/commit/44157ff4ca))

### Removed

- The wakatime and activity-watch integrations are removed, and Read Mail, Read Net News, Games and Getting New Versions disappear from the menu bar. ([97267e71e3](https://github.com/Jylhis/monorepo/commit/97267e71e3), [b050f655cc](https://github.com/Jylhis/monorepo/commit/b050f655cc))

### Fixed

- TAB and backtab in dired use dirvish's own subtree commands, ending listing corruption, and the diff-hl dired autoload no longer empties listings. ([9fff989492](https://github.com/Jylhis/monorepo/commit/9fff989492), [006c3d1a9b](https://github.com/Jylhis/monorepo/commit/006c3d1a9b))
- Terminal frames where auto-dark cannot detect the appearance now load the configured theme, and opening a file no longer echoes a zoxide message. ([d876e4d0ea](https://github.com/Jylhis/monorepo/commit/d876e4d0ea), [de22d60a39](https://github.com/Jylhis/monorepo/commit/de22d60a39))

## 2026-07

### Added

- **Devenv integration:** a `C-c v` transient for tasks, builds and processes, native environment loading in place of direnv and envrc, `devenv-allow` and `devenv-revoke` trust management, and a mode-line indicator. ([4da598b4b7](https://github.com/Jylhis/monorepo/commit/4da598b4b7), [d0027a7232](https://github.com/Jylhis/monorepo/commit/d0027a7232), [e3131e2999](https://github.com/Jylhis/monorepo/commit/e3131e2999))
- A Go module with `go-ts-mode`, gopls workspace settings, gotest, dape debugging and goimports formatting, and OCaml support with tuareg, dune and ocamllsp. ([46d0d162b2](https://github.com/Jylhis/monorepo/commit/46d0d162b2), [9da97ee25a](https://github.com/Jylhis/monorepo/commit/9da97ee25a), [b0d72dea45](https://github.com/Jylhis/monorepo/commit/b0d72dea45))
- Eglot starts automatically for any project file whose language server is on the PATH, and a Messages notice appears when a classic mode has an available tree-sitter counterpart. ([9da97ee25a](https://github.com/Jylhis/monorepo/commit/9da97ee25a), [0b86cfa73e](https://github.com/Jylhis/monorepo/commit/0b86cfa73e))
- `jotain-screenshot`, exposed as an MCP tool, and a `just screenshot` recipe that renders the frame under Xvfb. ([cdfc75b972](https://github.com/Jylhis/monorepo/commit/cdfc75b972))
- Static landing page and documentation site at docs.example.org, assembled from the docs, Info manual, man pages and generated references. ([60bec618af](https://github.com/Jylhis/monorepo/commit/60bec618af), [3dd89f5d8b](https://github.com/Jylhis/monorepo/commit/3dd89f5d8b))
- Bootstrap scripts bring the config up in fresh containers and on Debian or Ubuntu without Nix. ([a6f2a78e60](https://github.com/Jylhis/monorepo/commit/a6f2a78e60), [9e2b7c8884](https://github.com/Jylhis/monorepo/commit/9e2b7c8884))
- `emacs-nox` and `run-built-debug` recipes, and a `run-built-debug-log` recipe mirroring messages and backtraces into `var/debug`. ([ed61044152](https://github.com/Jylhis/monorepo/commit/ed61044152), [174b9b1f23](https://github.com/Jylhis/monorepo/commit/174b9b1f23))

### Changed

- **Completion redesign:** auto-popup only in code, RET always inserts a newline, TAB completes via `tab-always-indent`, Elisp comments and docstrings complete symbols, and spell-checking runs globally through jinx. ([cf208befbe](https://github.com/Jylhis/monorepo/commit/cf208befbe), [574bd1563b](https://github.com/Jylhis/monorepo/commit/574bd1563b), [288a779682](https://github.com/Jylhis/monorepo/commit/288a779682))
- The default Emacs becomes emacs-overlay's unstable variant (the Emacs 31 pretest), and the Linux GUI build defaults to pgtk. ([47d691138e](https://github.com/Jylhis/monorepo/commit/47d691138e), [e97c44520b](https://github.com/Jylhis/monorepo/commit/e97c44520b))
- vterm is replaced by ghostel, and terminal support moves into an `init-terminal` module. ([fb56298e71](https://github.com/Jylhis/monorepo/commit/fb56298e71), [7e02de7399](https://github.com/Jylhis/monorepo/commit/7e02de7399))
- treesit-auto is removed and classic modes remap to tree-sitter variants through `major-mode-remap-alist`, falling back when a grammar is missing. ([418a64b5d8](https://github.com/Jylhis/monorepo/commit/418a64b5d8))
- Keybinding moves avoid shadowing: git status jump to `C-x G`, jj to `C-x J`, gptel to `C-c s`/`C-c S`, eca to `C-c e`, claude-code-ide to `C-c q`, and `helpful-at-point` to `C-h .`. ([ddf5b12e8d](https://github.com/Jylhis/monorepo/commit/ddf5b12e8d), [6503a3c862](https://github.com/Jylhis/monorepo/commit/6503a3c862))
- Startup and GC tuning: lazier loading of consult, corfu and cape, byte-compiled `.elc` copies in the Home Manager deployment, and native compilation jobs scaled to half the cores. ([ccf57ce4f1](https://github.com/Jylhis/monorepo/commit/ccf57ce4f1), [df99dd7dea](https://github.com/Jylhis/monorepo/commit/df99dd7dea), [f00ec0fa3d](https://github.com/Jylhis/monorepo/commit/f00ec0fa3d), [d6deb9ca63](https://github.com/Jylhis/monorepo/commit/d6deb9ca63))
- Author-specific paths become `jotain-notes-directory` and `jotain-repositories-roots` options. ([53cea3802f](https://github.com/Jylhis/monorepo/commit/53cea3802f))
- Design v2 themes and fonts (Zilla Slab, Hanken Grotesk, IBM Plex Mono) are adopted. ([2d27344773](https://github.com/Jylhis/monorepo/commit/2d27344773))
- Dependency updates (4 commits).

### Removed

- The diminish package and the melpa-stable archive are removed. ([9322a278ec](https://github.com/Jylhis/monorepo/commit/9322a278ec), [f9b4d8e3c6](https://github.com/Jylhis/monorepo/commit/f9b4d8e3c6))

### Fixed

- Spell-checking works out of the box with bundled English, Finnish, German and French dictionaries. ([f87d6bdb9a](https://github.com/Jylhis/monorepo/commit/f87d6bdb9a))
- Devenv buffers layer PATH instead of replacing it, so rust-analyzer and cargo work. ([4c8e2b9c6b](https://github.com/Jylhis/monorepo/commit/4c8e2b9c6b))
- Nix buffers no longer error on first Eglot load, TAB on a blank line in `nix-ts-mode` indents to the enclosing block, and the x86_64-darwin build is pinned to a supported nixpkgs. ([90e5365b2b](https://github.com/Jylhis/monorepo/commit/90e5365b2b), [950933569c](https://github.com/Jylhis/monorepo/commit/950933569c), [636b2a1fe3](https://github.com/Jylhis/monorepo/commit/636b2a1fe3))
- Whitespace cleanup is skipped during super-save, and doom-modeline icons are shown only on graphical frames. ([775896a5d4](https://github.com/Jylhis/monorepo/commit/775896a5d4), [513da74ef2](https://github.com/Jylhis/monorepo/commit/513da74ef2))

## 2026-06

### Added

- **Language support:** Zig (`zig-ts-mode`, zls, zig fmt, compile entries), Bazel and Starlark with buildifier, and Meson file support. ([24c3c0f330](https://github.com/Jylhis/monorepo/commit/24c3c0f330), [e4234816cd](https://github.com/Jylhis/monorepo/commit/e4234816cd), [11273c2515](https://github.com/Jylhis/monorepo/commit/11273c2515))
- tempel snippets with `M-+`/`M-*`, curated templates for six languages and eglot-tempel merged into one completion popup. ([3ad2e5106a](https://github.com/Jylhis/monorepo/commit/3ad2e5106a), [fa84c78dcd](https://github.com/Jylhis/monorepo/commit/fa84c78dcd), [2c481ab470](https://github.com/Jylhis/monorepo/commit/2c481ab470))
- Jujutsu support through `vc-jj` and majutsu (`C-c j`), with a jj-aware modeline counter. ([381ea9f630](https://github.com/Jylhis/monorepo/commit/381ea9f630))
- A `jotctl` command starts, stops, restarts and shows logs for the daemon on systemd or launchd. ([ca2b3e44f0](https://github.com/Jylhis/monorepo/commit/ca2b3e44f0))
- A nix-on-droid module provides a terminal-only Emacs for Android. ([8b4c6e17e7](https://github.com/Jylhis/monorepo/commit/8b4c6e17e7), [3e3b4dbf01](https://github.com/Jylhis/monorepo/commit/3e3b4dbf01))
- Opt-in lighter builds: `emacs-lite` bundles only the grammars in use. ([37b0d483bc](https://github.com/Jylhis/monorepo/commit/37b0d483bc))
- Guarded Emacs 31 quality-of-life settings (`kill-region-dwim`, readable ibuffer sizes, minibuffer-regexp-mode, window-tree transposes under `C-x W`). ([604db31f39](https://github.com/Jylhis/monorepo/commit/604db31f39), [1d09ea3d6f](https://github.com/Jylhis/monorepo/commit/1d09ea3d6f))

### Changed

- OpenRouter becomes the default gptel backend, with direct Anthropic, Gemini and Ollama backends selectable and a curated model list. ([e8c723e078](https://github.com/Jylhis/monorepo/commit/e8c723e078), [5dd1517953](https://github.com/Jylhis/monorepo/commit/5dd1517953))
- Base Emacs builds realign with upstream nixpkgs, and the overlay composes emacs-overlay. ([f9d459337a](https://github.com/Jylhis/monorepo/commit/f9d459337a), [8c0c277753](https://github.com/Jylhis/monorepo/commit/8c0c277753))
- Native compilation is capped at three background jobs, scrolling is fast-but-imprecise, and savehist persists kill and search rings again. ([bee6e87a8a](https://github.com/Jylhis/monorepo/commit/bee6e87a8a), [604db31f39](https://github.com/Jylhis/monorepo/commit/604db31f39))
- Prose buffers use mixed-pitch-mode so Org tables stay aligned. ([936cd250e6](https://github.com/Jylhis/monorepo/commit/936cd250e6))
- Dependency updates (4 commits).

### Removed

- The full jylhis Emacs package set is no longer a flake output while the fork still crashes. ([7d740f1f97](https://github.com/Jylhis/monorepo/commit/7d740f1f97))

### Fixed

- Emacs 30 ispell completion is disabled in text buffers, ending "No plain word-list found" errors. ([36df249de4](https://github.com/Jylhis/monorepo/commit/36df249de4))
- The Docker dispatcher moves to `C-c w` so it no longer shadows dirvish. ([c3b19e0fb1](https://github.com/Jylhis/monorepo/commit/c3b19e0fb1))
- The UI falls back to Modus themes when the Jylhis themes package is missing. ([e58993c8b8](https://github.com/Jylhis/monorepo/commit/e58993c8b8))

## 2026-05

### Added

- **Git workflow:** `C-x C-g` jumps to files `git status` reports, the modeline shows uncommitted-change and commits-today counters, and a `docker` transient on `C-c d` drives podman by default. ([6d76707e2c](https://github.com/Jylhis/monorepo/commit/6d76707e2c), [5c22ba0ffe](https://github.com/Jylhis/monorepo/commit/5c22ba0ffe), [2e1ff8a79d](https://github.com/Jylhis/monorepo/commit/2e1ff8a79d))
- **Project management:** `C-x p P` opens projects from `~/Projects`, `~/code` or `~/src` with tab-bar workspaces per project, and projects are labelled by their full parent path. ([c6f8ede88e](https://github.com/Jylhis/monorepo/commit/c6f8ede88e), [39724f9046](https://github.com/Jylhis/monorepo/commit/39724f9046))
- Multiple language servers run at once for TypeScript, TSX and Python through the rass multiplexer. ([9fc2f2823b](https://github.com/Jylhis/monorepo/commit/9fc2f2823b), [9b8dbdfde5](https://github.com/Jylhis/monorepo/commit/9b8dbdfde5))
- Dired copy, rename and link operations run asynchronously, and `C-c C-r` hands marked files to rsync. ([5d9d15d012](https://github.com/Jylhis/monorepo/commit/5d9d15d012))
- Ergonomics: window-resize repeat map, regexp-replace-as-diff on `M-s R`, transpose-sentences and paragraphs, DWIM `C-g`, and a color emoji fallback with the `C-x 8 e` picker. ([9e23a16b6b](https://github.com/Jylhis/monorepo/commit/9e23a16b6b), [b21b8829ae](https://github.com/Jylhis/monorepo/commit/b21b8829ae), [7591e47ce5](https://github.com/Jylhis/monorepo/commit/7591e47ce5), [826fc6198b](https://github.com/Jylhis/monorepo/commit/826fc6198b))
- The eca editor code assistant and tagref integration, and a Keybindings reference with which-key labels. ([afcb8f90eb](https://github.com/Jylhis/monorepo/commit/afcb8f90eb), [ca146f3fac](https://github.com/Jylhis/monorepo/commit/ca146f3fac))
- A jylhis Emacs backend built from the Meson fork, selectable in the Home Manager, NixOS and nix-darwin modules. ([30b3549947](https://github.com/Jylhis/monorepo/commit/30b3549947), [d69ad5f2b7](https://github.com/Jylhis/monorepo/commit/d69ad5f2b7))
- A generated package reference documents every `use-package` block from `;;; @doc` markers. ([c5abb97de6](https://github.com/Jylhis/monorepo/commit/c5abb97de6), [0d70b3c979](https://github.com/Jylhis/monorepo/commit/0d70b3c979))

### Changed

- Themes switch to `jylhis-paper` and `jylhis-roast` with Literata as variable-pitch font. ([b237baf963](https://github.com/Jylhis/monorepo/commit/b237baf963), [08187d50b4](https://github.com/Jylhis/monorepo/commit/08187d50b4))
- The default Emacs build moves from 30 to 31, sourced from emacs-overlay. ([b88435a0a0](https://github.com/Jylhis/monorepo/commit/b88435a0a0), [6d89586af6](https://github.com/Jylhis/monorepo/commit/6d89586af6))
- Org Babel asks before evaluating source blocks, and `M-j` and `C-c ;` get multi-line comment behaviour. ([a4fdab6786](https://github.com/Jylhis/monorepo/commit/a4fdab6786), [91143b3cfd](https://github.com/Jylhis/monorepo/commit/91143b3cfd))
- Built-in defaults are adopted: short answers, bookmark autosave, isearch match counts, and a keyboard-quit remap. ([e06f4254c7](https://github.com/Jylhis/monorepo/commit/e06f4254c7), [ddd9bf87ec](https://github.com/Jylhis/monorepo/commit/ddd9bf87ec))
- YAML buffers run `prog-mode-hook` but no longer inherit variable-pitch, wrapping or spell-checking from text-mode. ([a85b032ed5](https://github.com/Jylhis/monorepo/commit/a85b032ed5), [27b716c946](https://github.com/Jylhis/monorepo/commit/27b716c946))
- Emacs leaves the devenv shell, and CI runs light checks on pull requests and the full suite plus docs deploy on main. ([7a5d9c94cf](https://github.com/Jylhis/monorepo/commit/7a5d9c94cf), [db33f78fba](https://github.com/Jylhis/monorepo/commit/db33f78fba))
- Dependency updates (2 commits).

### Security

- Editor wrappers pass `--` before user arguments, and git-stats subprocesses disable fsmonitor and external diff drivers. ([46f6efbce1](https://github.com/Jylhis/monorepo/commit/46f6efbce1), [801db59b77](https://github.com/Jylhis/monorepo/commit/801db59b77))
- ESLint and Tailwind language servers are opt-in through `jotain-prog-enable-risky-js-lsp`. ([ef72b0015a](https://github.com/Jylhis/monorepo/commit/ef72b0015a))

### Fixed

- Git stats work under non-English locales and during rebases or merges, and the status jump handles renames, deletions and unusual file names. ([f074ea7f7c](https://github.com/Jylhis/monorepo/commit/f074ea7f7c), [190e042de0](https://github.com/Jylhis/monorepo/commit/190e042de0), [570130c607](https://github.com/Jylhis/monorepo/commit/570130c607), [9270eba2e4](https://github.com/Jylhis/monorepo/commit/9270eba2e4))
- Blocked `.envrc` buffers no longer pop up on `find-file`. ([b78293b862](https://github.com/Jylhis/monorepo/commit/b78293b862))
- The daemon no longer aborts on first run when a save fires before vc-git loads. ([98dc61f381](https://github.com/Jylhis/monorepo/commit/98dc61f381))
- Single `C-g` aborts a minibuffer opened by `M-x TAB`. ([b18fc28994](https://github.com/Jylhis/monorepo/commit/b18fc28994), [e17cc34b17](https://github.com/Jylhis/monorepo/commit/e17cc34b17))

## 2026-04

### Added

- **Vanilla rebuild:** a modular `init-*.el` configuration for completion, magit, eglot, gptel, org and Nix-provided packages, with package lists derived from `use-package` forms and a `just` workflow for run, debug, tty and check. ([af75bd29bb](https://github.com/Jylhis/monorepo/commit/af75bd29bb), [927171afba](https://github.com/Jylhis/monorepo/commit/927171afba), [e09dc9304f](https://github.com/Jylhis/monorepo/commit/e09dc9304f))
- Emacs is built from source with Nix and bundles all tree-sitter grammars, with variants for GUI, nox, pgtk and macport. ([718c966d75](https://github.com/Jylhis/monorepo/commit/718c966d75), [16f3d8171a](https://github.com/Jylhis/monorepo/commit/16f3d8171a), [ea80fa5ec1](https://github.com/Jylhis/monorepo/commit/ea80fa5ec1))
- A Home Manager `services.jotain` module runs the Emacs daemon under systemd or launchd, with an emacsclient desktop entry; `EDITOR` and `VISUAL` default to emacsclient. ([6448a0428f](https://github.com/Jylhis/monorepo/commit/6448a0428f), [75f43b9309](https://github.com/Jylhis/monorepo/commit/75f43b9309))
- `just bench` prints startup time, GC metrics and per-module require timing. ([d6b99531b9](https://github.com/Jylhis/monorepo/commit/d6b99531b9))
- An Info manual with an HTML Nix option reference, and a Mintlify documentation site covering compilation, version control and configuration. ([0387ab4b6a](https://github.com/Jylhis/monorepo/commit/0387ab4b6a), [cad1be5452](https://github.com/Jylhis/monorepo/commit/cad1be5452), [1cf7abe952](https://github.com/Jylhis/monorepo/commit/1cf7abe952))
- Dired gains wdired, diredfl, dired-narrow and dired-subtree. ([696ae6f44c](https://github.com/Jylhis/monorepo/commit/696ae6f44c))
- Ghostty terminal support (xterm-ghostty, Kitty keyboard protocol, OSC 52), a ranked Nerd Font default list, and a consult configuration. ([e8a076f5cc](https://github.com/Jylhis/monorepo/commit/e8a076f5cc), [de75270e1e](https://github.com/Jylhis/monorepo/commit/de75270e1e), [7045bebb59](https://github.com/Jylhis/monorepo/commit/7045bebb59))
- `M-x jotain-sonarlint` starts SonarLint as a secondary Eglot connection. ([da0054686c](https://github.com/Jylhis/monorepo/commit/da0054686c))
- A telemetry mode batches usage events (sessions, time per mode, top commands, errors) to PostHog; it is switched off by default a few days later. ([efafb5e6ed](https://github.com/Jylhis/monorepo/commit/efafb5e6ed), [30f472fe0f](https://github.com/Jylhis/monorepo/commit/30f472fe0f))
- Time tracking through ActivityWatch, wakatime, keyfreq and org-clock-csv. ([3cc0215e80](https://github.com/Jylhis/monorepo/commit/3cc0215e80))

### Changed

- Startup drops from about ten seconds to about three, and removing global-treesit-auto-mode cuts average `find-file` time from 2.4 s to 0.29 s. ([6a1c29bfca](https://github.com/Jylhis/monorepo/commit/6a1c29bfca), [91f3867296](https://github.com/Jylhis/monorepo/commit/91f3867296))
- The Nix layout settles on an overlay, `default.nix` and a thin flake, with the Emacs wrapper putting ripgrep, fd, git, direnv and coreutils on PATH. ([e84e562a70](https://github.com/Jylhis/monorepo/commit/e84e562a70), [f17d1eed6b](https://github.com/Jylhis/monorepo/commit/f17d1eed6b))
- no-littering is replaced by a `jotain-var-file` helper. ([1fb9f6f191](https://github.com/Jylhis/monorepo/commit/1fb9f6f191))
- nix-ts-mode is pinned to the Nix-provided build so tree-sitter queries match. ([3af947b2f0](https://github.com/Jylhis/monorepo/commit/3af947b2f0))
- Dependency updates (2 commits).

### Fixed

- macOS fixes: dired falls back to BSD ls, auto-dark detects appearance in terminal frames, and the macport variant builds. ([8f650adf4d](https://github.com/Jylhis/monorepo/commit/8f650adf4d), [abfaa101a5](https://github.com/Jylhis/monorepo/commit/abfaa101a5), [43ae20adde](https://github.com/Jylhis/monorepo/commit/43ae20adde))
- Startup errors from missing modus-themes and GitHub-only packages are resolved. ([77c20bd923](https://github.com/Jylhis/monorepo/commit/77c20bd923))
- Dired icons use the first installed Nerd Font; blocked `.envrc` errors stay quiet. ([1e178c2322](https://github.com/Jylhis/monorepo/commit/1e178c2322), [4bbae08deb](https://github.com/Jylhis/monorepo/commit/4bbae08deb))
- The Home Manager module installs the Emacs package and wraps it with an explicit init directory so the daemon never falls back to `~/.emacs.d`. ([9167537e00](https://github.com/Jylhis/monorepo/commit/9167537e00), [5b09b5368a](https://github.com/Jylhis/monorepo/commit/5b09b5368a))

## 2026-03

### Added

- **Rewrite:** all modules are removed and the configuration is rebuilt on a `lisp/` (built-in features) and `modules/` (third-party packages) split, with new `jotain-defaults` and `jotain-platform` modules and an architecture guide. ([875f963ba2](https://github.com/Jylhis/monorepo/commit/875f963ba2), [240a023ccc](https://github.com/Jylhis/monorepo/commit/240a023ccc), [c742a0cb33](https://github.com/Jylhis/monorepo/commit/c742a0cb33))
- gptel multi-provider chat, apheleia, eglot-booster, combobulate, envrc, doom-modeline, indent-bars, pulsar, forge and git-timemachine. ([c9cdc9c7e8](https://github.com/Jylhis/monorepo/commit/c9cdc9c7e8))
- Vue support through `vue-mode` and vue-language-server, and TS, TSX, JSX and JS files open in the built-in tree-sitter modes. ([a68ade52f0](https://github.com/Jylhis/monorepo/commit/a68ade52f0), [8fd7c698b0](https://github.com/Jylhis/monorepo/commit/8fd7c698b0), [5f024db907](https://github.com/Jylhis/monorepo/commit/5f024db907))
- PDFs open in pdf-tools, and visiting a file in a missing directory creates it. ([0f79d2beaf](https://github.com/Jylhis/monorepo/commit/0f79d2beaf), [973b276f73](https://github.com/Jylhis/monorepo/commit/973b276f73))
- macOS builds via nix-darwin-emacs, and a Mintlify documentation site. ([8f4a8f0107](https://github.com/Jylhis/monorepo/commit/8f4a8f0107), [636e9c34dd](https://github.com/Jylhis/monorepo/commit/636e9c34dd))

### Changed

- Fill column is 100 with a column indicator in prog buffers, region expansion uses expreg, and user options are set with `setopt`. ([8b1807b49f](https://github.com/Jylhis/monorepo/commit/8b1807b49f), [8cca171b79](https://github.com/Jylhis/monorepo/commit/8cca171b79), [35e1a2add3](https://github.com/Jylhis/monorepo/commit/35e1a2add3))
- Buffers with the same name are disambiguated by directory (`src/index.ts`), and flymake diagnostics at point show via eldoc. ([85472247cd](https://github.com/Jylhis/monorepo/commit/85472247cd), [2c23a87378](https://github.com/Jylhis/monorepo/commit/2c23a87378))
- The development shell moves to devenv, and the Emacs base falls back to the unstable PGTK build first. ([c157d0d6f6](https://github.com/Jylhis/monorepo/commit/c157d0d6f6), [41a468d228](https://github.com/Jylhis/monorepo/commit/41a468d228))
- magit-todos loads on demand, and Elisp helpers move to the `jotain-utils-` prefix. ([1c881f0b87](https://github.com/Jylhis/monorepo/commit/1c881f0b87), [02c9d08aeb](https://github.com/Jylhis/monorepo/commit/02c9d08aeb))
- Dependency updates (2 commits).

### Removed

- The Home Manager NMT checks, the VM check and the CI workflow are removed during the rewrite, leaving formatting and ERT checks. ([c65af1a0b1](https://github.com/Jylhis/monorepo/commit/c65af1a0b1), [e9b1abedc5](https://github.com/Jylhis/monorepo/commit/e9b1abedc5))

### Security

- The trust check for local Emacs Lisp files uses `file-in-directory-p` instead of a string prefix match, closing a directory traversal bypass. ([57d21bba16](https://github.com/Jylhis/monorepo/commit/57d21bba16))

### Fixed

- Nerd-icons glyphs render again after moving to the split nerd-fonts set. ([2ae59f3d73](https://github.com/Jylhis/monorepo/commit/2ae59f3d73))
- Theme switching unloads every previous theme cleanly, and child frames inherit the default font. ([981af0d5de](https://github.com/Jylhis/monorepo/commit/981af0d5de), [a01ba18e7b](https://github.com/Jylhis/monorepo/commit/a01ba18e7b))
- Sops key bindings apply only in sops-mode buffers, and pixel scrolling works in daemon and client sessions. ([ca714581f8](https://github.com/Jylhis/monorepo/commit/ca714581f8), [0d73d49f4f](https://github.com/Jylhis/monorepo/commit/0d73d49f4f))
- The Home Manager module no longer recurses infinitely with conditional fontconfig, and the darwin build avoids pulling stock Emacs. ([a4f2da2d7f](https://github.com/Jylhis/monorepo/commit/a4f2da2d7f), [ebcdc72d8b](https://github.com/Jylhis/monorepo/commit/ebcdc72d8b), [a5d9c280f2](https://github.com/Jylhis/monorepo/commit/a5d9c280f2))

## 2026-02

### Added

- `jinja2-mode` for `.j2` and `.jinja2` files, and the claude-code-ide package on `C-c C-'`. ([5f4ed8bb7d](https://github.com/Jylhis/monorepo/commit/5f4ed8bb7d), [06e88aa712](https://github.com/Jylhis/monorepo/commit/06e88aa712))

### Changed

- The theme follows the system appearance through auto-dark, with `jylhis-theme-light` and `jylhis-theme-dark` options, and the dark theme becomes doom-nord. ([f7081744e6](https://github.com/Jylhis/monorepo/commit/f7081744e6), [0ea6ecd5d0](https://github.com/Jylhis/monorepo/commit/0ea6ecd5d0), [6302a4fc32](https://github.com/Jylhis/monorepo/commit/6302a4fc32))
- `load-theme` disables currently enabled themes first, so faces no longer blend. ([637806a08d](https://github.com/Jylhis/monorepo/commit/637806a08d))
- The dashboard gains a Files section and yields to files opened at launch. ([9dc2a4094d](https://github.com/Jylhis/monorepo/commit/9dc2a4094d), [0835090388](https://github.com/Jylhis/monorepo/commit/0835090388))
- The Home Manager module no longer writes an inline `init.el`. ([f9407ee25d](https://github.com/Jylhis/monorepo/commit/f9407ee25d))

### Fixed

- Theme toggling moves to `C-c t` after modus-themes v4 removed its toggle command. ([98ba5a8297](https://github.com/Jylhis/monorepo/commit/98ba5a8297))
- `platform-show-info` no longer freezes enumerating fonts. ([a2dc35f339](https://github.com/Jylhis/monorepo/commit/a2dc35f339))
- tree-sitter-quint is excluded from the bundled grammars because it broke hash checks. ([1b71a85add](https://github.com/Jylhis/monorepo/commit/1b71a85add))

## 2026-01

### Added

- An Enlight startup dashboard with the day's agenda, Downloads and project switching. ([3747b0f4b2](https://github.com/Jylhis/monorepo/commit/3747b0f4b2), [64d7b5d8be](https://github.com/Jylhis/monorepo/commit/64d7b5d8be))
- Eglot gains docker-langserver and sonarlint-ls, `csv-mode` with aligned columns, and a worktrees section in the Magit status buffer. ([bc305835b1](https://github.com/Jylhis/monorepo/commit/bc305835b1), [21c861d32e](https://github.com/Jylhis/monorepo/commit/21c861d32e), [6d2a60c48e](https://github.com/Jylhis/monorepo/commit/6d2a60c48e))

### Changed

- The package requires Emacs 30.2 or newer, and every tree-sitter grammar from nixpkgs is installed without a hand-kept list. ([ae72961d86](https://github.com/Jylhis/monorepo/commit/ae72961d86), [50674ff711](https://github.com/Jylhis/monorepo/commit/50674ff711))
- Completion candidates sort by minibuffer history, and a shorter contributor guide plus an end-user Home Manager guide are written. ([0036a9f1e1](https://github.com/Jylhis/monorepo/commit/0036a9f1e1), [eff7584089](https://github.com/Jylhis/monorepo/commit/eff7584089))

### Removed

- bash-language-server and the broken sonarlint-ls package are no longer installed as runtime dependencies. ([010e9533c0](https://github.com/Jylhis/monorepo/commit/010e9533c0), [2671d88ca2](https://github.com/Jylhis/monorepo/commit/2671d88ca2))

### Fixed

- The Emacs wrapper extracts grammar names from both old and new nixpkgs naming. ([3a9a5c909c](https://github.com/Jylhis/monorepo/commit/3a9a5c909c))
- The CSV separator list no longer splits fields on spaces. ([da04d98efa](https://github.com/Jylhis/monorepo/commit/da04d98efa))

## 2025-12

### Added

- The gited package offers a dired-like interface for branches and tags, loaded after Magit. ([183f1d016c](https://github.com/Jylhis/monorepo/commit/183f1d016c))

### Changed

- The flake targets only Linux systems, and a `nix run` quick start lets the configuration be tried without installing. ([a57b2bdc6a](https://github.com/Jylhis/monorepo/commit/a57b2bdc6a), [457a08fac5](https://github.com/Jylhis/monorepo/commit/457a08fac5))
- Redundant `:ensure t` is dropped for Nix-provided packages, with a test checking `ensure` usage. ([768372b558](https://github.com/Jylhis/monorepo/commit/768372b558))

## 2025-11

### Added

- **Jotain rewrite:** a Nix-packaged distribution with `jotain-core`, `jotain-editor`, `jotain-prog` and `jotain-tools` modules requiring Emacs 30.1, plus a `jot` CLI with doctor, eval and run. ([74cb935429](https://github.com/Jylhis/monorepo/commit/74cb935429), [c0ee38b2b0](https://github.com/Jylhis/monorepo/commit/c0ee38b2b0), [96cc7c49e1](https://github.com/Jylhis/monorepo/commit/96cc7c49e1))
- An `M-SPC` leader keymap with Doom-style prefixes, and startup tuning of GC and file handlers. ([407c24829a](https://github.com/Jylhis/monorepo/commit/407c24829a))
- Language modules for Python, JavaScript, Ruby, Perl, assembly, shell, Nix and build systems with LSP, tests and REPL support. ([4d32f5eaef](https://github.com/Jylhis/monorepo/commit/4d32f5eaef))
- A worktree manager (later work-manager) creates, loads, removes and syncs git worktrees with JIRA-based branch names under `C-c w`. ([5d1b354630](https://github.com/Jylhis/monorepo/commit/5d1b354630), [e4db8eb001](https://github.com/Jylhis/monorepo/commit/e4db8eb001), [1b14cd9b17](https://github.com/Jylhis/monorepo/commit/1b14cd9b17))
- `my/toggle-window-split` on `C-x j` flips two windows between side-by-side and stacked. ([ba608afd5d](https://github.com/Jylhis/monorepo/commit/ba608afd5d))
- Runtime dependencies (tree-sitter grammars, LSP servers, CLI tools, fonts) are managed by Nix, with use-package forms scanned automatically to build the package list. ([ef6bbadb10](https://github.com/Jylhis/monorepo/commit/ef6bbadb10), [0ca092d020](https://github.com/Jylhis/monorepo/commit/0ca092d020))
- ERT suites for the programming and completion modules. ([339a39ad43](https://github.com/Jylhis/monorepo/commit/339a39ad43), [4b600e404a](https://github.com/Jylhis/monorepo/commit/4b600e404a))

### Changed

- The earlier monolithic configuration is replaced and `package.el` reads no archives, so packages come only from Nix. ([15766b9e4b](https://github.com/Jylhis/monorepo/commit/15766b9e4b), [fe7eb211d1](https://github.com/Jylhis/monorepo/commit/fe7eb211d1))
- Built-in packages are marked `:ensure nil` and packages load lazily. ([e228bc8606](https://github.com/Jylhis/monorepo/commit/e228bc8606), [dc3d3be26e](https://github.com/Jylhis/monorepo/commit/dc3d3be26e))
- Contributor and user documentation is rewritten for the new layout. ([c5fc769cec](https://github.com/Jylhis/monorepo/commit/c5fc769cec), [427b11049d](https://github.com/Jylhis/monorepo/commit/427b11049d))

### Removed

- The `config/` modules (including the 1Password, Android and per-project settings) and the platform detection and app-launcher files are removed. ([fc6da89eec](https://github.com/Jylhis/monorepo/commit/fc6da89eec), [5b04fb75ec](https://github.com/Jylhis/monorepo/commit/5b04fb75ec))

### Fixed

- treesit-auto installs missing grammars, and embark-eldoc no longer raises a void-function error. ([5e4acd7a2b](https://github.com/Jylhis/monorepo/commit/5e4acd7a2b), [be30f7e624](https://github.com/Jylhis/monorepo/commit/be30f7e624))
- The dark titlebar works again on macOS, fonts install as individual Nix packages, and vterm loads only on Linux. ([272b2fff20](https://github.com/Jylhis/monorepo/commit/272b2fff20))

## 2025-10

### Added

- Terminal Emacs receives richer key events through the kkp package, Corfu popups show kind icons, and electric-pair-mode is enabled. ([85b14f06a6](https://github.com/Jylhis/monorepo/commit/85b14f06a6), [b03414bec3](https://github.com/Jylhis/monorepo/commit/b03414bec3), [31e1c1ecec](https://github.com/Jylhis/monorepo/commit/31e1c1ecec))
- Home Manager module tests, speed-tiered `check-instant`, `check-fast` and `check-full` recipes, and deadnix and statix in the dev shell. ([459af68d5a](https://github.com/Jylhis/monorepo/commit/459af68d5a), [1e522b0a03](https://github.com/Jylhis/monorepo/commit/1e522b0a03), [9e72d51d87](https://github.com/Jylhis/monorepo/commit/9e72d51d87))
- A `config` flake package copies the configuration without tooling files. ([cfecf204f1](https://github.com/Jylhis/monorepo/commit/cfecf204f1))

### Changed

- The project is renamed Jotain, changing the flake overlays, module default and shell alias. ([338f40554f](https://github.com/Jylhis/monorepo/commit/338f40554f))
- Cape file completion triggers on tilde and path prefixes, dabbrev needs three characters, and file and buffer completion use orderless. ([ae18a5e2d3](https://github.com/Jylhis/monorepo/commit/ae18a5e2d3), [baafc4230c](https://github.com/Jylhis/monorepo/commit/baafc4230c))
- Org agenda files are collected from several Notes directories with duplicates removed. ([68ae87449f](https://github.com/Jylhis/monorepo/commit/68ae87449f))
- Helper functions are renamed (`my/display-ansi-colors`, `app-launchers-counsel-launcher`) and given autoloads. ([b148989a59](https://github.com/Jylhis/monorepo/commit/b148989a59), [585be20373](https://github.com/Jylhis/monorepo/commit/585be20373), [b4f65ac2f2](https://github.com/Jylhis/monorepo/commit/b4f65ac2f2))
- The Obsidian integration is disabled and later removed. ([8873c3a2a0](https://github.com/Jylhis/monorepo/commit/8873c3a2a0), [42c2c1517b](https://github.com/Jylhis/monorepo/commit/42c2c1517b))

### Fixed

- Flyspell no longer takes over `C-M-i`, so completion-at-point works. ([2b1d0ffda6](https://github.com/Jylhis/monorepo/commit/2b1d0ffda6))
- The justfile detects the current Nix system so tests run on macOS and ARM Linux, and the dev shell uses an isolated home. ([9ad0bdde38](https://github.com/Jylhis/monorepo/commit/9ad0bdde38), [d0a0689ae6](https://github.com/Jylhis/monorepo/commit/d0a0689ae6))

## 2025-09

### Added

- An app-launcher module starts installed applications through completion. ([52f494265f](https://github.com/Jylhis/monorepo/commit/52f494265f))
- 1Password auth-source integration with a configurable vault and one-hour credential cache. ([d122794775](https://github.com/Jylhis/monorepo/commit/d122794775))
- Obsidian vault integration with completion-at-point. ([25807d20fd](https://github.com/Jylhis/monorepo/commit/25807d20fd))
- A Nix flake with a Home Manager module that installs the configuration, and direnv support. ([18693c3bb2](https://github.com/Jylhis/monorepo/commit/18693c3bb2))

### Changed

- Themes load without a confirmation prompt, magit-todos is added, and symlinks are no longer resolved when visiting files. ([866d9a4575](https://github.com/Jylhis/monorepo/commit/866d9a4575), [5bc77421d9](https://github.com/Jylhis/monorepo/commit/5bc77421d9))
- Middle-mouse paste inserts at the cursor, and minibuffer depth is indicated. ([0e97d8644e](https://github.com/Jylhis/monorepo/commit/0e97d8644e), [336f0ebfc8](https://github.com/Jylhis/monorepo/commit/336f0ebfc8))

## 2025-08

### Added

- Platform detection with Android-specific settings, per-project settings and ERT platform tests. ([d7bc80b4a6](https://github.com/Jylhis/monorepo/commit/d7bc80b4a6))
- Programming and variable-pitch font fallback chains applied across daemon and client sessions. ([cee2b10bdc](https://github.com/Jylhis/monorepo/commit/cee2b10bdc))

### Changed

- The configuration is split into per-topic `config/*.el` modules for core, completion, UI, programming, writing and git. ([59166dd46d](https://github.com/Jylhis/monorepo/commit/59166dd46d))

## 2024-05

### Added

- Initial configuration: an `init.el` with per-topic `personal/setup-*.el` files for defaults, key bindings, completion, LSP, Git and languages such as Go, Python, Rust and Nix. ([42c2e5e06b](https://github.com/Jylhis/monorepo/commit/42c2e5e06b))
