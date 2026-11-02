# Changelog

All notable changes to emacs are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
This project has no versioned releases, so entries are grouped by month.
Entries up to 2026-11 were reconstructed from git history (first commit 2024-05-28).

## [Unreleased]

### Added

- **manual:** Add just info for the combined Info manual ([cb838529c6](https://github.com/Jylhis/monorepo/commit/cb838529c6))
- **emacs-docs:** Rename jotain-site and deploy it to docs.example.org ([aab05e8d5d](https://github.com/Jylhis/monorepo/commit/aab05e8d5d))
- **llm:** Route Claude to the direct Anthropic API on the desktop ([645d85fc1d](https://github.com/Jylhis/monorepo/commit/645d85fc1d))

### Fixed

- **emacs-docs:** Emit URL-safe API page names ([#907](https://github.com/Jylhis/monorepo/pull/907)) ([dbaf2352c4](https://github.com/Jylhis/monorepo/commit/dbaf2352c4))

## 2026-10

### Added

- **design:** Contrast floors, ds.base layer, Geist Mono, scoped Card ([5f6e3e289c](https://github.com/Jylhis/monorepo/commit/5f6e3e289c))
- Add Astro support (astro-ts-mode + astro-ls) ([3c41c320d2](https://github.com/Jylhis/monorepo/commit/3c41c320d2))
- Enable the newcomers-presets theme ([a001b129a7](https://github.com/Jylhis/monorepo/commit/a001b129a7))

### Changed

- Marchyo, emacs, cleandev: Fira Sans and IBM Plex Mono on the desktop ([62eb502581](https://github.com/Jylhis/monorepo/commit/62eb502581))
- **design:** Fix stale comments after the 4.0 token move ([50d12f56db](https://github.com/Jylhis/monorepo/commit/50d12f56db))
- Marchyo site: vendor the design assets so the projection builds ([e3961a57a2](https://github.com/Jylhis/monorepo/commit/e3961a57a2))
- Etc/emacs, jotain-site: build the design assets from lib/design ([7a5e40a333](https://github.com/Jylhis/monorepo/commit/7a5e40a333))
- Register Astro in the lang-eval matrix and docs ([a3e11e597f](https://github.com/Jylhis/monorepo/commit/a3e11e597f))
- **elisp:** Set vc-dir-auto-hide-up-to-date with setq ([9c363d1ee2](https://github.com/Jylhis/monorepo/commit/9c363d1ee2))
- Standalone emacs.nix reads pins from nix/inputs.nix ([6e47ab9c36](https://github.com/Jylhis/monorepo/commit/6e47ab9c36))
- Raise the floor to Emacs 31.1 ([b5f6d070a9](https://github.com/Jylhis/monorepo/commit/b5f6d070a9))
- Stop list shaking in Markdown and Org ([2f1e4e889c](https://github.com/Jylhis/monorepo/commit/2f1e4e889c))
- **jotain-site:** Move the Emacs website to usr.sbin/jotain-site ([cef5b12c12](https://github.com/Jylhis/monorepo/commit/cef5b12c12))
- **TODO:** Consolidate sibling TODO files, index inline TODOs, drop done items ([18c9b10e5b](https://github.com/Jylhis/monorepo/commit/18c9b10e5b))
- Reject absolute repo-review paths and keep the nil-path error ([eeeb3bd902](https://github.com/Jylhis/monorepo/commit/eeeb3bd902))
- Reject repo-review entries outside the review root ([04280269f1](https://github.com/Jylhis/monorepo/commit/04280269f1))
- Make the configuration Linux-only ([8af9ec0c77](https://github.com/Jylhis/monorepo/commit/8af9ec0c77))
- Port asdf deltas ([b3000d73b0](https://github.com/Jylhis/monorepo/commit/b3000d73b0))
- **jotain:** Integrate repo-review into the Emacs config ([71bc44254e](https://github.com/Jylhis/monorepo/commit/71bc44254e))
- **elisp:** Devenv.el fixups (up executable, fetch dedup, lighter) ([b9a9eaf4b9](https://github.com/Jylhis/monorepo/commit/b9a9eaf4b9))
- **elisp:** Add devenv-shell for per-buffer activation without trust ([20cff724b6](https://github.com/Jylhis/monorepo/commit/20cff724b6))
- **jotain:** Put the init-prog defcustoms in a defined group ([07e83b8ab3](https://github.com/Jylhis/monorepo/commit/07e83b8ab3))
- **jotain:** Restore just ds-sync lost in the monorepo import ([f83dda2312](https://github.com/Jylhis/monorepo/commit/f83dda2312))
- **jotain:** Restore vendored ds byte parity with the design pin ([74fccf9887](https://github.com/Jylhis/monorepo/commit/74fccf9887))
- **jotain:** Trim trailing whitespace the pre-push hook enforces ([37ce3ed579](https://github.com/Jylhis/monorepo/commit/37ce3ed579))
- **checks:** Enroll jotain elisp gates ([0816737339](https://github.com/Jylhis/monorepo/commit/0816737339))
- **jotain:** Consume in-tree, drop the flake input ([d517bc3cb1](https://github.com/Jylhis/monorepo/commit/d517bc3cb1))
- **jotain:** Scaffold project.nix, devenv, justfile; drop own CI ([46c9a23897](https://github.com/Jylhis/monorepo/commit/46c9a23897))
- Trim comments and docs, fix stale claims ([6a0c033b68](https://github.com/Jylhis/monorepo/commit/6a0c033b68))
- Describe emacs-unstable as the 31.1 release tag ([1b13a00797](https://github.com/Jylhis/monorepo/commit/1b13a00797))
- **elisp:** Stop silencing async native-comp warnings ([cb3ffb4f34](https://github.com/Jylhis/monorepo/commit/cb3ffb4f34))
- **elisp:** Skip the idle GC when little was allocated ([c48f5da136](https://github.com/Jylhis/monorepo/commit/c48f5da136))
- **skills:** Refresh the Emacs skills for the 31.1 release ([a6dd7a66c7](https://github.com/Jylhis/monorepo/commit/a6dd7a66c7))
- **devenv:** Make env auto-activation subprocess-free ([61d846f591](https://github.com/Jylhis/monorepo/commit/61d846f591))
- **nix:** Port the upstream Darwin patch rework, add fix-ns-x-colors ([0f15bdc375](https://github.com/Jylhis/monorepo/commit/0f15bdc375))
- Triage the remaining TODO entries ([ae08365c1b](https://github.com/Jylhis/monorepo/commit/ae08365c1b))
- **nix:** Add the opt-in cpuTune perf build ([6cd1f8d0e4](https://github.com/Jylhis/monorepo/commit/6cd1f8d0e4))
- Fold plan.md into todo.md Move the open performance points into TODO.md, update the plan.md §3 reference in emacs.nix, and drop plan.md from the AGENTS.md working-notes list; also record the no-op spell.dictionaries module finding ([ea748c5a80](https://github.com/Jylhis/monorepo/commit/ea748c5a80))
- Condense and trim stale commentary across the tree ([f56ebe1a98](https://github.com/Jylhis/monorepo/commit/f56ebe1a98))
- **completion:** Float vertico in a posframe via multiform ([af7c76c3ea](https://github.com/Jylhis/monorepo/commit/af7c76c3ea))
- **nix:** Share the emacsclient EDITOR wrapper between the system modules ([10bc0f1eea](https://github.com/Jylhis/monorepo/commit/10bc0f1eea))
- **elisp:** Centralise the eglot workspace-config merge in init-prog ([1f3a76e532](https://github.com/Jylhis/monorepo/commit/1f3a76e532))
- **nix:** Share the pandoc-to-Texinfo fragment filter ([0724d518d1](https://github.com/Jylhis/monorepo/commit/0724d518d1))
- **nix:** Correct the stale consumer lists in two header comments ([6506b54a5c](https://github.com/Jylhis/monorepo/commit/6506b54a5c))
- **devenv:** Collapse the enterTest store-path assertions into one helper ([f715ff1714](https://github.com/Jylhis/monorepo/commit/f715ff1714))
- Sync the AGENTS.md check inventory with nix/checks.nix ([175cff3868](https://github.com/Jylhis/monorepo/commit/175cff3868))
- **nix:** Drop the unused sonarlintLs overlay attribute ([b0fbd7b4cb](https://github.com/Jylhis/monorepo/commit/b0fbd7b4cb))
- **nix:** Share the runtime-deps wrapper between the system modules ([1321f012e9](https://github.com/Jylhis/monorepo/commit/1321f012e9))
- **nix:** Drop the trailing INFOPATH colon from the Emacs wrapper ([82861d90e4](https://github.com/Jylhis/monorepo/commit/82861d90e4))
- Dependency updates (1 commit)

### Removed

- Drop settings equal to their defaults ([3114d8e017](https://github.com/Jylhis/monorepo/commit/3114d8e017))
- Drop the Jotain name ([515c4ee27a](https://github.com/Jylhis/monorepo/commit/515c4ee27a))

### Fixed

- Select IBM Plex Mono on NixOS-only installs ([41629215af](https://github.com/Jylhis/monorepo/commit/41629215af))
- Use Geist for the body font and install it with the module ([6b45bec504](https://github.com/Jylhis/monorepo/commit/6b45bec504))
- Extend mixed-pitch faces after load ([3d58fdda95](https://github.com/Jylhis/monorepo/commit/3d58fdda95))

## 2026-09

### Added

- Add project-nix-store project.el backend for the Nix store ([#229](https://github.com/Jylhis/monorepo/pull/229)) ([d33a7a0abb](https://github.com/Jylhis/monorepo/commit/d33a7a0abb))
- **lang:** Add Robot Framework support ([bd11466c4c](https://github.com/Jylhis/monorepo/commit/bd11466c4c))
- Add eglot-booster, rust-analyzer config, dape cargo + Go project markers ([7d9b8f82ea](https://github.com/Jylhis/monorepo/commit/7d9b8f82ea))
- Add tree-sitter for OCaml, Haskell, Just, and HTML ([13ec44e9bd](https://github.com/Jylhis/monorepo/commit/13ec44e9bd))
- **devops:** Map .just files to just-mode ([88d1c44b30](https://github.com/Jylhis/monorepo/commit/88d1c44b30))
- **design:** Track Jylhis design v2.0.0 theming framework ([2f5e5760df](https://github.com/Jylhis/monorepo/commit/2f5e5760df))

### Changed

- **nix:** Re-enable majutsu with its real dependency set ([8563279b16](https://github.com/Jylhis/monorepo/commit/8563279b16))
- Hacky fix: revert ([c1987f560e](https://github.com/Jylhis/monorepo/commit/c1987f560e))
- **nix:** Trim history comments and unify version wording ([12975f7dae](https://github.com/Jylhis/monorepo/commit/12975f7dae))
- **elisp:** Trim history and restatement comments ([abe26f2811](https://github.com/Jylhis/monorepo/commit/abe26f2811))
- Add just update-pins for hand-pinned upstreams ([7edd18cc74](https://github.com/Jylhis/monorepo/commit/7edd18cc74))
- **nix:** Re-pin jylhis design to e24a796d ([84512a161b](https://github.com/Jylhis/monorepo/commit/84512a161b))
- **elisp:** Update jylhis design system to 3.0.0 ([61e3ed9a50](https://github.com/Jylhis/monorepo/commit/61e3ed9a50))
- **nix:** Source emacs-api-doc features from emacs-package-set ([1ba132aafb](https://github.com/Jylhis/monorepo/commit/1ba132aafb))
- Document legacyPackages.emacs-packages ([1517326459](https://github.com/Jylhis/monorepo/commit/1517326459))
- **nix:** Expose the config's Emacs packages via legacyPackages ([e39cc2d061](https://github.com/Jylhis/monorepo/commit/e39cc2d061))
- **nix:** Resolve the config's Emacs package set in one module, gated by an eval check ([e364b992d3](https://github.com/Jylhis/monorepo/commit/e364b992d3))
- **preview:** Archive site to a tarball before upload-artifact ([0547f97e98](https://github.com/Jylhis/monorepo/commit/0547f97e98))
- **site:** Switch to Actions-source Pages + downloadable artifact previews ([5f6419b9b0](https://github.com/Jylhis/monorepo/commit/5f6419b9b0))
- **site:** Address Codex review — Pages deploy token, preview 404, stale docs ([302d9141c3](https://github.com/Jylhis/monorepo/commit/302d9141c3))
- **site:** Add /packages/ third-party API search page + branch-based Pages previews ([92f9c9e6ad](https://github.com/Jylhis/monorepo/commit/92f9c9e6ad))
- **nix:** Stop stale ELPA core packages shadowing Emacs 31 built-ins ([dd31464071](https://github.com/Jylhis/monorepo/commit/dd31464071))
- Deploy pr-preview/pr-236 @ f8be4b818e07 ([bea2f56a22](https://github.com/Jylhis/monorepo/commit/bea2f56a22))
- Update agents ([4f11febb03](https://github.com/Jylhis/monorepo/commit/4f11febb03))
- **org:** Stop indentation jumping in list items and wrapped lines ([#235](https://github.com/Jylhis/monorepo/pull/235)) ([fcebaa35db](https://github.com/Jylhis/monorepo/commit/fcebaa35db))
- **config:** Adopt GNU Emacs newcomers-presets-theme user options ([#230](https://github.com/Jylhis/monorepo/pull/230)) ([bfd0da7779](https://github.com/Jylhis/monorepo/commit/bfd0da7779))
- **site:** Build from cd7274545f8c621ea6f11f5f92f4fb6abccbb0c2 ([ed1c66861e](https://github.com/Jylhis/monorepo/commit/ed1c66861e))
- **completion:** Make TAB indent and complete; wire HTML LSP ([cd7274545f](https://github.com/Jylhis/monorepo/commit/cd7274545f))
- **startup:** Defer eager on-demand built-ins (ffap, shr, bookmark, apropos, re-builder) ([630c651ef0](https://github.com/Jylhis/monorepo/commit/630c651ef0))
- Migrate the site to GitHub Pages under a /jotain base path ([3c5b3b9d39](https://github.com/Jylhis/monorepo/commit/3c5b3b9d39))
- Harden network security: GnuTLS verification and NSM ([398292cb72](https://github.com/Jylhis/monorepo/commit/398292cb72))
- **design:** Re-pin to merged main commit ec1bf783 ([81a580c0eb](https://github.com/Jylhis/monorepo/commit/81a580c0eb))
- Use stdenv.hostPlatform.isLinux to drop deprecation warning ([3f307251c2](https://github.com/Jylhis/monorepo/commit/3f307251c2))
- Dependency updates (1 commit)

### Removed

- Prune stale references and fix the cache-parity snippet ([c25c853d5a](https://github.com/Jylhis/monorepo/commit/c25c853d5a))
- Prune completed TODO/plan items, fix stale bench refs ([fd333a4ac4](https://github.com/Jylhis/monorepo/commit/fd333a4ac4))
- Remove disabled Emacs stub recipes and dead devenv scaffolding ([5d533e5e19](https://github.com/Jylhis/monorepo/commit/5d533e5e19))

### Fixed

- **prog:** Keep dape cargo-lldb :program hint from erroring ([c36de52e3f](https://github.com/Jylhis/monorepo/commit/c36de52e3f))
- **prog:** Stop eglot-booster boosting remote servers ([98f651cc9a](https://github.com/Jylhis/monorepo/commit/98f651cc9a))
- **lang:** Apply eglot workspace configuration via the global default ([0c617e385c](https://github.com/Jylhis/monorepo/commit/0c617e385c))
- **project:** Drop dead go.work root marker, keep go.mod ([cfb472b4f9](https://github.com/Jylhis/monorepo/commit/cfb472b4f9))
- **lang-data:** Accept GitLab !reference tags in yaml-language-server ([ba93e3aaba](https://github.com/Jylhis/monorepo/commit/ba93e3aaba))
- **systems:** Enable global-sops-mode only when the sops CLI is present ([0e4ae84843](https://github.com/Jylhis/monorepo/commit/0e4ae84843))
- **ui:** Stop doom-modeline worktree-icon redisplay storm ([871b787e3a](https://github.com/Jylhis/monorepo/commit/871b787e3a))
- **completion:** Declare corfu-insert for the per-file split compile ([26bd402754](https://github.com/Jylhis/monorepo/commit/26bd402754))
- **completion:** Make C-M-i commit the selected candidate ([513e6ebd31](https://github.com/Jylhis/monorepo/commit/513e6ebd31))
- **lang-systems:** Require treesit directly for cuda-ts-mode ([660a33357c](https://github.com/Jylhis/monorepo/commit/660a33357c))

## 2026-08

### Added

- Add per-language IDE-feature evaluation engine ([c0fa78e473](https://github.com/Jylhis/monorepo/commit/c0fa78e473))
- **qml:** Add QML editing support for Quickshell ([2a857fb836](https://github.com/Jylhis/monorepo/commit/2a857fb836))
- **completion:** Calmer, more natural in-buffer completion ([52f4e7c38a](https://github.com/Jylhis/monorepo/commit/52f4e7c38a))
- **nix:** Per-file config compile split + staged temacs build ([c776768d22](https://github.com/Jylhis/monorepo/commit/c776768d22))
- **api-doc:** Split generator into per-package cacheable derivations ([0de1bbb5dd](https://github.com/Jylhis/monorepo/commit/0de1bbb5dd))
- **nix:** Fail on unresolved Emacs packages; keep packaging dual-source-safe ([a55bd4dac7](https://github.com/Jylhis/monorepo/commit/a55bd4dac7))
- **lang:** Add LikeC4 architecture-as-code support ([1e0478494e](https://github.com/Jylhis/monorepo/commit/1e0478494e))
- Add verb and casual opt-in modules ([a7b76205ac](https://github.com/Jylhis/monorepo/commit/a7b76205ac))
- **nix:** Add scanner-fidelity flake check ([2cdecaef94](https://github.com/Jylhis/monorepo/commit/2cdecaef94))
- **ui:** Stop forcing the menu bar off in frame defaults ([6b58fdac21](https://github.com/Jylhis/monorepo/commit/6b58fdac21))
- **nix:** Add eca-models-in-sync flake check ([94322ea240](https://github.com/Jylhis/monorepo/commit/94322ea240))
- **devops:** Add Structurizr DSL major mode for .dsl files ([1dd977075a](https://github.com/Jylhis/monorepo/commit/1dd977075a))
- **showcase:** Scene harness and capture recipe ([c30c79bc61](https://github.com/Jylhis/monorepo/commit/c30c79bc61))
- **writing:** Surface markdown-mode commands under a C-c m keymap ([b26628ea94](https://github.com/Jylhis/monorepo/commit/b26628ea94))
- **org:** Configure Org Babel as a notebook environment ([993a8e7fac](https://github.com/Jylhis/monorepo/commit/993a8e7fac))

### Changed

- Ship lang-eval engine and templates to the elisp-test check ([880c16bc61](https://github.com/Jylhis/monorepo/commit/880c16bc61))
- **lang-eval:** Read snippet templates once per run ([714380517b](https://github.com/Jylhis/monorepo/commit/714380517b))
- Quiet cuda-ts-mode tree-sitter grammar probes ([7b066c887e](https://github.com/Jylhis/monorepo/commit/7b066c887e))
- **CUDA:** Use a real cuda-ts-mode on the tree-sitter-cuda grammar ([c183425b2f](https://github.com/Jylhis/monorepo/commit/c183425b2f))
- Integrate secrets with Emacs auth-source ([24181f3d9d](https://github.com/Jylhis/monorepo/commit/24181f3d9d))
- Move C/C++ to tree-sitter and add CUDA support ([64fb9d285b](https://github.com/Jylhis/monorepo/commit/64fb9d285b))
- Enhance dired defaults per Cherti file-management article ([4b07e6d92b](https://github.com/Jylhis/monorepo/commit/4b07e6d92b))
- Generalize the secrets env file and add external-integration CLI toggles ([2b477c7311](https://github.com/Jylhis/monorepo/commit/2b477c7311))
- Wire language evaluation into Nix, Justfile, and docs nav ([211a5b4b3a](https://github.com/Jylhis/monorepo/commit/211a5b4b3a))
- Expose ECA config + credentials and module parity through the Nix modules ([59b2373553](https://github.com/Jylhis/monorepo/commit/59b2373553))
- Tune scrolling behaviour per jamescherti recommendations ([b78d28f4e6](https://github.com/Jylhis/monorepo/commit/b78d28f4e6))
- **nix:** Upgrade Emacs to the 31.1 line (emacs-unstable 31.1-rc1) ([1912e70815](https://github.com/Jylhis/monorepo/commit/1912e70815))
- Generate per-package Emacs Lisp API reference (fork elisp-doc) ([7228a9a2a3](https://github.com/Jylhis/monorepo/commit/7228a9a2a3))
- **reviews:** Add 2026-08 Emacs sources research survey ([f475346b91](https://github.com/Jylhis/monorepo/commit/f475346b91))
- **startup:** Cut eager surface, add compiled fast-launch and real benchmark ([2620ef047b](https://github.com/Jylhis/monorepo/commit/2620ef047b))
- **reviews:** Add UI review of the user-facing layer ([b8aaf6dcfa](https://github.com/Jylhis/monorepo/commit/b8aaf6dcfa))
- Trim verbose and redundant code comments ([0038430c53](https://github.com/Jylhis/monorepo/commit/0038430c53))
- **nix:** Incremental checks, four-build matrix, opt-in ccache; remove jylhis fork and legacy GUI variants ([#215](https://github.com/Jylhis/monorepo/pull/215)) ([44157ff4ca](https://github.com/Jylhis/monorepo/commit/44157ff4ca))

### Removed

- **keys:** Remove and disable psychotherapist, games, mail, news menu items ([b050f655cc](https://github.com/Jylhis/monorepo/commit/b050f655cc))
- **tracking:** Drop wakatime/activity-watch, keep keyfreq in init-editing ([97267e71e3](https://github.com/Jylhis/monorepo/commit/97267e71e3))
- Prune triaged investigate list and completed plan items ([d2693f2490](https://github.com/Jylhis/monorepo/commit/d2693f2490))

### Fixed

- Fix cuda-ts-mode indent call for Emacs 31 ([a47b4b4533](https://github.com/Jylhis/monorepo/commit/a47b4b4533))
- Fix packages-doc-in-sync: keep auth-source @doc adjacent to its use-package ([9e0a97935a](https://github.com/Jylhis/monorepo/commit/9e0a97935a))
- **comment:** Jinx is configured in init-writing.el, not init-editing.el ([0322cd103d](https://github.com/Jylhis/monorepo/commit/0322cd103d))
- **elisp:** Declare vertico-mode and vertico-multiform-mode ([1e42111bbf](https://github.com/Jylhis/monorepo/commit/1e42111bbf))
- **elisp:** Declare auto-dark-mode and consult--customize-put ([bfcc722e85](https://github.com/Jylhis/monorepo/commit/bfcc722e85))
- **elisp:** Declare the two remaining split-compile references ([b0821b8770](https://github.com/Jylhis/monorepo/commit/b0821b8770))
- **elisp:** Clean-session fixes for the six remaining split-compile failures ([8fc3aefda8](https://github.com/Jylhis/monorepo/commit/8fc3aefda8))
- **elisp:** Declare jinx-mode in init-lang-data; --keep-going for the split CI step ([f0be1a36c8](https://github.com/Jylhis/monorepo/commit/f0be1a36c8))
- **elisp:** Add clean-session stubs the monolith batch compile masked ([8c0cb46b46](https://github.com/Jylhis/monorepo/commit/8c0cb46b46))
- **nav:** Use dirvish's native subtree for inline directory expansion ([9fff989492](https://github.com/Jylhis/monorepo/commit/9fff989492))
- **vc:** Pin diff-hl-dired-mode autoload to its own file ([006c3d1a9b](https://github.com/Jylhis/monorepo/commit/006c3d1a9b))
- Decouple API-doc generator from the core distribution build ([0dadb97d4b](https://github.com/Jylhis/monorepo/commit/0dadb97d4b))
- **ui:** Enable a theme in terminal frames when auto-dark can't detect ([d876e4d0ea](https://github.com/Jylhis/monorepo/commit/d876e4d0ea))
- **completion:** Silence zoxide "Process finished" echo on find-file ([de22d60a39](https://github.com/Jylhis/monorepo/commit/de22d60a39))
- **vc:** Keep transient @doc adjacent to its use-package form ([2d96c878fa](https://github.com/Jylhis/monorepo/commit/2d96c878fa))
- Refresh round-undecorated-frame.patch darwin hash ([0b9249c326](https://github.com/Jylhis/monorepo/commit/0b9249c326))

## 2026-07

### Added

- **scripts:** Bootstrap Jotain from apt on hosts without Nix ([9e2b7c8884](https://github.com/Jylhis/monorepo/commit/9e2b7c8884))
- **lang-nix:** Indent blank lines to the enclosing block level ([950933569c](https://github.com/Jylhis/monorepo/commit/950933569c))
- **just:** Add run-built-debug-log full-debug logging harness ([174b9b1f23](https://github.com/Jylhis/monorepo/commit/174b9b1f23))
- **devenv:** Prefer a project Nix LSP, ship nixd as fallback ([d5fd2202ec](https://github.com/Jylhis/monorepo/commit/d5fd2202ec))
- Add OCaml language support ([b0d72dea45](https://github.com/Jylhis/monorepo/commit/b0d72dea45))
- Add Emacs internals + Elisp knowledge base and AI skills ([3661be8fdd](https://github.com/Jylhis/monorepo/commit/3661be8fdd))
- Add AI screenshot capture: jotain-screenshot + just screenshot ([cdfc75b972](https://github.com/Jylhis/monorepo/commit/cdfc75b972))
- Add auto-activation trust (allow/revoke) and mode-line status ([d0027a7232](https://github.com/Jylhis/monorepo/commit/d0027a7232))
- Add native devenv.sh integration (devenv.el + init-devenv) ([4da598b4b7](https://github.com/Jylhis/monorepo/commit/4da598b4b7))

### Changed

- **init:** Never fetch package archives at startup ([3d3e2048b0](https://github.com/Jylhis/monorepo/commit/3d3e2048b0))
- **journal:** Flakes-vs-channels deep-research report ([5cc6f50ee8](https://github.com/Jylhis/monorepo/commit/5cc6f50ee8))
- **nix:** Default the Linux GUI build to pgtk ([e97c44520b](https://github.com/Jylhis/monorepo/commit/e97c44520b))
- Make CLAUDE.md, AGENTS.md, and the skills match the repo ([81fe2e1af8](https://github.com/Jylhis/monorepo/commit/81fe2e1af8))
- Fix stale user-doc claims; add README, LICENSE, and an ERT guide ([a7dc38c610](https://github.com/Jylhis/monorepo/commit/a7dc38c610))
- Place init-snippets at its load-order position in the package reference ([e0a3b182e5](https://github.com/Jylhis/monorepo/commit/e0a3b182e5))
- **tooling:** Bootstrap script and tracked Claude Code settings for Nix-less sessions ([a6f2a78e60](https://github.com/Jylhis/monorepo/commit/a6f2a78e60))
- **just:** Fail disabled stubs instead of exiting 0 ([2e7159b478](https://github.com/Jylhis/monorepo/commit/2e7159b478))
- Record the measured completion results ([5126c41592](https://github.com/Jylhis/monorepo/commit/5126c41592))
- **completion:** TAB indents, RET newlines, C-M-i completes ([cf208befbe](https://github.com/Jylhis/monorepo/commit/cf208befbe))
- **ui:** Reattach the doom-modeline @doc block to its use-package form ([f02dd712e6](https://github.com/Jylhis/monorepo/commit/f02dd712e6))
- **design:** Adopt jylhis/design v2 "The Survey" ([2d27344773](https://github.com/Jylhis/monorepo/commit/2d27344773))
- Research and design spec for completion and snippets ([76d2c5f8d9](https://github.com/Jylhis/monorepo/commit/76d2c5f8d9))
- Add completion/snippets research report (parts 1-4) ([50f614f754](https://github.com/Jylhis/monorepo/commit/50f614f754))
- **ui:** Gate doom-modeline icons on graphical frames ([513da74ef2](https://github.com/Jylhis/monorepo/commit/513da74ef2))
- **jinx:** Bundle aspell dicts so spell-check works out of the box ([f87d6bdb9a](https://github.com/Jylhis/monorepo/commit/f87d6bdb9a))
- **site:** Follow up the Emacs 31 pretest switch from main ([fb7f76330e](https://github.com/Jylhis/monorepo/commit/fb7f76330e))
- **site:** Give lispref makeinfo the doc/emacs include path ([fdfc0a1a1d](https://github.com/Jylhis/monorepo/commit/fdfc0a1a1d))
- **info-manual:** Create parent dirs before makeinfo --html ([b5eeedf5d0](https://github.com/Jylhis/monorepo/commit/b5eeedf5d0))
- **site:** Handle directory-shaped emacs.src for the manuals build ([1ac5c030be](https://github.com/Jylhis/monorepo/commit/1ac5c030be))
- **site:** Fix statix inherits and pass a real path as src ([542f8eb992](https://github.com/Jylhis/monorepo/commit/542f8eb992))
- **site:** Generate the docs site from every source the repo has ([3dd89f5d8b](https://github.com/Jylhis/monorepo/commit/3dd89f5d8b))
- **website:** Add landing page and docs site for docs.example.org ([60bec618af](https://github.com/Jylhis/monorepo/commit/60bec618af))
- Expose topical Jotain entries under the Emacs Info category ([59dd27d498](https://github.com/Jylhis/monorepo/commit/59dd27d498))
- **elisp:** Lift owner paths into jotain defcustoms ([53cea3802f](https://github.com/Jylhis/monorepo/commit/53cea3802f))
- **elisp:** Retire diminish; explicit cc-mode choice ([9322a278ec](https://github.com/Jylhis/monorepo/commit/9322a278ec))
- **elisp:** Dead config and convention cleanups ([f9b4d8e3c6](https://github.com/Jylhis/monorepo/commit/f9b4d8e3c6))
- **just/nix:** Offline system detection; drop allowUnfree asymmetry ([cad770ac9e](https://github.com/Jylhis/monorepo/commit/cad770ac9e))
- **repo:** Retire stale planning debris ([3990dc3bf6](https://github.com/Jylhis/monorepo/commit/3990dc3bf6))
- **devenv:** Correct @doc; regenerate package reference ([cf41ba40a8](https://github.com/Jylhis/monorepo/commit/cf41ba40a8))
- Align user-facing pages with the two launch modes ([48dbf3d3f5](https://github.com/Jylhis/monorepo/commit/48dbf3d3f5))
- Make CLAUDE.md match the repo's current reality ([229eb2a472](https://github.com/Jylhis/monorepo/commit/229eb2a472))
- **devenv/eca:** Subset google-fonts to Literata; cross-reference model lists ([366e418f7c](https://github.com/Jylhis/monorepo/commit/366e418f7c))
- **module:** Byte-compile the config for the HM deployment ([df99dd7dea](https://github.com/Jylhis/monorepo/commit/df99dd7dea))
- **checks:** Run all test files; harden scanner regexes ([7ad8d94df9](https://github.com/Jylhis/monorepo/commit/7ad8d94df9))
- **modules:** Satisfy the runtime-binary contract in every mode ([d8ef7dee69](https://github.com/Jylhis/monorepo/commit/d8ef7dee69))
- **just/flake:** Full nox distribution for aarch64; separate debug launch ([ed61044152](https://github.com/Jylhis/monorepo/commit/ed61044152))
- **keys:** Which-key label table and repeat-map cleanup ([4891a6fc39](https://github.com/Jylhis/monorepo/commit/4891a6fc39))
- **keys:** Move primary entry points into user-reserved C-c space ([6503a3c862](https://github.com/Jylhis/monorepo/commit/6503a3c862))
- **vc:** Drop obsolete diff-hl-magit-pre-refresh hook ([ddf5b12e8d](https://github.com/Jylhis/monorepo/commit/ddf5b12e8d))
- **early-init:** Quickstart invalidation; scaled native-comp jobs ([f00ec0fa3d](https://github.com/Jylhis/monorepo/commit/f00ec0fa3d))
- **startup:** Defer eagerly-loaded packages ([ccf57ce4f1](https://github.com/Jylhis/monorepo/commit/ccf57ce4f1))
- **packages:** Point ghostel trivialBuild at the elisp subdirectory ([9dab9fd510](https://github.com/Jylhis/monorepo/commit/9dab9fd510))
- **packages:** Build ghostel elisp-only while upstream zig fetch is broken ([7e02de7399](https://github.com/Jylhis/monorepo/commit/7e02de7399))
- **core:** GC discipline — startup restore, named hooks, guarded idle GC ([d6deb9ca63](https://github.com/Jylhis/monorepo/commit/d6deb9ca63))
- **devenv:** Cache trust state for the long TTL ([348dcb6a58](https://github.com/Jylhis/monorepo/commit/348dcb6a58))
- **snippets/writing/docs:** Capf teardown, jinx guard, Info additions ([737c2730ed](https://github.com/Jylhis/monorepo/commit/737c2730ed))
- **prog:** Tagref fallback guard, sonarlint eglot init, eglot config ([c77b40e47b](https://github.com/Jylhis/monorepo/commit/c77b40e47b))
- **vc:** Enable global-diff-hl-mode; unload doom-modeline from init path; misc-info git-stats ([9a70eaa126](https://github.com/Jylhis/monorepo/commit/9a70eaa126))
- **editing:** Guard whitespace-cleanup against super-save; drop dup newcomment ([775896a5d4](https://github.com/Jylhis/monorepo/commit/775896a5d4))
- Cache-parity story for the unstable default ([98e0d9c206](https://github.com/Jylhis/monorepo/commit/98e0d9c206))
- Default to the unstable (Emacs 31 pretest) variant ([47d691138e](https://github.com/Jylhis/monorepo/commit/47d691138e))
- **flake:** Compose emacs-overlay into every overlay consumer ([8a7aa0d871](https://github.com/Jylhis/monorepo/commit/8a7aa0d871))
- **flake:** Demote jylhis-emacs to non-flake input; resync locks ([0016a1e6ce](https://github.com/Jylhis/monorepo/commit/0016a1e6ce))
- Root-map lock nodes in parity snippet ([7befadcb31](https://github.com/Jylhis/monorepo/commit/7befadcb31))
- **just:** Resolve lock nodes through root input map ([bf40e64553](https://github.com/Jylhis/monorepo/commit/bf40e64553))
- Add 2026-07 Emacs+Nix deep-research review ([0eb250f05a](https://github.com/Jylhis/monorepo/commit/0eb250f05a))
- **completion:** Document Embark workflows and action-menu tips ([5d681997a6](https://github.com/Jylhis/monorepo/commit/5d681997a6))
- Match generated quoting in package-reference flymake entry ([aa04c2a3df](https://github.com/Jylhis/monorepo/commit/aa04c2a3df))
- **prog:** Show only most severe flymake diagnostic at end-of-line ([da857876a5](https://github.com/Jylhis/monorepo/commit/da857876a5))
- **writing:** Drive jinx from global-jinx-mode ([288a779682](https://github.com/Jylhis/monorepo/commit/288a779682))
- Refresh package-reference for new completion @doc markers ([09935e3b8b](https://github.com/Jylhis/monorepo/commit/09935e3b8b))
- **completion:** Apply cape learnings — TAB trigger and Elisp symbol capf ([574bd1563b](https://github.com/Jylhis/monorepo/commit/574bd1563b))
- Make the modern dev-workflow uniform across languages ([0ef59a50ec](https://github.com/Jylhis/monorepo/commit/0ef59a50ec))
- Refresh package archives only when stale, not every startup ([6d9d5e23e0](https://github.com/Jylhis/monorepo/commit/6d9d5e23e0))
- Update jylhis-emacs-themes pin to current design HEAD ([3e37aa141f](https://github.com/Jylhis/monorepo/commit/3e37aa141f))
- Add 2026-07 deep-research review of implementation and docs ([5b775e06a7](https://github.com/Jylhis/monorepo/commit/5b775e06a7))
- Pin x86_64-darwin to nixpkgs-26.05-darwin ([636b2a1fe3](https://github.com/Jylhis/monorepo/commit/636b2a1fe3))
- Refine Elisp reference notes per PR review ([31a07870d3](https://github.com/Jylhis/monorepo/commit/31a07870d3))
- Write screenshot data via write-region, not a temp buffer ([1bde5bb678](https://github.com/Jylhis/monorepo/commit/1bde5bb678))
- Drive project env via native devenv; stop go-mode hijacking file opens ([e3131e2999](https://github.com/Jylhis/monorepo/commit/e3131e2999))
- Stop custom.el writes from raising a supersession prompt ([53a2099423](https://github.com/Jylhis/monorepo/commit/53a2099423))
- Log when a classic major mode is used despite a ready ts-mode ([0b86cfa73e](https://github.com/Jylhis/monorepo/commit/0b86cfa73e))
- Auto-adopt devenv-provided language servers and overhaul Go tooling ([9da97ee25a](https://github.com/Jylhis/monorepo/commit/9da97ee25a))
- Address review: safe cache invalidation, envrc guard, MCP quoting ([611b4a35b4](https://github.com/Jylhis/monorepo/commit/611b4a35b4))
- Unify terminal setup around Ghostty via ghostel ([fb56298e71](https://github.com/Jylhis/monorepo/commit/fb56298e71))
- Address review: direct dape save hook, workspace config for go-work-ts-mode ([9fc78cf294](https://github.com/Jylhis/monorepo/commit/9fc78cf294))
- Upgrade Go developer experience: go-ts-mode, gopls tuning, dape, gotest ([46d0d162b2](https://github.com/Jylhis/monorepo/commit/46d0d162b2))
- Use mixed-pitch-mode for prose so Org tables stay aligned ([936cd250e6](https://github.com/Jylhis/monorepo/commit/936cd250e6))
- Hoist tempel eglot-merge guard out of use-package :init ([40c9a0bec5](https://github.com/Jylhis/monorepo/commit/40c9a0bec5))
- Dependency updates (4 commits)

### Removed

- Remove envrc ([98991938c3](https://github.com/Jylhis/monorepo/commit/98991938c3))
- Drop treesit-auto; route ts-modes via major-mode-remap-alist ([418a64b5d8](https://github.com/Jylhis/monorepo/commit/418a64b5d8))
- Drop duplicate goimports apheleia mapping (merge artifact) ([ff52eb873d](https://github.com/Jylhis/monorepo/commit/ff52eb873d))

### Fixed

- **devenv:** Load the shell env, not the derivation env ([4c8e2b9c6b](https://github.com/Jylhis/monorepo/commit/4c8e2b9c6b))
- **devenv:** Correct *devenv log* summary line count ([a13cfce4cc](https://github.com/Jylhis/monorepo/commit/a13cfce4cc))
- **completion:** Pin zoxide-executable across devenv exec-path swaps ([88f8ea7573](https://github.com/Jylhis/monorepo/commit/88f8ea7573))
- Fix eglot-tempel-mode void-function error on first eglot load ([90e5365b2b](https://github.com/Jylhis/monorepo/commit/90e5365b2b))

## 2026-06

### Added

- Add snippet tooling via Tempel ([3ad2e5106a](https://github.com/Jylhis/monorepo/commit/3ad2e5106a))
- Add Bazel/Starlark support ([e4234816cd](https://github.com/Jylhis/monorepo/commit/e4234816cd))
- Add Zig support (zig-ts-mode + zls + zig fmt) ([24c3c0f330](https://github.com/Jylhis/monorepo/commit/24c3c0f330))
- Adopt Emacs 31 quality-of-life built-ins from the "Emacs 31 Is Around the Corner" blog post ([1d09ea3d6f](https://github.com/Jylhis/monorepo/commit/1d09ea3d6f))
- Adopt a few built-ins from recent Emacs blog posts ([#176](https://github.com/Jylhis/monorepo/pull/176)) ([604db31f39](https://github.com/Jylhis/monorepo/commit/604db31f39))
- Add jotctl daemon-management command to Home Manager module ([ca2b3e44f0](https://github.com/Jylhis/monorepo/commit/ca2b3e44f0))
- Add opt-in lighter Emacs builds (curated grammars, trimmed bare) ([37b0d483bc](https://github.com/Jylhis/monorepo/commit/37b0d483bc))
- Add full Jujutsu (jj) VCS support ([381ea9f630](https://github.com/Jylhis/monorepo/commit/381ea9f630))
- Add newcomment defaults; drop duplicate isearch block ([c8eb40c1e2](https://github.com/Jylhis/monorepo/commit/c8eb40c1e2))
- Add rsync to Jotain runtime path ([#166](https://github.com/Jylhis/monorepo/pull/166)) ([ac1aa42e2f](https://github.com/Jylhis/monorepo/commit/ac1aa42e2f))
- Add nix-on-droid module and nixpkgs 24.05+ override compatibility ([8b4c6e17e7](https://github.com/Jylhis/monorepo/commit/8b4c6e17e7))
- **ai:** Curate OpenRouter model lineup by use case ([5dd1517953](https://github.com/Jylhis/monorepo/commit/5dd1517953))
- **ai:** Add OpenRouter support ([e8c723e078](https://github.com/Jylhis/monorepo/commit/e8c723e078))
- Add Meson file support ([11273c2515](https://github.com/Jylhis/monorepo/commit/11273c2515))

### Changed

- Address Codex review: nix-ts-mode, droid stateVersion, docs ([3e3b4dbf01](https://github.com/Jylhis/monorepo/commit/3e3b4dbf01))
- **templates:** Add non-treesit fallback mode headers and fix indentation ([fa84c78dcd](https://github.com/Jylhis/monorepo/commit/fa84c78dcd))
- Refresh package-reference for init-snippets @doc blocks ([2024eaae32](https://github.com/Jylhis/monorepo/commit/2024eaae32))
- Guard buildifier formatter against nil filepath ([355a78d6df](https://github.com/Jylhis/monorepo/commit/355a78d6df))
- Align cache-parity references to pkgs.emacs (emacs31 → emacs) ([#177](https://github.com/Jylhis/monorepo/pull/177)) ([78ddd69f48](https://github.com/Jylhis/monorepo/commit/78ddd69f48))
- Tune Emacs runtime perf for Intel Mac; document follow-ups ([bee6e87a8a](https://github.com/Jylhis/monorepo/commit/bee6e87a8a))
- Stop exporting broken jylhis package set ([#168](https://github.com/Jylhis/monorepo/pull/168)) ([7d740f1f97](https://github.com/Jylhis/monorepo/commit/7d740f1f97))
- Escape MDX angle brackets in generated package reference ([#169](https://github.com/Jylhis/monorepo/pull/169)) ([664d13b5c3](https://github.com/Jylhis/monorepo/commit/664d13b5c3))
- Guard missing Jylhis theme package ([#170](https://github.com/Jylhis/monorepo/pull/170)) ([e58993c8b8](https://github.com/Jylhis/monorepo/commit/e58993c8b8))
- Use upstream SonarLint package attr in module ([#171](https://github.com/Jylhis/monorepo/pull/171)) ([0707308359](https://github.com/Jylhis/monorepo/commit/0707308359))
- Update emacs package to latest version ([8c0c277753](https://github.com/Jylhis/monorepo/commit/8c0c277753))
- Sync package-reference with gptel @doc marker ([7a44de1a29](https://github.com/Jylhis/monorepo/commit/7a44de1a29))
- Realign emacs.nix bases with upstream nixpkgs/emacs-overlay ([f9d459337a](https://github.com/Jylhis/monorepo/commit/f9d459337a))
- Clarify font-scale customization docs ([b2518133f4](https://github.com/Jylhis/monorepo/commit/b2518133f4))
- **journal:** Document local integration of 12 GitHub PRs ([3835e50349](https://github.com/Jylhis/monorepo/commit/3835e50349))
- Harden .git parsing in git stats ([ba2c351b06](https://github.com/Jylhis/monorepo/commit/ba2c351b06))
- Quote Justfile path interpolations ([a7b045bb93](https://github.com/Jylhis/monorepo/commit/a7b045bb93))
- Harden Pages workflow ref and permissions ([cca706670a](https://github.com/Jylhis/monorepo/commit/cca706670a))
- Stop persisting ring contents in savehist ([a7addfb911](https://github.com/Jylhis/monorepo/commit/a7addfb911))
- Pin ECA server in modules ([0194b77923](https://github.com/Jylhis/monorepo/commit/0194b77923))
- Restrict Cachix auth to push events in CI ([a038235292](https://github.com/Jylhis/monorepo/commit/a038235292))
- Dependency updates (4 commits)

### Removed

- Drop stale -Dimagemagick Meson flag for jylhis-emacs ([90fb5af524](https://github.com/Jylhis/monorepo/commit/90fb5af524))

### Fixed

- Fix tempel template parsing and eglot capf shadowing ([2c481ab470](https://github.com/Jylhis/monorepo/commit/2c481ab470))
- Fix review findings: drop redundant save-some-buffers action, sync docs ([9add386a3d](https://github.com/Jylhis/monorepo/commit/9add386a3d))
- Fix review findings across Elisp, Nix, and docs ([c3b19e0fb1](https://github.com/Jylhis/monorepo/commit/c3b19e0fb1))
- Fix ispell ([36df249de4](https://github.com/Jylhis/monorepo/commit/36df249de4))
- Resolve standalone nixpkgs via root lock input ([c4e61784f0](https://github.com/Jylhis/monorepo/commit/c4e61784f0))

## 2026-05

### Added

- Add eca and tagref integrations ([afcb8f90eb](https://github.com/Jylhis/monorepo/commit/afcb8f90eb))
- Add jylhis Emacs backend for consumers ([30b3549947](https://github.com/Jylhis/monorepo/commit/30b3549947))
- Add shrink-window (v) to the window-resize repeat-map ([77128145ac](https://github.com/Jylhis/monorepo/commit/77128145ac))
- Add window-resize repeat-map and Ergonomics doc chapter ([9e23a16b6b](https://github.com/Jylhis/monorepo/commit/9e23a16b6b))
- Adopt small built-in defaults from Prot's "decent defaults" ([e06f4254c7](https://github.com/Jylhis/monorepo/commit/e06f4254c7))
- Add which-key prefix labels and a Keybindings reference page ([ca146f3fac](https://github.com/Jylhis/monorepo/commit/ca146f3fac))
- Add newcomment defaults and C-c ; binding ([91143b3cfd](https://github.com/Jylhis/monorepo/commit/91143b3cfd))
- Add async dired copies and dired-rsync ([5d9d15d012](https://github.com/Jylhis/monorepo/commit/5d9d15d012))
- Add daemon/client launch ergonomics and -Q quick-edit recipe ([757fe5093c](https://github.com/Jylhis/monorepo/commit/757fe5093c))
- Add git-stats counters to the doom-modeline ([5c22ba0ffe](https://github.com/Jylhis/monorepo/commit/5c22ba0ffe))
- Add docker TUI and podman/docker backend switch ([2e1ff8a79d](https://github.com/Jylhis/monorepo/commit/2e1ff8a79d))
- Add Unicode emoji rendering and emoji.el picker ([826fc6198b](https://github.com/Jylhis/monorepo/commit/826fc6198b))
- Add jotain-switch-git-status-buffer for quick changed-file jumps ([6d76707e2c](https://github.com/Jylhis/monorepo/commit/6d76707e2c))
- Enable Org Babel evaluation confirmation ([a4fdab6786](https://github.com/Jylhis/monorepo/commit/a4fdab6786))
- Add jylhis Emacs package ([d69ad5f2b7](https://github.com/Jylhis/monorepo/commit/d69ad5f2b7))
- Adopt upstream hl-todo per-keyword faces from design system ([08187d50b4](https://github.com/Jylhis/monorepo/commit/08187d50b4))

### Changed

- Update Nix input pins ([7756ba597b](https://github.com/Jylhis/monorepo/commit/7756ba597b))
- Track jylhis/emacs dev branch as a flake input ([35b3b71f67](https://github.com/Jylhis/monorepo/commit/35b3b71f67))
- Stop jotain-git-stats racing the user's foreground git ops ([190e042de0](https://github.com/Jylhis/monorepo/commit/190e042de0))
- Disable Emacs in the devenv shell, move provenance to flake check ([7a5d9c94cf](https://github.com/Jylhis/monorepo/commit/7a5d9c94cf))
- Split PR check job into per-check steps for diagnostics ([4c2b64c785](https://github.com/Jylhis/monorepo/commit/4c2b64c785))
- Split CI: lean PR checks, deploy-on-main workflow ([db33f78fba](https://github.com/Jylhis/monorepo/commit/db33f78fba))
- Update lock ([3183372a06](https://github.com/Jylhis/monorepo/commit/3183372a06))
- Apply suggestions from code review ([e37a5ec644](https://github.com/Jylhis/monorepo/commit/e37a5ec644))
- Reorder DWIM quit so M-x TAB C-g still aborts in one keystroke ([b18fc28994](https://github.com/Jylhis/monorepo/commit/b18fc28994))
- Update lock ([89af8fda45](https://github.com/Jylhis/monorepo/commit/89af8fda45))
- Wire emacs-overlay for Emacs 31 base package ([6d89586af6](https://github.com/Jylhis/monorepo/commit/6d89586af6))
- Disable text-mode prose features in YAML buffers ([27b716c946](https://github.com/Jylhis/monorepo/commit/27b716c946))
- Update lock ([9209512e8e](https://github.com/Jylhis/monorepo/commit/9209512e8e))
- Align docs with Emacs 31 baseline ([7f7ebb0267](https://github.com/Jylhis/monorepo/commit/7f7ebb0267))
- Address PR review on yaml prog-mode hook ([77805706a5](https://github.com/Jylhis/monorepo/commit/77805706a5))
- Refresh package-reference.mdx for yaml-mode doc change ([5c352f30de](https://github.com/Jylhis/monorepo/commit/5c352f30de))
- Run prog-mode-hook in YAML buffers ([a85b032ed5](https://github.com/Jylhis/monorepo/commit/a85b032ed5))
- Gate risky JS LSP companions behind opt-in ([ef72b0015a](https://github.com/Jylhis/monorepo/commit/ef72b0015a))
- Harden git-stats subprocess invocation ([801db59b77](https://github.com/Jylhis/monorepo/commit/801db59b77))
- **init-ui:** Prepend (not append) emoji font for symbol charset ([4ca83c09eb](https://github.com/Jylhis/monorepo/commit/4ca83c09eb))
- Use --numstat for locale-independent change counting ([f074ea7f7c](https://github.com/Jylhis/monorepo/commit/f074ea7f7c))
- Refine jotain-keyboard-quit-dwim branch order ([e17cc34b17](https://github.com/Jylhis/monorepo/commit/e17cc34b17))
- Refresh package-reference.mdx for new transpose @doc block ([f66c8ea924](https://github.com/Jylhis/monorepo/commit/f66c8ea924))
- Bind transpose-sentences/paragraphs and combobulate drag commands ([7591e47ce5](https://github.com/Jylhis/monorepo/commit/7591e47ce5))
- Sync package-reference.mdx, clarify dired @doc ([c0f77e1a14](https://github.com/Jylhis/monorepo/commit/c0f77e1a14))
- Bind Emacs 30 replace-regexp-as-diff family ([b21b8829ae](https://github.com/Jylhis/monorepo/commit/b21b8829ae))
- Remap keyboard-quit instead of binding C-g directly ([ddd9bf87ec](https://github.com/Jylhis/monorepo/commit/ddd9bf87ec))
- Refresh package-reference.mdx to match new @doc markers ([0b46ed8f15](https://github.com/Jylhis/monorepo/commit/0b46ed8f15))
- Upgrade Emacs package defaults to 31 ([b88435a0a0](https://github.com/Jylhis/monorepo/commit/b88435a0a0))
- Key project completion and tab reuse on full path ([39724f9046](https://github.com/Jylhis/monorepo/commit/39724f9046))
- Refresh package-reference and tighten newcomment @doc ([fbabb28213](https://github.com/Jylhis/monorepo/commit/fbabb28213))
- Address review feedback and refresh package reference ([8a2f407c6b](https://github.com/Jylhis/monorepo/commit/8a2f407c6b))
- Close gaps from "Stealing from the Best Emacs Configs" ([76cfc14189](https://github.com/Jylhis/monorepo/commit/76cfc14189))
- Refresh package-reference.mdx; address review feedback ([b5c172334c](https://github.com/Jylhis/monorepo/commit/b5c172334c))
- Update lock ([f17be27284](https://github.com/Jylhis/monorepo/commit/f17be27284))
- Tighten init-core summary, clarify interactive-form syntax ([a6ef4b07aa](https://github.com/Jylhis/monorepo/commit/a6ef4b07aa))
- Describe init-core M-x predicate filter ([57323c5659](https://github.com/Jylhis/monorepo/commit/57323c5659))
- Sync package-reference.mdx with new @doc markers ([e75da68eac](https://github.com/Jylhis/monorepo/commit/e75da68eac))
- Harden editor wrappers against option injection ([46f6efbce1](https://github.com/Jylhis/monorepo/commit/46f6efbce1))
- Declare profiler-* functions for byte-compile ([e6489ae28e](https://github.com/Jylhis/monorepo/commit/e6489ae28e))
- Preserve --init-directory in emacsclient fallback path ([bea1e39fbc](https://github.com/Jylhis/monorepo/commit/bea1e39fbc))
- Sync package-reference.mdx with new devops doc blocks ([8195be1136](https://github.com/Jylhis/monorepo/commit/8195be1136))
- Declare programs.* in module-eval stub for shellAliases ([5ec68a70b7](https://github.com/Jylhis/monorepo/commit/5ec68a70b7))
- Harden jotain-switch-git-status-buffer parser; refresh package ref ([570130c607](https://github.com/Jylhis/monorepo/commit/570130c607))
- Refresh package-reference for new emoji entry ([798edeeb4a](https://github.com/Jylhis/monorepo/commit/798edeeb4a))
- Make rass entries resilient and add typescript-mode ([9b8dbdfde5](https://github.com/Jylhis/monorepo/commit/9b8dbdfde5))
- Borrow four ideas from LEmacs ([c6f8ede88e](https://github.com/Jylhis/monorepo/commit/c6f8ede88e))
- Address CI byte-compile + gemini-code-assist review ([8678a6248e](https://github.com/Jylhis/monorepo/commit/8678a6248e))
- **init-ui:** Drop subr-x when-let* for plain let/when ([e88073b007](https://github.com/Jylhis/monorepo/commit/e88073b007))
- Route TS/TSX and Python eglot through rassumfrassum ([9fc2f2823b](https://github.com/Jylhis/monorepo/commit/9fc2f2823b))
- Update lock ([e143ce3a12](https://github.com/Jylhis/monorepo/commit/e143ce3a12))
- Address PR review: shared helpers, multi-line @doc, root-relative link ([0d70b3c979](https://github.com/Jylhis/monorepo/commit/0d70b3c979))
- Document use-package blocks and generate package reference ([c5abb97de6](https://github.com/Jylhis/monorepo/commit/c5abb97de6))
- Update Jylhis design pin ([9f6ab48eec](https://github.com/Jylhis/monorepo/commit/9f6ab48eec))
- Update lock ([c97080a1c8](https://github.com/Jylhis/monorepo/commit/c97080a1c8))
- Update lock ([a286aa30f6](https://github.com/Jylhis/monorepo/commit/a286aa30f6))
- Switch to Jylhis design system themes (paper/roast) ([b237baf963](https://github.com/Jylhis/monorepo/commit/b237baf963))
- Dependency updates (2 commits)

### Fixed

- Fix jylhis Emacs Meson build ([e8d755677e](https://github.com/Jylhis/monorepo/commit/e8d755677e))
- Fix jotain daemon crash from void vc-git-root at startup ([98dc61f381](https://github.com/Jylhis/monorepo/commit/98dc61f381))
- Resolve dockerfile LSP from upstream nixpkgs, drop overlay alias ([5a909ac1eb](https://github.com/Jylhis/monorepo/commit/5a909ac1eb))
- Fix rename path order, drop D, force untracked listing ([9270eba2e4](https://github.com/Jylhis/monorepo/commit/9270eba2e4))
- Fix Keybindings doc namespace categorisation ([6f39e0849e](https://github.com/Jylhis/monorepo/commit/6f39e0849e))
- Fix byte-compile warnings and apply review feedback ([03daba5825](https://github.com/Jylhis/monorepo/commit/03daba5825))
- Fix CI: forward-declare backend var, drop deprecated alias ([b49e774258](https://github.com/Jylhis/monorepo/commit/b49e774258))
- Fix envrc blocked-popup suppression missing allow-no-window ([b78293b862](https://github.com/Jylhis/monorepo/commit/b78293b862))
- Fix packages-doc generator: skip empty modules in body join ([495531a2d6](https://github.com/Jylhis/monorepo/commit/495531a2d6))
- Fix string-context error from baseNameOf in scanDirectoryWithDoc ([cc35c898c1](https://github.com/Jylhis/monorepo/commit/cc35c898c1))

## 2026-04

### Added

- Add repository contributor guide ([74e4844fd8](https://github.com/Jylhis/monorepo/commit/74e4844fd8))
- Add todo ([b6dd6059b3](https://github.com/Jylhis/monorepo/commit/b6dd6059b3))
- Add SonarCloud support via SonarLint language server ([da0054686c](https://github.com/Jylhis/monorepo/commit/da0054686c))
- Add CodeQL analysis workflow configuration ([927a3846e1](https://github.com/Jylhis/monorepo/commit/927a3846e1))
- Add module option docs with nixosOptionsDoc and GitHub Pages deployment ([cad1be5452](https://github.com/Jylhis/monorepo/commit/cad1be5452))
- Add compilation mode guide documenting build/error navigation workflow ([17e7e84ede](https://github.com/Jylhis/monorepo/commit/17e7e84ede))
- Enable visual-wrap-prefix-mode for text-mode buffers ([c56e3844ef](https://github.com/Jylhis/monorepo/commit/c56e3844ef))
- Add per-system packages output to flake.nix ([bcb2750b43](https://github.com/Jylhis/monorepo/commit/bcb2750b43))
- Add statix.toml and update CLAUDE.md for hybrid flake setup ([a99ea83719](https://github.com/Jylhis/monorepo/commit/a99ea83719))
- Add statix and deadnix to dev shell for Nix linting ([320be40459](https://github.com/Jylhis/monorepo/commit/320be40459))
- Add statix.toml to suppress false positives on idiomatic module patterns ([eec770bc22](https://github.com/Jylhis/monorepo/commit/eec770bc22))
- Add startup benchmark recipe to Justfile ([d6b99531b9](https://github.com/Jylhis/monorepo/commit/d6b99531b9))
- Import Mintlify docs from next and update for current project state ([c5ea9e493a](https://github.com/Jylhis/monorepo/commit/c5ea9e493a))
- Add home-manager module for jotain daemon and emacsclient ([6448a0428f](https://github.com/Jylhis/monorepo/commit/6448a0428f))
- Add Emacs state files to .gitignore ([0f6f814039](https://github.com/Jylhis/monorepo/commit/0f6f814039))
- Add display-performance and ergonomic settings ([fcce6e3f9c](https://github.com/Jylhis/monorepo/commit/fcce6e3f9c))
- Add package-quickstart.el to .gitignore ([ac0fc21f80](https://github.com/Jylhis/monorepo/commit/ac0fc21f80))
- Add devenv emacs-lisp language module with ellsp and elsa disabled ([5673c54ea3](https://github.com/Jylhis/monorepo/commit/5673c54ea3))
- Add Justfile for check, run, fmt, pins, and clean ([927171afba](https://github.com/Jylhis/monorepo/commit/927171afba))
- Add Ghostty terminal compatibility ([e8a076f5cc](https://github.com/Jylhis/monorepo/commit/e8a076f5cc))
- Add font preferences with fallback and enhance consult configuration ([de75270e1e](https://github.com/Jylhis/monorepo/commit/de75270e1e))
- Add editor instrumentation for self-tracking stack ([3cc0215e80](https://github.com/Jylhis/monorepo/commit/3cc0215e80))
- Initial version control docs ([3f458bc0fb](https://github.com/Jylhis/monorepo/commit/3f458bc0fb))
- Add magit ([c5db0b404a](https://github.com/Jylhis/monorepo/commit/c5db0b404a))
- Initial tests ([acab458696](https://github.com/Jylhis/monorepo/commit/acab458696))
- Add journal ([e3481a45ac](https://github.com/Jylhis/monorepo/commit/e3481a45ac))
- Add python for some claude scripts ([5bab7fdc4f](https://github.com/Jylhis/monorepo/commit/5bab7fdc4f))
- Add Tier 3 supporting agents, skill, and hook ([6d460d7e08](https://github.com/Jylhis/monorepo/commit/6d460d7e08))
- Add Tier 2 agents for workflow support ([42a32addb8](https://github.com/Jylhis/monorepo/commit/42a32addb8))
- Add Tier 1 skills, agents, and lint hook ([47f8d1ac63](https://github.com/Jylhis/monorepo/commit/47f8d1ac63))
- Add emacs-researcher agent for deep Emacs investigation ([86bc11b0a0](https://github.com/Jylhis/monorepo/commit/86bc11b0a0))
- Add PostHog telemetry integration for Emacs usage tracking ([efafb5e6ed](https://github.com/Jylhis/monorepo/commit/efafb5e6ed))
- Add lsp and elsa options to devenv emacs-lisp module ([20b24bc458](https://github.com/Jylhis/monorepo/commit/20b24bc458))
- Add wrapped Elsa CLI using emacsPackages.elsa ([ea86488a6c](https://github.com/Jylhis/monorepo/commit/ea86488a6c))
- Add Nix derivation for ellsp language server ([13e8724ddf](https://github.com/Jylhis/monorepo/commit/13e8724ddf))
- Add all cached emacs variant targets to Justfile ([ea80fa5ec1](https://github.com/Jylhis/monorepo/commit/ea80fa5ec1))
- Add custom.el to .gitignore ([c99fb02e54](https://github.com/Jylhis/monorepo/commit/c99fb02e54))
- Add distribution layer with all tree-sitter grammars ([16f3d8171a](https://github.com/Jylhis/monorepo/commit/16f3d8171a))
- Add NixOS Emacs wiki link to inspiration docs ([8f5561e9bb](https://github.com/Jylhis/monorepo/commit/8f5561e9bb))
- Add transient/ to .gitignore ([332a970e63](https://github.com/Jylhis/monorepo/commit/332a970e63))
- Add CLAUDE.md with project overview and key commands ([12656329f1](https://github.com/Jylhis/monorepo/commit/12656329f1))
- Add docs: reference guide for setting variables in Emacs Lisp ([8543073ed5](https://github.com/Jylhis/monorepo/commit/8543073ed5))
- Add early-init.el with toolbar, startup screen, and use-package settings ([b12310f6b3](https://github.com/Jylhis/monorepo/commit/b12310f6b3))

### Changed

- Pin nix-ts-mode to Nix to prevent MELPA/grammar version conflicts ([3af947b2f0](https://github.com/Jylhis/monorepo/commit/3af947b2f0))
- Split EDITOR/VISUAL and default to emacsclient ([75f43b9309](https://github.com/Jylhis/monorepo/commit/75f43b9309))
- Build all flake outputs on all four platforms in CI ([9df97fcee1](https://github.com/Jylhis/monorepo/commit/9df97fcee1))
- Update lock ([fd209ce1fb](https://github.com/Jylhis/monorepo/commit/fd209ce1fb))
- Convert orphaned .md drafts to .draft.mdx ([5f9f3a59f8](https://github.com/Jylhis/monorepo/commit/5f9f3a59f8))
- Optimize CI performance and cost ([836eef9f4b](https://github.com/Jylhis/monorepo/commit/836eef9f4b))
- Change package ecosystem to 'nix' in dependabot config ([e2dfb96377](https://github.com/Jylhis/monorepo/commit/e2dfb96377))
- Configure variable-pitch face so emacsclient frames get proper font size ([f3b392e232](https://github.com/Jylhis/monorepo/commit/f3b392e232))
- Enhance dired and guarantee folders-first sort on BSD ls ([696ae6f44c](https://github.com/Jylhis/monorepo/commit/696ae6f44c))
- Bundle docs as jotain.info and wire into Emacs help system ([0387ab4b6a](https://github.com/Jylhis/monorepo/commit/0387ab4b6a))
- Ensure rg and friends are on PATH regardless of launch context ([f17d1eed6b](https://github.com/Jylhis/monorepo/commit/f17d1eed6b))
- Update lock ([bc5c61a490](https://github.com/Jylhis/monorepo/commit/bc5c61a490))
- ⚡ Refactor test-use-package-ensure.el optimization to use let* ([d81c287e47](https://github.com/Jylhis/monorepo/commit/d81c287e47))
- Broaden just update/verify and remove stale npins references ([ab550f2b29](https://github.com/Jylhis/monorepo/commit/ab550f2b29))
- Extract benchmark Elisp into bench/{early-init,init}.el ([31117f52ae](https://github.com/Jylhis/monorepo/commit/31117f52ae))
- Replace devenv workflow with comprehensive CI ([09f4e2933c](https://github.com/Jylhis/monorepo/commit/09f4e2933c))
- Update CLAUDE.md to reflect flake-compat migration ([c34581d0f6](https://github.com/Jylhis/monorepo/commit/c34581d0f6))
- Expose lib in flake outputs; fix module.nix default package reference ([ff83c97e1e](https://github.com/Jylhis/monorepo/commit/ff83c97e1e))
- Simplify config: remove dead code, fix naming, deduplicate devenv ([1d9909d7dd](https://github.com/Jylhis/monorepo/commit/1d9909d7dd))
- Suppress envrc error buffer popup for blocked directories ([4bbae08deb](https://github.com/Jylhis/monorepo/commit/4bbae08deb))
- **dired:** Cache executable-find result in defvar ([5e9d15459c](https://github.com/Jylhis/monorepo/commit/5e9d15459c))
- Migrate to hybrid non-flake + flake setup with synchronized nixpkgs pinning ([e84e562a70](https://github.com/Jylhis/monorepo/commit/e84e562a70))
- Optimize startup from ~10s to ~3s by deferring eager loads ([6a1c29bfca](https://github.com/Jylhis/monorepo/commit/6a1c29bfca))
- Update gitignore ([25ddbabae1](https://github.com/Jylhis/monorepo/commit/25ddbabae1))
- **nix:** Extract duplicated extraPackages to nix/extra-packages.nix ([6e7523a22c](https://github.com/Jylhis/monorepo/commit/6e7523a22c))
- **module.nix:** Install Jotain config and pin --init-directory ([5b09b5368a](https://github.com/Jylhis/monorepo/commit/5b09b5368a))
- Move home-manager module from nix/jotain-service.nix to module.nix ([9956f90801](https://github.com/Jylhis/monorepo/commit/9956f90801))
- **jotain-service:** Install cfg.package into home.packages ([9167537e00](https://github.com/Jylhis/monorepo/commit/9167537e00))
- Auto-map use-package forms to emacsPackages via nix/use-package.nix ([e09dc9304f](https://github.com/Jylhis/monorepo/commit/e09dc9304f))
- Init CLAUDE.md ([debd796e07](https://github.com/Jylhis/monorepo/commit/debd796e07))
- **devenv:** Fix enterTest isolation assertion and drop unused var ([0457dd6b67](https://github.com/Jylhis/monorepo/commit/0457dd6b67))
- Use local emacs.nix as the devenv emacs (cache-parity verified) ([53f81f7906](https://github.com/Jylhis/monorepo/commit/53f81f7906))
- Bring Emacs source build over from next branch ([718c966d75](https://github.com/Jylhis/monorepo/commit/718c966d75))
- Initialise Jotain Emacs configuration ([af75bd29bb](https://github.com/Jylhis/monorepo/commit/af75bd29bb))
- Disable jotain-telemetry and rewrite 04-07 journal ([30f472fe0f](https://github.com/Jylhis/monorepo/commit/30f472fe0f))
- Improve Mintlify docs: add Guides nav group, favicon, convert orphan pages to mdx ([ed87bb6137](https://github.com/Jylhis/monorepo/commit/ed87bb6137))
- Move Mintlify docs under docs/ directory ([cc3d3c3a62](https://github.com/Jylhis/monorepo/commit/cc3d3c3a62))
- Initialize Mintlify docs structure ([1cf7abe952](https://github.com/Jylhis/monorepo/commit/1cf7abe952))
- Switch to nix-ts-mode and install markdown-mode ([00dc58b7fc](https://github.com/Jylhis/monorepo/commit/00dc58b7fc))
- Update lock ([6b5c4d23b4](https://github.com/Jylhis/monorepo/commit/6b5c4d23b4))
- Refactor telemetry: add debug logging, fix PostHog serialization, move to lisp/ ([7cd866a879](https://github.com/Jylhis/monorepo/commit/7cd866a879))
- Set default PostHog API key for telemetry ([4220ef9c30](https://github.com/Jylhis/monorepo/commit/4220ef9c30))
- Disble lsp and elsa ([75e8338539](https://github.com/Jylhis/monorepo/commit/75e8338539))
- Build ellsp from source with bundled Emacs ([76fc92cee6](https://github.com/Jylhis/monorepo/commit/76fc92cee6))
- Update journal and CLAUDE.md with ellsp/elsa work ([82f371ad3b](https://github.com/Jylhis/monorepo/commit/82f371ad3b))
- Update journal ([09dd92331e](https://github.com/Jylhis/monorepo/commit/09dd92331e))
- Change theme ([5f29aeb512](https://github.com/Jylhis/monorepo/commit/5f29aeb512))
- Improve debuggability ([2663233be8](https://github.com/Jylhis/monorepo/commit/2663233be8))
- Update clean task ([eb257399c7](https://github.com/Jylhis/monorepo/commit/eb257399c7))
- Update journal 2026-04-02: tree-sitter grammars, setopt, docs ([9212cbb47f](https://github.com/Jylhis/monorepo/commit/9212cbb47f))
- Update CLAUDE.md for emacs.nix/default.nix split ([282e8a3a21](https://github.com/Jylhis/monorepo/commit/282e8a3a21))
- Rename default.nix to emacs.nix ([65543f8def](https://github.com/Jylhis/monorepo/commit/65543f8def))
- Use setopt for defcustom variables in early-init.el ([863accbe5d](https://github.com/Jylhis/monorepo/commit/863accbe5d))
- Update journal 2026-04-02: vanilla Emacs setup, theme, docs ([141d5fa806](https://github.com/Jylhis/monorepo/commit/141d5fa806))
- Configure init.el: custom-file, modus theme, core settings, nix-mode ([bfa9b2c011](https://github.com/Jylhis/monorepo/commit/bfa9b2c011))
- Multiplatform run ([e35c98a5d9](https://github.com/Jylhis/monorepo/commit/e35c98a5d9))
- Devenv lock ([0e893df228](https://github.com/Jylhis/monorepo/commit/0e893df228))
- Move docs ([a75db77e4f](https://github.com/Jylhis/monorepo/commit/a75db77e4f))
- Update .gitignore ([4737c5d7ba](https://github.com/Jylhis/monorepo/commit/4737c5d7ba))
- Update journal ([26117264ee](https://github.com/Jylhis/monorepo/commit/26117264ee))
- Make modules self-contained: decouple from overlay, add emacsPackage option ([21e8852332](https://github.com/Jylhis/monorepo/commit/21e8852332))
- Dependency updates (2 commits)

### Removed

- Remove CodeQL advanced workflow; use default setup instead ([ea3f26c324](https://github.com/Jylhis/monorepo/commit/ea3f26c324))
- Drop redundant top-level make-directory in init-core ([e9b1140919](https://github.com/Jylhis/monorepo/commit/e9b1140919))
- Remove redundant config and deduplicate Nix package definitions ([d3e267b3ed](https://github.com/Jylhis/monorepo/commit/d3e267b3ed))
- Remove unused keybindings ([726a5e38cb](https://github.com/Jylhis/monorepo/commit/726a5e38cb))
- **devenv:** Drop unused lib module arg ([49b771c28c](https://github.com/Jylhis/monorepo/commit/49b771c28c))
- Remove stray .tmp file accidentally committed in 5673c54 ([1bca53551d](https://github.com/Jylhis/monorepo/commit/1bca53551d))

### Fixed

- Fix Emacs config review findings ([77239cacc1](https://github.com/Jylhis/monorepo/commit/77239cacc1))
- Fix 8x file-open slowdown from global-treesit-auto-mode ([91f3867296](https://github.com/Jylhis/monorepo/commit/91f3867296))
- Fix stale references and gaps in CLAUDE.md ([803705ac4a](https://github.com/Jylhis/monorepo/commit/803705ac4a))
- Fix all nix flake check failures ([010916b9b8](https://github.com/Jylhis/monorepo/commit/010916b9b8))
- Fix inverted auto-dark theme order on macOS ([fcacbc8f0a](https://github.com/Jylhis/monorepo/commit/fcacbc8f0a))
- Fix compilation-first-column and compilation-finish-functions examples ([a389911edb](https://github.com/Jylhis/monorepo/commit/a389911edb))
- **core:** Remove redundant module-level make-directory call ([b107b1493b](https://github.com/Jylhis/monorepo/commit/b107b1493b))
- Fix dired error on macOS when GNU ls is unavailable ([8f650adf4d](https://github.com/Jylhis/monorepo/commit/8f650adf4d))
- Fix auto-dark theme toggling in terminal Emacs on macOS ([abfaa101a5](https://github.com/Jylhis/monorepo/commit/abfaa101a5))
- Fix missing nerd-icons in dired by using installed Nerd Font ([1e178c2322](https://github.com/Jylhis/monorepo/commit/1e178c2322))
- **module:** Add platform guards for systemd and launchd blocks ([afe0e211c6](https://github.com/Jylhis/monorepo/commit/afe0e211c6))
- Fix package-quickstart ergonomics and replace no-littering ([1fb9f6f191](https://github.com/Jylhis/monorepo/commit/1fb9f6f191))
- Fix clean-install bootstrap: refresh archives, drop :vc fallbacks ([3a1fbf683b](https://github.com/Jylhis/monorepo/commit/3a1fbf683b))
- Fix startup errors: packages, tree-sitter grammars, flymake, wakatime ([77c20bd923](https://github.com/Jylhis/monorepo/commit/77c20bd923))
- Fix consult autoload error by removing deleted preview-at-point hook ([7045bebb59](https://github.com/Jylhis/monorepo/commit/7045bebb59))
- Fix void-variable package-archives on startup ([3728eb3f95](https://github.com/Jylhis/monorepo/commit/3728eb3f95))
- Fix org-clock and org-clock-csv loading ([805e269649](https://github.com/Jylhis/monorepo/commit/805e269649))
- Fix test isolation: neutralize POSTHOG_API_KEY in clean-state macro ([5790e6c7d6](https://github.com/Jylhis/monorepo/commit/5790e6c7d6))
- Fix macport variant build by setting srcRepo = true ([43ae20adde](https://github.com/Jylhis/monorepo/commit/43ae20adde))
- Fix Darwin defaults ([d2d22ebf0e](https://github.com/Jylhis/monorepo/commit/d2d22ebf0e))
- Fix .gitignore: add trailing slash to eln-cache, add elpa/, fix missing newline ([84b1cb377b](https://github.com/Jylhis/monorepo/commit/84b1cb377b))

## 2026-03

### Added

- Add macOS support via darwin-emacs and cross-platform systems ([8f4a8f0107](https://github.com/Jylhis/monorepo/commit/8f4a8f0107))
- Add Mintlify documentation site ([636e9c34dd](https://github.com/Jylhis/monorepo/commit/636e9c34dd))
- Add jotain-defaults and jotain-platform modules ([c742a0cb33](https://github.com/Jylhis/monorepo/commit/c742a0cb33))
- Strip elisp/ to empty skeleton for rewrite ([875f963ba2](https://github.com/Jylhis/monorepo/commit/875f963ba2))
- Automatically create missing directories ([973b276f73](https://github.com/Jylhis/monorepo/commit/973b276f73))
- Automatically create missing directories ([99492a1d41](https://github.com/Jylhis/monorepo/commit/99492a1d41))
- Migrate dev shell from shell.nix to devenv.sh ([c157d0d6f6](https://github.com/Jylhis/monorepo/commit/c157d0d6f6))
- Add React support via typescript-ts-mode ([8fd7c698b0](https://github.com/Jylhis/monorepo/commit/8fd7c698b0))
- Add support for Vue via vue-mode and vue-language-server ([ad5564e93b](https://github.com/Jylhis/monorepo/commit/ad5564e93b))
- Add support for Vue via vue-mode and vue-language-server ([a68ade52f0](https://github.com/Jylhis/monorepo/commit/a68ade52f0))
- Add speckit.fix, speckit-workflow, and two auto-triggered skills ([004a863a50](https://github.com/Jylhis/monorepo/commit/004a863a50))
- Implement 003 bleeding edge IDE config ([c9cdc9c7e8](https://github.com/Jylhis/monorepo/commit/c9cdc9c7e8))
- Add .editorconfig and .dir-locals.el ([acca62a737](https://github.com/Jylhis/monorepo/commit/acca62a737))
- Add pre-merge quality gate to worktrunk config ([504569c3d6](https://github.com/Jylhis/monorepo/commit/504569c3d6))
- Add worktrunk project config with beads redirect hook ([2429571179](https://github.com/Jylhis/monorepo/commit/2429571179))
- Configure PDF viewing with pdf-tools ([0f79d2beaf](https://github.com/Jylhis/monorepo/commit/0f79d2beaf))

### Changed

- **Simplify:** Remove dead code, fix indentation, guard darwin override ([89b55572dc](https://github.com/Jylhis/monorepo/commit/89b55572dc))
- Disable emacs-lsp-booster to fix darwin build ([ebcdc72d8b](https://github.com/Jylhis/monorepo/commit/ebcdc72d8b))
- Address PR #97 review feedback ([7a556c13b5](https://github.com/Jylhis/monorepo/commit/7a556c13b5))
- Refactor Emacs configuration in default.nix ([0282d33e29](https://github.com/Jylhis/monorepo/commit/0282d33e29))
- Refactor fontconfig configuration to use optionalAttrs ([3c5bab29ab](https://github.com/Jylhis/monorepo/commit/3c5bab29ab))
- Refactor Emacs configuration and runtime dependencies ([f8d80b8051](https://github.com/Jylhis/monorepo/commit/f8d80b8051))
- Refactor default.nix to enhance font configuration ([86fa385cc5](https://github.com/Jylhis/monorepo/commit/86fa385cc5))
- Replace `setq` with `setopt` for customizable variables ([400c0343b2](https://github.com/Jylhis/monorepo/commit/400c0343b2))
- Update Emacs fallback to unstable-pgtk variant ([41a468d228](https://github.com/Jylhis/monorepo/commit/41a468d228))
- 🎨 Palette: [UX improvement] Improve buffer names for identical files ([85472247cd](https://github.com/Jylhis/monorepo/commit/85472247cd))
- Optimize Flymake diagnostic display by using Eldoc (Duplicate PR acknowledged) ([2c23a87378](https://github.com/Jylhis/monorepo/commit/2c23a87378))
- **fonts:** Size hash table dynamically from font-family-list result ([4345b182e4](https://github.com/Jylhis/monorepo/commit/4345b182e4))
- Refactor font caching to use O(1) hash table lookups ([e6f411439a](https://github.com/Jylhis/monorepo/commit/e6f411439a))
- Add speckit tooling and baseline feature spec ([60381e897d](https://github.com/Jylhis/monorepo/commit/60381e897d))
- Add README, architecture guide, update CLAUDE.md ([e00935d9d0](https://github.com/Jylhis/monorepo/commit/e00935d9d0))
- Update all elisp/ references to lisp/ + modules/ ([1938da8428](https://github.com/Jylhis/monorepo/commit/1938da8428))
- Rename elisp/ to lisp/, create modules/ ([240a023ccc](https://github.com/Jylhis/monorepo/commit/240a023ccc))
- **nix:** Migrate to devenv and restructure package paths ([0c1dfd9f8f](https://github.com/Jylhis/monorepo/commit/0c1dfd9f8f))
- Update settings, shell, and docs for skeleton state ([c521bcc340](https://github.com/Jylhis/monorepo/commit/c521bcc340))
- Simplify justfile for skeleton ([249e271099](https://github.com/Jylhis/monorepo/commit/249e271099))
- **nix:** Slim runtime-deps to minimal set ([887d4c62a9](https://github.com/Jylhis/monorepo/commit/887d4c62a9))
- **nix:** Remove jotain-modules package ([0abd4810e9](https://github.com/Jylhis/monorepo/commit/0abd4810e9))
- **nix:** Remove NMT and runtime checks from flake ([c65af1a0b1](https://github.com/Jylhis/monorepo/commit/c65af1a0b1))
- **programming:** Rename jotain/ → jotain- and fix use-package forms ([021de8cd54](https://github.com/Jylhis/monorepo/commit/021de8cd54))
- Rename my/ → jotain-utils- prefix in utils module ([02c9d08aeb](https://github.com/Jylhis/monorepo/commit/02c9d08aeb))
- Apply suggestion from @graphite-app[bot] ([484769bf48](https://github.com/Jylhis/monorepo/commit/484769bf48))
- Apply suggestions from code review ([d212c314f2](https://github.com/Jylhis/monorepo/commit/d212c314f2))
- Update elisp/core.el ([637c91cedb](https://github.com/Jylhis/monorepo/commit/637c91cedb))
- Refactor setq to setopt for customizable variables ([35e1a2add3](https://github.com/Jylhis/monorepo/commit/35e1a2add3))
- Configure world-clock with requested timezones ([b475889a2e](https://github.com/Jylhis/monorepo/commit/b475889a2e))
- Optimize jotain-dashboard-initial-buffer redundant list traversal ([76e54e7348](https://github.com/Jylhis/monorepo/commit/76e54e7348))
- 🎨 Palette: Auto-focus help windows ([f4a5c38e32](https://github.com/Jylhis/monorepo/commit/f4a5c38e32))
- Disable dialog boxes for yes-or-no prompts ([fa2809af01](https://github.com/Jylhis/monorepo/commit/fa2809af01))
- Configure world-clock with requested timezones ([59227174e3](https://github.com/Jylhis/monorepo/commit/59227174e3))
- **core:** Replace expand-region with expreg ([8cca171b79](https://github.com/Jylhis/monorepo/commit/8cca171b79))
- Update programming.el ([5f024db907](https://github.com/Jylhis/monorepo/commit/5f024db907))
- Add missing commands and architecture details to CLAUDE.md ([911397d452](https://github.com/Jylhis/monorepo/commit/911397d452))
- Apply suggestions from code review ([8195eff256](https://github.com/Jylhis/monorepo/commit/8195eff256))
- Optimize org agenda directory traversal ([12ee1b39a6](https://github.com/Jylhis/monorepo/commit/12ee1b39a6))
- Update default fill-column to 100 ([8b1807b49f](https://github.com/Jylhis/monorepo/commit/8b1807b49f))
- Update gitignore ([41ae6e2a80](https://github.com/Jylhis/monorepo/commit/41ae6e2a80))
- **constitution:** Amend to v1.1.0 — add Discoverability, Built-in First, Per-Project Config ([961d20e04d](https://github.com/Jylhis/monorepo/commit/961d20e04d))
- Update CLAUDE.md with accurate module table, overlay docs, and CI info ([a1b0d8ba4b](https://github.com/Jylhis/monorepo/commit/a1b0d8ba4b))
- Initialize beads issues.jsonl ([f23395634e](https://github.com/Jylhis/monorepo/commit/f23395634e))
- Update elisp/git.el ([9414017333](https://github.com/Jylhis/monorepo/commit/9414017333))
- Disable magit-todos-mode by default and configure depth limit ([1c881f0b87](https://github.com/Jylhis/monorepo/commit/1c881f0b87))
- **speckit:** Integrate beads for issue tracking ([9896e88658](https://github.com/Jylhis/monorepo/commit/9896e88658))
- **speckit:** Add worktree cascade to create-new-feature ([84116862ce](https://github.com/Jylhis/monorepo/commit/84116862ce))
- Dependency updates (2 commits)

### Removed

- Remove ignored files ([f907d57d87](https://github.com/Jylhis/monorepo/commit/f907d57d87))
- Remove CI, automation, and non-essential project files ([e9b1abedc5](https://github.com/Jylhis/monorepo/commit/e9b1abedc5))
- Remove speckit specs, framework, and Claude tooling ([e54f258c80](https://github.com/Jylhis/monorepo/commit/e54f258c80))
- Remove beads issue tracker from the repo ([d7b9439e92](https://github.com/Jylhis/monorepo/commit/d7b9439e92))
- Remove lefover file ([c432e000c4](https://github.com/Jylhis/monorepo/commit/c432e000c4))

### Fixed

- Fix NMT daemon tests for macOS (launchd instead of systemd) ([68c3162982](https://github.com/Jylhis/monorepo/commit/68c3162982))
- Fix emacs-30 on darwin: disable mailutils for nss_wrapper ([a5d9c280f2](https://github.com/Jylhis/monorepo/commit/a5d9c280f2))
- Fix infinite recursion in home-manager module ([a4f2da2d7f](https://github.com/Jylhis/monorepo/commit/a4f2da2d7f))
- Fix eglot test for renamed variable ([e0defd4f99](https://github.com/Jylhis/monorepo/commit/e0defd4f99))
- Fix directory traversal vulnerability in file trust check ([57d21bba16](https://github.com/Jylhis/monorepo/commit/57d21bba16))
- Minor keybinding and ensure corrections ([d2fe4717fa](https://github.com/Jylhis/monorepo/commit/d2fe4717fa))
- **systems:** Scope sops keybindings to sops-mode only ([ca714581f8](https://github.com/Jylhis/monorepo/commit/ca714581f8))
- **platforms:** Consolidate Android config into android.el, remove dead code ([a031dced06](https://github.com/Jylhis/monorepo/commit/a031dced06))
- **ui:** Use setopt for custom-safe-themes, fix pixel-scroll daemon mode ([0d73d49f4f](https://github.com/Jylhis/monorepo/commit/0d73d49f4f))
- **fonts:** Resolve font-spec variable shadowing and improve declarations ([8fab3d4077](https://github.com/Jylhis/monorepo/commit/8fab3d4077))
- Fix clean unloading of Emacs themes ([ec294d344a](https://github.com/Jylhis/monorepo/commit/ec294d344a))
- Fix child frame font inheritance ([a01ba18e7b](https://github.com/Jylhis/monorepo/commit/a01ba18e7b))
- Fix platform-gui-p and platform-terminal-p to evaluate dynamically ([c692c9495b](https://github.com/Jylhis/monorepo/commit/c692c9495b))
- Fix clean unloading of Emacs themes ([981af0d5de](https://github.com/Jylhis/monorepo/commit/981af0d5de))
- Replace plain fonts with nerd-font variants to restore icons ([2ae59f3d73](https://github.com/Jylhis/monorepo/commit/2ae59f3d73))
- Add app meta.description and remove stale kind-icon test ([c2e307145f](https://github.com/Jylhis/monorepo/commit/c2e307145f))
- Fix quoting in git worktree detection logic ([f2da753708](https://github.com/Jylhis/monorepo/commit/f2da753708))

## 2026-02

### Added

- **web:** Add Nix installation via SessionStart hook ([b28e95cde8](https://github.com/Jylhis/monorepo/commit/b28e95cde8))
- Add claude-code-ide Emacs integration ([06e88aa712](https://github.com/Jylhis/monorepo/commit/06e88aa712))
- **shell:** Add MCP server infrastructure for dev shell ([6639d4022d](https://github.com/Jylhis/monorepo/commit/6639d4022d))
- **claude:** Extend Claude Code setup with new skills and hooks ([9fd7f97202](https://github.com/Jylhis/monorepo/commit/9fd7f97202))
- **ui:** Implement clean theme loading to prevent blending ([637806a08d](https://github.com/Jylhis/monorepo/commit/637806a08d))
- **programming:** Add jinja2-mode support ([cb155200ac](https://github.com/Jylhis/monorepo/commit/cb155200ac))
- **programming:** Add jinja2-mode support ([5f4ed8bb7d](https://github.com/Jylhis/monorepo/commit/5f4ed8bb7d))
- **ui:** Respect system theme and add configuration options ([0ea6ecd5d0](https://github.com/Jylhis/monorepo/commit/0ea6ecd5d0))
- **ui:** Respect system theme and add configuration options ([f7081744e6](https://github.com/Jylhis/monorepo/commit/f7081744e6))

### Changed

- Update flake input ([0b9d91c682](https://github.com/Jylhis/monorepo/commit/0b9d91c682))
- Disable claude-code due to conflicts ([384c1d7f46](https://github.com/Jylhis/monorepo/commit/384c1d7f46))
- Refactor agents to skills and add Claude Code IDE integration ([0835090388](https://github.com/Jylhis/monorepo/commit/0835090388))
- Extract AI tooling into dedicated ai.el module ([0393d6fe0a](https://github.com/Jylhis/monorepo/commit/0393d6fe0a))
- Convert agents to skills for better context and auto-discovery ([1815f15819](https://github.com/Jylhis/monorepo/commit/1815f15819))
- # Init speckit ([a9dc260693](https://github.com/Jylhis/monorepo/commit/a9dc260693))
- **dashboard:** Simplify enlight config and menu items ([9dc2a4094d](https://github.com/Jylhis/monorepo/commit/9dc2a4094d))
- Switch theme to doom-nord for dark mode ([607b8b6a83](https://github.com/Jylhis/monorepo/commit/607b8b6a83))
- Use doom theme ([6302a4fc32](https://github.com/Jylhis/monorepo/commit/6302a4fc32))
- Apply suggestion from @gemini-code-assist[bot] ([4962735c8a](https://github.com/Jylhis/monorepo/commit/4962735c8a))
- ⚡ Optimize font enumeration in platform-show-info ([a2dc35f339](https://github.com/Jylhis/monorepo/commit/a2dc35f339))
- Update elisp/utils.el ([d057c4d222](https://github.com/Jylhis/monorepo/commit/d057c4d222))
- Optimize org-agenda-files update loop ([3d167ab686](https://github.com/Jylhis/monorepo/commit/3d167ab686))
- Make XDG dependency optional and handle downloads path robustly ([f4faa14de1](https://github.com/Jylhis/monorepo/commit/f4faa14de1))

### Removed

- **shell:** Remove org-mcp server and its Emacs build infrastructure ([e7103dc807](https://github.com/Jylhis/monorepo/commit/e7103dc807))

### Fixed

- **mcp:** Switch MCP servers to nix run for devShell-free invocation ([cb55fde0f5](https://github.com/Jylhis/monorepo/commit/cb55fde0f5))
- Fix grammar ([1b71a85add](https://github.com/Jylhis/monorepo/commit/1b71a85add))
- Fix init.el ([f9407ee25d](https://github.com/Jylhis/monorepo/commit/f9407ee25d))
- **ui:** Update modus-themes config for v4 compatibility ([c8cb87c2f6](https://github.com/Jylhis/monorepo/commit/c8cb87c2f6))
- **ui:** Update modus-themes config for v4 compatibility ([98ba5a8297](https://github.com/Jylhis/monorepo/commit/98ba5a8297))

## 2026-01

### Added

- Add Enlight dashboard configuration with improvements ([e2eb6ecb92](https://github.com/Jylhis/monorepo/commit/e2eb6ecb92))
- Add Enlight dashboard configuration ([3747b0f4b2](https://github.com/Jylhis/monorepo/commit/3747b0f4b2))
- Add csv-mode configuration ([21c861d32e](https://github.com/Jylhis/monorepo/commit/21c861d32e))
- Show worktrees in magit status buffer ([e6f76ac9fc](https://github.com/Jylhis/monorepo/commit/e6f76ac9fc))
- Show worktrees in magit status buffer ([6d2a60c48e](https://github.com/Jylhis/monorepo/commit/6d2a60c48e))
- Add LLM.md and streamline CLAUDE.md ([eff7584089](https://github.com/Jylhis/monorepo/commit/eff7584089))
- Add initial Renovate configuration file ([8039d6be4c](https://github.com/Jylhis/monorepo/commit/8039d6be4c))
- Add docker and sonarlint lsp ([bc305835b1](https://github.com/Jylhis/monorepo/commit/bc305835b1))

### Changed

- Update elisp/programming.el ([da04d98efa](https://github.com/Jylhis/monorepo/commit/da04d98efa))
- Switch to pkgs.tree-sitter.allGrammars for all language grammars ([50674ff711](https://github.com/Jylhis/monorepo/commit/50674ff711))
- Update elisp/git.el ([99eb8b9513](https://github.com/Jylhis/monorepo/commit/99eb8b9513))
- Update tests for new :ensure convention and Nix tree-sitter ([c53d6fc2db](https://github.com/Jylhis/monorepo/commit/c53d6fc2db))
- Standardize use-package declarations and add history sorting ([0036a9f1e1](https://github.com/Jylhis/monorepo/commit/0036a9f1e1))
- Adapt configuration for Emacs 30+ built-in features ([3f38cbf2be](https://github.com/Jylhis/monorepo/commit/3f38cbf2be))
- Update minimum Emacs version to 30.2 ([ae72961d86](https://github.com/Jylhis/monorepo/commit/ae72961d86))
- Comment broken sonarlint-ls ([2671d88ca2](https://github.com/Jylhis/monorepo/commit/2671d88ca2))
- Update language extraction format ([3a9a5c909c](https://github.com/Jylhis/monorepo/commit/3a9a5c909c))

### Removed

- Remove bash-language-server tests ([9be13aba43](https://github.com/Jylhis/monorepo/commit/9be13aba43))
- Remove bash-language-server from runtime dependencies ([010e9533c0](https://github.com/Jylhis/monorepo/commit/010e9533c0))

### Fixed

- Fix startup error: Move enlight-content to :config block ([64d7b5d8be](https://github.com/Jylhis/monorepo/commit/64d7b5d8be))
- Remove space from csv-separators ([c59779929c](https://github.com/Jylhis/monorepo/commit/c59779929c))

## 2025-12

### Added

- Add Gited package for Git branch management ([183f1d016c](https://github.com/Jylhis/monorepo/commit/183f1d016c))

### Changed

- Use only linux systems ([a57b2bdc6a](https://github.com/Jylhis/monorepo/commit/a57b2bdc6a))

### Fixed

- Fix warning ([768372b558](https://github.com/Jylhis/monorepo/commit/768372b558))
- Fix flake check ([457a08fac5](https://github.com/Jylhis/monorepo/commit/457a08fac5))

## 2025-11

### Added

- Add comprehensive test suite for completion.el module ([4b600e404a](https://github.com/Jylhis/monorepo/commit/4b600e404a))
- Add comprehensive test suite for programming.el module ([339a39ad43](https://github.com/Jylhis/monorepo/commit/339a39ad43))
- New dependency management ([ef6bbadb10](https://github.com/Jylhis/monorepo/commit/ef6bbadb10))
- Add streamlined test-jotain.el smoke tests ([52da7f912c](https://github.com/Jylhis/monorepo/commit/52da7f912c))
- Add jotain-tools.el with Git, Org, and help tools ([66c182df02](https://github.com/Jylhis/monorepo/commit/66c182df02))
- Add jotain-prog.el with essential programming tools ([539b38634e](https://github.com/Jylhis/monorepo/commit/539b38634e))
- Add jotain-editor.el with UI and completion ([7f58b41e3f](https://github.com/Jylhis/monorepo/commit/7f58b41e3f))
- Add jotain-core.el with consolidated core settings ([5bd4973598](https://github.com/Jylhis/monorepo/commit/5bd4973598))
- Add jotain.el main entry point ([74cb935429](https://github.com/Jylhis/monorepo/commit/74cb935429))
- Add worktree management system with JIRA integration ([5d1b354630](https://github.com/Jylhis/monorepo/commit/5d1b354630))
- Implement window split toggle ([ba608afd5d](https://github.com/Jylhis/monorepo/commit/ba608afd5d))
- Initial new structure ([706a321cca](https://github.com/Jylhis/monorepo/commit/706a321cca))
- Add test suite ([facf887604](https://github.com/Jylhis/monorepo/commit/facf887604))
- Add CLI implementation ([96cc7c49e1](https://github.com/Jylhis/monorepo/commit/96cc7c49e1))
- Add Emacs init templates ([96edde2080](https://github.com/Jylhis/monorepo/commit/96edde2080))
- Add Emacs Lisp source code ([5362d67689](https://github.com/Jylhis/monorepo/commit/5362d67689))
- Add Nix infrastructure and modules ([f1e96f18d8](https://github.com/Jylhis/monorepo/commit/f1e96f18d8))
- Add GitHub workflows and configuration ([66e4253b70](https://github.com/Jylhis/monorepo/commit/66e4253b70))
- Add project documentation ([8c40daae0c](https://github.com/Jylhis/monorepo/commit/8c40daae0c))
- Add development tooling configuration ([304b75337e](https://github.com/Jylhis/monorepo/commit/304b75337e))
- Add .gitignore ([f44f2a7ab2](https://github.com/Jylhis/monorepo/commit/f44f2a7ab2))
- Add Nix flake infrastructure ([dd711258d5](https://github.com/Jylhis/monorepo/commit/dd711258d5))
- Add comprehensive Doom Emacs and Spacemacs analysis ([0bdd798d46](https://github.com/Jylhis/monorepo/commit/0bdd798d46))
- Add comprehensive Doom/Spacemacs research findings and recommendations ([085809fe19](https://github.com/Jylhis/monorepo/commit/085809fe19))
- Add comprehensive language configurations for 15+ languages ([4d32f5eaef](https://github.com/Jylhis/monorepo/commit/4d32f5eaef))
- Add Claude Code SessionStart hook for automatic Nix setup ([5d5e56ed06](https://github.com/Jylhis/monorepo/commit/5d5e56ed06))
- Implement Doom Emacs and Spacemacs patterns for enhanced configuration ([407c24829a](https://github.com/Jylhis/monorepo/commit/407c24829a))

### Changed

- Improve test quality: platform detection and runtime deps ([081eeb14ed](https://github.com/Jylhis/monorepo/commit/081eeb14ed))
- Cleanup storage ([31d6061f3e](https://github.com/Jylhis/monorepo/commit/31d6061f3e))
- Clean workflows ([910022fc2c](https://github.com/Jylhis/monorepo/commit/910022fc2c))
- Update GitHub Actions to latest versions ([d6d161b541](https://github.com/Jylhis/monorepo/commit/d6d161b541))
- Improve dependency handling ([f872f25e37](https://github.com/Jylhis/monorepo/commit/f872f25e37))
- Update readme ([427b11049d](https://github.com/Jylhis/monorepo/commit/427b11049d))
- Update lock ([6ba104974e](https://github.com/Jylhis/monorepo/commit/6ba104974e))
- Deps management ([fe7eb211d1](https://github.com/Jylhis/monorepo/commit/fe7eb211d1))
- Update title in readme.org and remove section ([2eacf23533](https://github.com/Jylhis/monorepo/commit/2eacf23533))
- Update caching actions in emacs.yml workflow ([9de4bbe9d1](https://github.com/Jylhis/monorepo/commit/9de4bbe9d1))
- Replace magic-nix-cache-action with flakehub-cache-action ([40a062c338](https://github.com/Jylhis/monorepo/commit/40a062c338))
- Mark built-in packages with :ensure nil ([e228bc8606](https://github.com/Jylhis/monorepo/commit/e228bc8606))
- Mark app-launcher as local package with :ensure nil ([b6eb59bd08](https://github.com/Jylhis/monorepo/commit/b6eb59bd08))
- Restructure Nix configuration with automatic dependency extraction ([0ca092d020](https://github.com/Jylhis/monorepo/commit/0ca092d020))
- Update CLAUDE.md for new architecture ([c5fc769cec](https://github.com/Jylhis/monorepo/commit/c5fc769cec))
- Update flake.nix excludes for new structure ([4c03000ecd](https://github.com/Jylhis/monorepo/commit/4c03000ecd))
- Simplify config.nix for new structure ([b9d3e9a29e](https://github.com/Jylhis/monorepo/commit/b9d3e9a29e))
- Update default.nix for new structure ([03d416541b](https://github.com/Jylhis/monorepo/commit/03d416541b))
- Simplify init.el to minimal loader ([c0ee38b2b0](https://github.com/Jylhis/monorepo/commit/c0ee38b2b0))
- Rename package from worktree-manager to work-manager ([1b14cd9b17](https://github.com/Jylhis/monorepo/commit/1b14cd9b17))
- Tweak use-package configuration ([8a694843b7](https://github.com/Jylhis/monorepo/commit/8a694843b7))
- Create standalone worktree-manager package and generic work module ([e4db8eb001](https://github.com/Jylhis/monorepo/commit/e4db8eb001))
- Beginning of new foundation ([3e4a5b3980](https://github.com/Jylhis/monorepo/commit/3e4a5b3980))
- Migrate configs ([61d507eba3](https://github.com/Jylhis/monorepo/commit/61d507eba3))
- Cleanup docs ([aa056eace1](https://github.com/Jylhis/monorepo/commit/aa056eace1))
- Refactor dev shell to use template files ([ac40dd9a59](https://github.com/Jylhis/monorepo/commit/ac40dd9a59))
- Refactor configuration for aggressive lazy loading following Doom/Spacemacs patterns ([dc3d3be26e](https://github.com/Jylhis/monorepo/commit/dc3d3be26e))
- Update SessionStart hook to only install Nix in cloud environments ([2faae22274](https://github.com/Jylhis/monorepo/commit/2faae22274))

### Removed

- Remove redundant packageNameMap entries in dependency extractor ([a6e181b9c0](https://github.com/Jylhis/monorepo/commit/a6e181b9c0))
- Remove old elisp utility files ([5b04fb75ec](https://github.com/Jylhis/monorepo/commit/5b04fb75ec))
- Remove old config/ directory ([fc6da89eec](https://github.com/Jylhis/monorepo/commit/fc6da89eec))
- Remove old complex test infrastructure ([ebcab454c5](https://github.com/Jylhis/monorepo/commit/ebcab454c5))
- Remove everything ([15766b9e4b](https://github.com/Jylhis/monorepo/commit/15766b9e4b))

### Fixed

- Fix test compatibility: web-mode and yaml-mode ([4ab6772d72](https://github.com/Jylhis/monorepo/commit/4ab6772d72))
- Fix critical early-init.el typo and refactor runtime dependencies ([272b2fff20](https://github.com/Jylhis/monorepo/commit/272b2fff20))
- Fix treesitter ([5e4acd7a2b](https://github.com/Jylhis/monorepo/commit/5e4acd7a2b))
- Fix module ([e92c699664](https://github.com/Jylhis/monorepo/commit/e92c699664))
- Fix and format ([0dd2f26c3b](https://github.com/Jylhis/monorepo/commit/0dd2f26c3b))
- Fix completion package loading order ([be30f7e624](https://github.com/Jylhis/monorepo/commit/be30f7e624))

## 2025-10

### Added

- Add development environment isolation to protect user's home config ([d0a0689ae6](https://github.com/Jylhis/monorepo/commit/d0a0689ae6))
- Add kkp ([85b14f06a6](https://github.com/Jylhis/monorepo/commit/85b14f06a6))
- Add autoload cookies to interactive commands ([b4f65ac2f2](https://github.com/Jylhis/monorepo/commit/b4f65ac2f2))
- Add deadnix and statix to devShell ([9e72d51d87](https://github.com/Jylhis/monorepo/commit/9e72d51d87))
- Add kind-icon ([b03414bec3](https://github.com/Jylhis/monorepo/commit/b03414bec3))
- Add cape file ([baafc4230c](https://github.com/Jylhis/monorepo/commit/baafc4230c))
- Add path conditions ([2c518848e0](https://github.com/Jylhis/monorepo/commit/2c518848e0))
- Add Nix and extra actions for automation ([714a800fc9](https://github.com/Jylhis/monorepo/commit/714a800fc9))
- Add NMT tests ([459af68d5a](https://github.com/Jylhis/monorepo/commit/459af68d5a))
- Add tests ([18cb8a80e4](https://github.com/Jylhis/monorepo/commit/18cb8a80e4))
- Add elec-pair ([31e1c1ecec](https://github.com/Jylhis/monorepo/commit/31e1c1ecec))
- Add Claude ([aa9616a42c](https://github.com/Jylhis/monorepo/commit/aa9616a42c))

### Changed

- Simplify testing infrastructure without sacrificing coverage ([b57367ddef](https://github.com/Jylhis/monorepo/commit/b57367ddef))
- Rename repo ([338f40554f](https://github.com/Jylhis/monorepo/commit/338f40554f))
- Update agents ([15c3195f3a](https://github.com/Jylhis/monorepo/commit/15c3195f3a))
- Clean up TODOs ([8136c95f7f](https://github.com/Jylhis/monorepo/commit/8136c95f7f))
- Find org files ([68ae87449f](https://github.com/Jylhis/monorepo/commit/68ae87449f))
- Update claude file ([8e7ca5604d](https://github.com/Jylhis/monorepo/commit/8e7ca5604d))
- Rename functions with app-launchers prefix ([585be20373](https://github.com/Jylhis/monorepo/commit/585be20373))
- Rename jylhis-setup-org-fonts to my/setup-org-fonts ([fe736c0fe3](https://github.com/Jylhis/monorepo/commit/fe736c0fe3))
- Rename display-ansi-colors to my/display-ansi-colors ([b148989a59](https://github.com/Jylhis/monorepo/commit/b148989a59))
- Add description and example to module.nix option ([2ab3cd6f57](https://github.com/Jylhis/monorepo/commit/2ab3cd6f57))
- Change completion ([ae18a5e2d3](https://github.com/Jylhis/monorepo/commit/ae18a5e2d3))
- Update emacs agent ([4d3201a947](https://github.com/Jylhis/monorepo/commit/4d3201a947))
- Update lock ([eea04da420](https://github.com/Jylhis/monorepo/commit/eea04da420))
- Update emacs agent ([dc4504a326](https://github.com/Jylhis/monorepo/commit/dc4504a326))
- Update claude-code-review.yml ([26257536cb](https://github.com/Jylhis/monorepo/commit/26257536cb))
- Update GitHub Actions workflow configuration ([343fdd5737](https://github.com/Jylhis/monorepo/commit/343fdd5737))
- "Claude Code Review workflow" ([55b8a0ca82](https://github.com/Jylhis/monorepo/commit/55b8a0ca82))
- "Claude PR Assistant workflow" ([7de71a845c](https://github.com/Jylhis/monorepo/commit/7de71a845c))
- Config package ([cfecf204f1](https://github.com/Jylhis/monorepo/commit/cfecf204f1))
- Update claude stuff ([09cc36e6f3](https://github.com/Jylhis/monorepo/commit/09cc36e6f3))
- Run flake check in CI ([ac2d59aff6](https://github.com/Jylhis/monorepo/commit/ac2d59aff6))
- Use fileset for configuration ([0f72adbbfb](https://github.com/Jylhis/monorepo/commit/0f72adbbfb))
- Update readme ([bcc17020f5](https://github.com/Jylhis/monorepo/commit/bcc17020f5))
- Disable obsidian for now ([8873c3a2a0](https://github.com/Jylhis/monorepo/commit/8873c3a2a0))

### Removed

- Remove commented Obsidian code ([42c2c1517b](https://github.com/Jylhis/monorepo/commit/42c2c1517b))
- Remove todo ([a47fd324f7](https://github.com/Jylhis/monorepo/commit/a47fd324f7))

### Fixed

- Fix test ([1e522b0a03](https://github.com/Jylhis/monorepo/commit/1e522b0a03))
- Move mode activations from :custom to :config ([b00ed246a0](https://github.com/Jylhis/monorepo/commit/b00ed246a0))
- Standardize :ensure usage to :ensure t ([6a4df9f2d6](https://github.com/Jylhis/monorepo/commit/6a4df9f2d6))
- Remove redundant require in collaboration.el ([386b407f7f](https://github.com/Jylhis/monorepo/commit/386b407f7f))
- Replace hardcoded x86_64-linux with system detection in justfile ([9ad0bdde38](https://github.com/Jylhis/monorepo/commit/9ad0bdde38))
- Fix binding ([2b1d0ffda6](https://github.com/Jylhis/monorepo/commit/2b1d0ffda6))
- Fix typo ([87b7223c25](https://github.com/Jylhis/monorepo/commit/87b7223c25))
- Fix rule ([203ec6c132](https://github.com/Jylhis/monorepo/commit/203ec6c132))
- Fix erros ([8d7310ba59](https://github.com/Jylhis/monorepo/commit/8d7310ba59))
- Fix errors ([a09d9c4d49](https://github.com/Jylhis/monorepo/commit/a09d9c4d49))
- Fix config ([c3ef1b3e50](https://github.com/Jylhis/monorepo/commit/c3ef1b3e50))
- Fix actions ([cd682b875e](https://github.com/Jylhis/monorepo/commit/cd682b875e))

## 2025-09

### Added

- Create flakehub-publish-rolling.yml ([5320359c62](https://github.com/Jylhis/monorepo/commit/5320359c62))
- Initial action ([17450dd03d](https://github.com/Jylhis/monorepo/commit/17450dd03d))

### Changed

- Init flake ([18693c3bb2](https://github.com/Jylhis/monorepo/commit/18693c3bb2))
- Sync Emacs configuration from source repository ([25807d20fd](https://github.com/Jylhis/monorepo/commit/25807d20fd))
- Sync Emacs configuration from source repository ([d122794775](https://github.com/Jylhis/monorepo/commit/d122794775))
- Sync Emacs configuration from source repository ([809f35841c](https://github.com/Jylhis/monorepo/commit/809f35841c))
- Sync Emacs configuration from source repository ([336f0ebfc8](https://github.com/Jylhis/monorepo/commit/336f0ebfc8))
- Sync Emacs configuration from source repository ([092cb5cc6d](https://github.com/Jylhis/monorepo/commit/092cb5cc6d))
- Sync Emacs configuration from source repository ([f9f80a799f](https://github.com/Jylhis/monorepo/commit/f9f80a799f))
- Sync Emacs configuration from source repository ([5bc77421d9](https://github.com/Jylhis/monorepo/commit/5bc77421d9))
- Sync Emacs configuration from source repository ([0e97d8644e](https://github.com/Jylhis/monorepo/commit/0e97d8644e))
- Sync Emacs configuration from source repository ([78bb6a4c3a](https://github.com/Jylhis/monorepo/commit/78bb6a4c3a))
- Sync Emacs configuration from source repository ([8722adbd79](https://github.com/Jylhis/monorepo/commit/8722adbd79))
- Sync Emacs configuration from source repository ([920aca57b2](https://github.com/Jylhis/monorepo/commit/920aca57b2))
- Sync Emacs configuration from source repository ([52f494265f](https://github.com/Jylhis/monorepo/commit/52f494265f))
- Sync Emacs configuration from source repository ([866d9a4575](https://github.com/Jylhis/monorepo/commit/866d9a4575))
- Sync Emacs configuration from source repository ([f11f68df91](https://github.com/Jylhis/monorepo/commit/f11f68df91))
- Sync Emacs configuration from source repository ([c8bd55b935](https://github.com/Jylhis/monorepo/commit/c8bd55b935))
- Sync Emacs configuration from source repository ([8885cd0a40](https://github.com/Jylhis/monorepo/commit/8885cd0a40))

## 2025-08

### Changed

- Sync Emacs configuration from source repository ([cee2b10bdc](https://github.com/Jylhis/monorepo/commit/cee2b10bdc))
- Sync Emacs configuration from source repository ([d7bc80b4a6](https://github.com/Jylhis/monorepo/commit/d7bc80b4a6))
- Sync Emacs configuration from source repository ([b66962d12c](https://github.com/Jylhis/monorepo/commit/b66962d12c))
- Sync Emacs configuration from source repository ([6b198d31b9](https://github.com/Jylhis/monorepo/commit/6b198d31b9))
- Sync with new configuration ([59166dd46d](https://github.com/Jylhis/monorepo/commit/59166dd46d))

## 2024-05

### Added

- Initial commit ([42c2e5e06b](https://github.com/Jylhis/monorepo/commit/42c2e5e06b))
