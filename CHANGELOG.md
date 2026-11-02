# Changelog

All notable changes to emacs are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
This project has no versioned releases, so entries are grouped by month.
Entries up to 2026-11 were reconstructed from git history (first commit 2026-10-11).

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
- Move lib/emacs to etc/emacs ([6fb57b973c](https://github.com/Jylhis/monorepo/commit/6fb57b973c))
- **TODO:** Consolidate sibling TODO files, index inline TODOs, drop done items ([18c9b10e5b](https://github.com/Jylhis/monorepo/commit/18c9b10e5b))
- Reject absolute repo-review paths and keep the nil-path error ([eeeb3bd902](https://github.com/Jylhis/monorepo/commit/eeeb3bd902))
- Reject repo-review entries outside the review root ([04280269f1](https://github.com/Jylhis/monorepo/commit/04280269f1))
- Make the configuration Linux-only ([8af9ec0c77](https://github.com/Jylhis/monorepo/commit/8af9ec0c77))
- Port asdf deltas ([b3000d73b0](https://github.com/Jylhis/monorepo/commit/b3000d73b0))
- Move lib/jotain to lib/emacs ([103d2dd7b1](https://github.com/Jylhis/monorepo/commit/103d2dd7b1))
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

### Removed

- Drop settings equal to their defaults ([3114d8e017](https://github.com/Jylhis/monorepo/commit/3114d8e017))
- Drop the Jotain name ([515c4ee27a](https://github.com/Jylhis/monorepo/commit/515c4ee27a))

### Fixed

- Select IBM Plex Mono on NixOS-only installs ([41629215af](https://github.com/Jylhis/monorepo/commit/41629215af))
- Use Geist for the body font and install it with the module ([6b45bec504](https://github.com/Jylhis/monorepo/commit/6b45bec504))
- Extend mixed-pitch faces after load ([3d58fdda95](https://github.com/Jylhis/monorepo/commit/3d58fdda95))
