# Changelog

All notable changes to emacs-docs are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
This project has no versioned releases, so entries are grouped by month.
Entries up to 2026-11 were reconstructed from git history (first commit 2026-07-21).

## [Unreleased]

### Changed

- The full Nix-built site is served from the root of docs.example.org: a dedicated workflow builds it and runs wrangler deploy when main is green, replacing the Workers Builds shell that could only ship the committed landing page. ([aab05e8d5d](https://github.com/Jylhis/monorepo/commit/aab05e8d5d))

## 2026-10

### Added

- The site's stylesheets import elements.css, which styles bare HTML elements, form controls, tables, dialogs and details with zero-specificity selectors so plain markup matches the design system. ([b63e48b0ce](https://github.com/Jylhis/monorepo/commit/b63e48b0ce))

### Changed

- Pages, titles and the man-page buffer call the configuration emacs-config instead of Jotain, following its rename. ([515c4ee27a](https://github.com/Jylhis/monorepo/commit/515c4ee27a))
- The site gets its own Nix package, justfile and man page, and its vendored design assets are built from the in-tree design system rather than a pinned upstream revision, with `just ds-sync` and a sync check keeping the committed copy current. ([cef5b12c12](https://github.com/Jylhis/monorepo/commit/cef5b12c12), [7a5e40a333](https://github.com/Jylhis/monorepo/commit/7a5e40a333))
- The vendored design system moves through versions 4.0 to 6.0: the Paper and Black modes follow the OS colour scheme when no mode is set, with a matching light and dark theme-color pair; headings use Jost, body text Fira Sans and code IBM Plex Mono, served as latin subsets built from nixpkgs. ([ac54b87d9c](https://github.com/Jylhis/monorepo/commit/ac54b87d9c), [377f04b56d](https://github.com/Jylhis/monorepo/commit/377f04b56d), [95d4b0a013](https://github.com/Jylhis/monorepo/commit/95d4b0a013))
- Restyled tokens and bare elements: raised keycap buttons with a pressed state, labels without uppercase, underlined links, a single 2px corner, bevel and rule treatments for inputs and tables, and contrast floors for border and status colours. ([7b387abfc3](https://github.com/Jylhis/monorepo/commit/7b387abfc3), [deee8da6ca](https://github.com/Jylhis/monorepo/commit/deee8da6ca), [5f6e3e289c](https://github.com/Jylhis/monorepo/commit/5f6e3e289c))
- Code syntax colours are retuned after Emacs Modus and Leuven, with keywords and function names in bold and comments and docstrings in italic. ([ac20cfc5c8](https://github.com/Jylhis/monorepo/commit/ac20cfc5c8), [8ad342dd7f](https://github.com/Jylhis/monorepo/commit/8ad342dd7f))

### Removed

- The Geist, Geist Mono and Instrument Serif font files and licences are dropped from the site in favour of the new fonts. ([b4cdef075e](https://github.com/Jylhis/monorepo/commit/b4cdef075e))

### Fixed

- Focus rings and the caret follow scoped light and dark modes, hidden=until-found works on bare elements, and element styles no longer leak into drawers, terminal output or tooltips. ([f3849591f4](https://github.com/Jylhis/monorepo/commit/f3849591f4), [8fd272aa54](https://github.com/Jylhis/monorepo/commit/8fd272aa54))
- Warning text clears 4.5:1 contrast in the light theme, and controls keep a 44px touch target on phones at default density. ([84f337af22](https://github.com/Jylhis/monorepo/commit/84f337af22), [18d098b343](https://github.com/Jylhis/monorepo/commit/18d098b343))

## 2026-09

### Added

- A /packages/ page offers live search over every bundled package and symbol, backed by a generated search-index.json; builds without the API reference omit the link to it. ([92f9c9e6ad](https://github.com/Jylhis/monorepo/commit/92f9c9e6ad), [302d9141c3](https://github.com/Jylhis/monorepo/commit/302d9141c3))

### Changed

- The site moves from Cloudflare Workers to GitHub Pages under a /jotain base path, with a baseHref option applied to every internal URL. ([3c5b3b9d39](https://github.com/Jylhis/monorepo/commit/3c5b3b9d39))
- Production publishes from the GitHub Actions Pages source, and pull request previews are downloadable workflow artifacts. ([5f6419b9b0](https://github.com/Jylhis/monorepo/commit/5f6419b9b0))
- The vendored design files follow the design system's v2.0.0 theming framework and then 3.0.0, which refreshes the site's tokens, fonts and favicon and adds density tokens. ([2f5e5760df](https://github.com/Jylhis/monorepo/commit/2f5e5760df), [61e3ed9a50](https://github.com/Jylhis/monorepo/commit/61e3ed9a50))

## 2026-07

### Added

- Landing page and docs site for the Emacs configuration, styled as an Emacs frame with README, manual and keybindings buffers, section search and a theme toggle. ([60bec618af](https://github.com/Jylhis/monorepo/commit/60bec618af))
- A Nix build assembles the full site from the docs pages, the configuration manual, man pages, the Emacs and Elisp manuals, the options reference and package help. ([3dd89f5d8b](https://github.com/Jylhis/monorepo/commit/3dd89f5d8b))

### Changed

- The site's CSS and fonts move to design system v2 (Zilla Slab, Hanken Grotesk and IBM Plex Mono), vendored from a pinned revision with a sync check and a `just ds-sync` recipe. ([2d27344773](https://github.com/Jylhis/monorepo/commit/2d27344773))
