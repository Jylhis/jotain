# website — page.jylhis.com/jotain

The landing page and docs site for Jotain, styled as an Emacs frame: a tab
bar of three buffers (`README.org` landing, `*Man JOTAIN(7)*` docs index,
`keybindings`), a modeline, and a minibuffer with `I-search` over the site's
sections. Built on the [Jylhis design system](https://github.com/jylhis/design)
v3 (the single `jylhis` theme, Print/Negative modes, bronze accent, Zilla
Slab + Hanken Grotesk + IBM Plex Mono + IBM Plex Sans Condensed).

## Layout

- `public/`: the deployed static site, no build step
  - `index.html`: all three buffers in one page, hash-routed (`#readme`,
    `#man`, `#keys`, plus section anchors like `#sec-qs`)
  - `css/site.css`: page styles; colors only via design-system tokens
  - `js/app.js`: buffer switching, `C-s` I-search, `C-x b` / `n` / `p`
    keys, modeline position, theme toggle (persisted to `localStorage`)
  - `ds/`: Jylhis design system CSS (`tokens.css`, `density.css`,
    `fonts.css`, `colors_and_type.css`, `motion.css`) and self-hosted
    fonts with their OFL licenses, copied verbatim from upstream. Never
    edit them here. The revision is pinned in `nix/design-pin.nix`, the
    same pin the Emacs themes are built from; `just ds-sync` re-vendors
    the directory, and the `ds-in-sync` flake check fails when they
    disagree.

## Generated content

`website/public/` is only the shell (landing SPA + shared CSS/JS/fonts).
`nix build .#site` (`nix/site.nix`) assembles the full site, adding:

- `/docs/…`: every `docs/**/*.mdx` page rendered to HTML, ordered by
  `docs/docs.json`
- `/manual/`: the Jotain manual (`docs/jotain.texi`) as HTML, one page per
  chapter, plus `/manual/jotain.info` for `C-h i`
- `/man/`: `jotain(7)` (from `docs/jotain.7.md`, also served as raw
  troff) and the man pages shipped by the Emacs build, via mandoc
- `/info/emacs/`, `/info/elisp/`: the GNU Emacs and Emacs Lisp manuals,
  rendered from the Emacs source revision Jotain builds
- `/options/`: Nix module options reference (`nix/options-doc.nix`)
- `/help/packages/`: per-package reference from `;;; @doc` markers
  (`nix/packages-doc.nix`)
- `/help/api/`: docstring-level API reference for every bundled package
  (`nix/emacs-api-doc.nix`); full `.#site` only, omitted from
  `.#site-preview`
- `/packages/`: package and symbol search over `/help/api/`
  (`website/public/js/packages.js`); also full-`.#site` only

## Deployment

`deploy.yml` publishes the site to **GitHub Pages** (Actions source) on
every push to main: `build-pages` builds `.#site` and uploads `public/`
with `actions/upload-pages-artifact`, and `deploy-pages` publishes it with
`actions/deploy-pages`. It is served as this repo's project site at
**<https://page.jylhis.com/jotain/>** (`page.jylhis.com` is the account's
Pages custom domain, so each project repo appears under `/<repo>/`).

Pages hosts a single deployment, so PR previews can't share it.
`preview.yml` builds the full `.#site` on every pull request and uploads it
as a **downloadable workflow artifact** (`site-preview-pr-<N>`). The tree is
packed into `site.tgz` because `actions/upload-artifact` rejects the `:` and
`*` in some generated `/help/api/` filenames. A bot comment links to it;
extract it and serve it under `/jotain/`, as `just serve-site` does.

`nix/site.nix`'s `baseHref` argument (default `/jotain`) prefixes every
internal absolute URL; pass `baseHref = ""` for a root-served copy.
`index.html`'s own nav links are document-relative, so the shell works at
any base.

## Local preview

```
just serve-site          # full site: nix build .#site, served under /jotain/
python3 -m http.server -d website/public 8080   # shell only (at the root)
```

## Conventions

- No hex literals in `site.css`: colors come from `ds/tokens.css` custom
  properties, so both modes stay in sync.
- A few places cannot use a custom property and hold hand-synced copies of
  token values: the `theme-color` metas in `index.html` and in
  `nix/site.nix`'s generated-page template, and `favicon.svg`. Update all
  of them on every retheme.
- `manual.css`, `pandoc-page.css`, and `docs.css` style generated markup
  (makeinfo, pandoc, mandoc) and pin `h1`–`h6` to `--font-mono`: an
  unstyled heading would inherit `--font-heading`, the design system's slab
  display face at `--type-scale-0` (3.25rem).
- Both modes always ship: Print (light) is `:root`, Negative (dark) is
  `[data-mode="dark"]` on `<html>`.
- Fonts are self-hosted; no third-party requests at runtime.
