# website — page.jylhis.com/jotain

The landing page and docs site for Jotain, styled as an Emacs frame: a tab
bar of three buffers (`README.org` landing, `*Man JOTAIN(7)*` docs index,
`keybindings`), a modeline, and a minibuffer with `I-search` over the site's
sections. Implements the "Jotain Website" Claude Design prototype on the
[Jylhis design system](https://github.com/jylhis/design) v2 "The Survey"
(Sheet/Field themes, bronze accent, Zilla Slab + Hanken Grotesk + IBM Plex
Mono).

## Layout

- `public/` — the deployed static site, no build step
  - `index.html` — all three buffers in one page, hash-routed (`#readme`,
    `#man`, `#keys`, plus section anchors like `#sec-qs`)
  - `css/site.css` — page styles; colors only via design-system tokens
  - `js/app.js` — buffer switching, `C-s` I-search, `C-x b` / `n` / `p`
    keys, modeline position, theme toggle (persisted to `localStorage`)
  - `ds/` — Jylhis design system CSS (`tokens.css`, `fonts.css`,
    `colors_and_type.css`, `motion.css`) and self-hosted woff2 fonts
    (fontsource 5.x, latin + latin-ext slices of the three v2 families),
    copied verbatim from upstream — never edit them here. The revision
    they come from is pinned in `nix/design-pin.nix`, the same pin the
    Emacs themes are built from; `just ds-sync` re-vendors this directory
    from it and the `ds-in-sync` flake check fails when the two disagree.

## Generated content

`website/public/` is only the shell (landing SPA + shared CSS/JS/fonts).
The full site is assembled by `nix build .#site` (`nix/site.nix`), which
adds everything the repo can generate:

- `/docs/…` — every `docs/**/*.mdx` page rendered to HTML, navigation
  and ordering from `docs/docs.json`
- `/manual/` — the Jotain manual (`docs/jotain.texi` pipeline) as HTML,
  one page per chapter, plus `/manual/jotain.info` for `C-h i`
- `/man/` — `jotain(7)` (from `docs/jotain.7.md`, also served as raw
  troff) and the man pages shipped by the Emacs build, via mandoc
- `/info/emacs/`, `/info/elisp/` — the GNU Emacs manual and the Emacs
  Lisp reference manual, rendered from the exact Emacs source revision
  Jotain builds
- `/options/` — Nix module options reference (`nix/options-doc.nix`)
- `/help/packages/` — per-package reference from `;;; @doc` markers
  (`nix/packages-doc.nix`)

## Deployment

The site is published to **GitHub Pages** by `deploy.yml` on every push
to main: the `build-pages` job runs `nix build .#site` and uploads the
`public/` tree as the Pages artifact, and `deploy-pages` publishes it.
GitHub serves it as this repo's project site at
**<https://page.jylhis.com/jotain/>** — `page.jylhis.com` is the account's
Pages custom domain (a CNAME to `jylhis.github.io`), so every project
repo's Pages appear under it at `/<repo>/`.

Because the site is served under the `/jotain/` subpath, `nix build .#site`
bakes that base path into every internal absolute URL via `nix/site.nix`'s
`baseHref` argument (default `/jotain`); pass `baseHref = ""` to build a
root-served copy. `index.html`'s own nav links are document-relative, so
the shell works at any base without rewriting.

## Local preview

```
just serve-site          # full site: nix build .#site, served under /jotain/
python3 -m http.server -d website/public 8080   # shell only (at the root)
```

## Conventions

- No hex literals in `site.css` — colors come from `ds/tokens.css`
  custom properties, so both themes stay in sync.
- Four places cannot use a custom property and hold hand-synced copies of
  token values instead: the `theme-color` metas in `index.html` and in
  `nix/site.nix`'s generated-page template, and `favicon.svg`. Update all
  four on every retheme.
- Generated markup (makeinfo, pandoc, mandoc) is styled by `manual.css`,
  `pandoc-page.css` and `docs.css`, which pin `h1`–`h6` to `--font-mono`
  on purpose: an unstyled heading inherits `--font-heading`, the design
  system's slab display face at `--type-scale-0` (3.25rem).
- Both themes always ship: Sheet (light) is `:root`, Field (dark) is
  `[data-theme="dark"]` on `<html>`.
- Fonts are self-hosted; no third-party requests at runtime.
