# website — jotain.j10s.io

The landing page and docs site for Jotain, styled as an Emacs frame: a tab
bar of three buffers (`README.org` landing, `*Man JOTAIN(7)*` docs index,
`keybindings`), a modeline, and a minibuffer with `I-search` over the site's
sections. Implements the "Jotain Website" Claude Design prototype on the
[Jylhis design system](https://github.com/jylhis/design) (Paper/Roast themes,
copper accent, JetBrains Mono + Literata).

## Layout

- `public/` — the deployed static site, no build step
  - `index.html` — all three buffers in one page, hash-routed (`#readme`,
    `#man`, `#keys`, plus section anchors like `#sec-qs`)
  - `css/site.css` — page styles; colors only via design-system tokens
  - `js/app.js` — buffer switching, `C-s` I-search, `C-x b` / `n` / `p`
    keys, modeline position, theme toggle (persisted to `localStorage`)
  - `ds/` — Jylhis design system CSS (`tokens.css`, `fonts.css`,
    `colors_and_type.css`, `motion.css`) and self-hosted variable woff2
    fonts (fontsource 5.2.8, latin + latin-ext slices)
  - `_headers` — long-lived immutable caching for fonts, basic security
    headers (honored by Cloudflare's static-asset serving)
- `wrangler.jsonc` — Cloudflare Workers static-assets config

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

`deploy.yml` builds `.#site` on every push to main and publishes the
result to the **`site` branch** (wrangler.jsonc at the root, the site
under `public/`). Deployment itself happens on the Cloudflare side via
the GitHub integration:

- **Workers Builds**: connect this repo in the Cloudflare dashboard,
  set the production branch to `site` and the root directory to `/` —
  the branch's `wrangler.jsonc` serves `public/` as static assets.
- **Cloudflare Pages** (alternative): watch the `site` branch, empty
  build command, output directory `public`.

Either way, attach the `jotain.j10s.io` custom domain to the
project/Worker in the dashboard (the `j10s.io` zone is already on
Cloudflare, so the DNS record is created automatically).

## Local preview

```
just serve-site          # full site: nix build .#site + http.server
python3 -m http.server -d website/public 8080   # shell only
```

## Conventions

- No hex literals in `site.css` — colors come from `ds/tokens.css`
  custom properties, so both themes stay in sync.
- Both themes always ship: Paper (light) is `:root`, Roast (dark) is
  `[data-theme="dark"]` on `<html>`.
- Fonts are self-hosted; no third-party requests at runtime.
