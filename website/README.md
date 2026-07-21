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

## Deployment

Deployment happens on the Cloudflare side via the GitHub integration —
nothing in CI here builds or pushes the site.

- **Workers Builds** (uses `wrangler.jsonc`): connect this repo in the
  Cloudflare dashboard, set the project *root directory* to `website/`.
  Every push to the production branch deploys `public/` as static assets.
- **Cloudflare Pages** (alternative): connect the repo, leave the build
  command empty, set the build output directory to `website/public`.

Either way, attach the `jotain.j10s.io` custom domain to the
project/Worker in the dashboard (the `j10s.io` zone is already on
Cloudflare, so the DNS record is created automatically).

## Local preview

The site is plain static files:

```
python3 -m http.server -d website/public 8080
# or, with wrangler:
cd website && npx wrangler dev
```

## Conventions

- No hex literals in `site.css` — colors come from `ds/tokens.css`
  custom properties, so both themes stay in sync.
- Both themes always ship: Paper (light) is `:root`, Roast (dark) is
  `[data-theme="dark"]` on `<html>`.
- Fonts are self-hosted; no third-party requests at runtime.
