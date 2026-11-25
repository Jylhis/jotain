# website: emacs.j10s.io

The landing page and docs site for Jotain, styled as an Emacs frame: a tab
bar with two buffers (`README.org`, `keybindings`), a modeline and a
minibuffer with `I-search` over the headings. It uses the Jylhis design
system (`ds/`). `public/ds` is a vendored copy of the Jylhis design system
7.0.0 web assets and is replaced by a pin when 7.0.0 is published.

## Layout

- `public/index.html`: the landing page. `@README@` and `@KEYBINDINGS@`
  placeholder lines are replaced at build time by the Org exports of
  `README.org` and `docs/keybindings.org`. `@VERSION@`, `@FLAKE@`,
  `@REPO_URL@`, `@REPO@` and `@SUPPORT@` come from `release.json`.
- `public/robots.txt` names the sitemap. `public/_headers` sets the security
  headers and the `Link` header that points at `/llms.txt`.
- `public/css/site.css`: frame and landing styles. `public/css/docs.css`:
  prose styles for the ox-html markup. Colors only via design-system tokens.
- `public/js/app.js`: buffer switching, `C-s` search, theme toggle.
  `docs.js`: theme toggle on the generated pages.

## Build

`nix build .#site` (`nix/site.nix`):

1. `nix/info-manual.nix` stages `docs/*.org`, `init.el` and the generated
   `options.org` and `language-support.org` (its `docsTree`).
2. One batch Emacs run (`nix/org-export.el`, `nix/site-export.el`) exports
   each chapter that `docs/jotain.org` includes, a Module Options page and
   `README.org` to body-only HTML. The `#+INCLUDE` order of `jotain.org`
   is the navigation order. Links between chapters become site URLs and
   `info:` links point at gnu.org.
3. A shell function wraps each fragment in the page chrome.

URLs: `/docs/` (index), `/docs/<slug>/` for `docs/<slug>.org`,
`/docs/module-options/`, `/jotain.info`, `/sitemap.xml` (every page) and
`/llms.txt` (the pages with their descriptions, for AI agents). Each page has
a description, a canonical URL and Open Graph tags; the description of a
chapter is its `#+DESCRIPTION`. `baseHref` (default `""`, the site
is served at the domain root) prefixes every absolute URL and has no
trailing slash. The build also emits `404.html`.

## Deployment and preview

The site is the assets-only Cloudflare Worker `emacs-docs`, served at
https://emacs.j10s.io/. `wrangler.jsonc` in this directory holds the Worker
config. `deploy.yml` builds `.#site` on every push to main and runs
`wrangler deploy --assets` on a copy of the result, using the repository
secrets `CLOUDFLARE_API_TOKEN` and `CLOUDFLARE_ACCOUNT_ID`. It then checks
three public routes with curl. `just deploy` does the same by hand with the
same two variables in the environment.
`preview.yml` uploads the site as the workflow artifact
`site-preview-pr-<N>`. `just serve-site` builds and serves it at
http://localhost:8080/.
