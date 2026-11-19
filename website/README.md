# website: page.jylhis.com/jotain

The landing page and docs site for Jotain, styled as an Emacs frame: a tab
bar with two buffers (`README.org`, `keybindings`), a modeline and a
minibuffer with `I-search` over the headings. It uses the Jylhis design
system (`ds/`, from the `ds-assets` package).

## Layout

- `public/index.html`: the landing page. `@README@` and `@KEYBINDINGS@`
  placeholder lines are replaced at build time by the Org exports of
  `README.org` and `docs/keybindings.org`. `@VERSION@`, `@FLAKE@`,
  `@REPO_URL@`, `@REPO@` and `@SUPPORT@` come from `release.json`.
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
`/docs/module-options/`, `/jotain.info`. `baseHref` (default `/jotain`)
prefixes every absolute URL; pass `baseHref = ""` for a root-served copy.

## Deployment and preview

`deploy.yml` publishes `.#site` to GitHub Pages on every push to main.
`preview.yml` uploads it as the workflow artifact `site-preview-pr-<N>`.
`just serve-site` builds and serves it under `/jotain/`.
