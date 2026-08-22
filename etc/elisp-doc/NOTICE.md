# etc/elisp-doc — vendored from elisp-doc

This directory forks parts of **elisp-doc** by gudzpoz, the generator behind
<https://doc.emacsen.de/>:

- Upstream: <https://codeberg.org/gudzpoz/elisp-doc>
- License: GNU GPL, version 3 or later (same as jotain — see the top-level
  `LICENSE`).

## What is vendored, verbatim

The following files are copied unmodified from elisp-doc (each keeps its own
license header). They implement elisp-doc's technique: drive
`helpful-function` / `helpful-variable` / `describe-face` into a buffer for a
symbol, then run a conversion pipeline that turns the buffer's text properties
into standalone HTML.

- `elisp-doc-extract.el` — the helpful→HTML conversion pipeline
  (`elisp-doc-export`, `elisp-doc--convert-steps`, the symbol / manual / source
  cross-linkers).
- `elisp-doc-index.el` — the batch export driver (`elisp-doc--export-all`) and
  the JSON / per-type / per-file index-page generators.
- `elisp-doc-shortdoc.el` — the shortdoc cheatsheet page generator.
- `full-feature-lister.el` — feature enumeration, itself taken by elisp-doc
  from [Malabarba/emacs-online-documentation]
  (https://github.com/Malabarba/emacs-online-documentation) (GPL).
- `style.css` — elisp-doc's page stylesheet, itself modified from
  [Water.css](https://github.com/kognise/water.css) (MIT; the MIT notice is
  retained at the top of the file).

## What is jotain's own

- `jotain-elisp-doc.el` — the batch entry point for **this** repo. It replaces
  elisp-doc's `elisp-doc-main.el` (which documents *all* of Emacs across
  versions with tmux/socket sharding). Instead it loads only the packages this
  config bundles and exports only the symbols those packages define, scoping the
  corpus to jotain. It reuses the vendored engine above without modifying it.

Not vendored: elisp-doc's `elisp-doc-main.el`, `elisp-doc-theme.el` (theme
gallery), its `texi/` VitePress manuals site, its `build/` podman/Rust bundler,
its `specials/search.js` (imports minisearch from a CDN, incompatible with
jotain's self-contained site), and its site-specific landing pages
(`specials/helpful.html`, `specials/nil-and-t.html`). jotain renders manuals in
`nix/site.nix` and builds everything through Nix.
