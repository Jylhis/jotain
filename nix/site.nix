# nix/site.nix — Assemble the full static site for page.jylhis.com/jotain.
#
# Composes the hand-written landing SPA (website/public) with every
# document the repo can generate:
#
#   /docs/…           docs/**/*.mdx rendered to HTML (nav from docs.json)
#   /manual/          the Jotain manual, makeinfo --html (via info-manual.nix)
#   /manual/jotain.info  the same manual as an Info file
#   /man/             jotain(7) + the man pages shipped by the Emacs build
#   /info/emacs/      the GNU Emacs manual from the pinned Emacs source
#   /info/elisp/      the Emacs Lisp reference manual, same source
#   /options/         Nix module options reference (options-doc.nix)
#   /help/packages/   per-package reference (packages-doc.nix)
#
# Output layout ($out/public/ is uploaded as the GitHub Pages artifact):
#   $out/public/          the site
#
# The site is served under a base path (baseHref, default /jotain) because it
# is a GitHub Pages project site at page.jylhis.com/jotain/. Every internal
# absolute URL below carries that prefix; pass baseHref = "" to serve at a root.
#
# Usage:
#   nix build .#site -o result-site
#   # served at /jotain/, so mount it there for a faithful local preview:
#   d=$(mktemp -d); ln -s "$PWD/result-site/public" "$d/jotain"
#   python3 -m http.server -d "$d" 8080   # → http://localhost:8080/jotain/
{
  pkgs,
  lib ? pkgs.lib,
  src ? ../.,
  # Whether to mount the generated per-package API reference (etc/elisp-doc)
  # at /help/api/. This build is heavy (a batch Emacs over ~100 packages),
  # so it is on for the deployed `.#site` but OFF for the `.#site-preview`
  # target PR CI builds — keeping the PR `site` job within its time budget.
  # Guarded lazily below so `withApiDoc = false` never realizes emacsApiDoc.
  withApiDoc ? true,
  # Path prefix the site is served under. The site is a GitHub Pages project
  # site at page.jylhis.com/jotain/, so every internal absolute URL carries
  # this prefix. Set to "" to serve from a domain root.
  baseHref ? "/jotain",
}:
let
  optionsDoc = import ./options-doc.nix { inherit pkgs src; };
  packagesDoc = import ./packages-doc.nix { inherit pkgs src; };
  # Generated docstring-level API reference for the bundled packages
  # (etc/elisp-doc). Its html/ tree is mounted at /help/api/; the mount
  # path is baked into the pages' absolute stylesheet links, so keep the
  # two in sync.
  emacsApiDoc = import ./emacs-api-doc.nix {
    inherit pkgs src;
    mountPath = "${baseHref}/help/api";
  };
  # The /help/ index row for the API reference, emitted only when the
  # reference is actually mounted (see withApiDoc).
  apiHelpRow = ''<div class="man-entry"><a href="${baseHref}/help/api/">C-h S — elisp API reference</a><span class="man-dots">·····································································</span><span class="man-desc">docstrings for every bundled package</span></div>'';
  infoManual = import ./info-manual.nix { inherit pkgs src; };
  # The Emacs Jotain actually ships (emacs-unstable base, now the 31.1
  # release branch) — its man pages and manual sources feed /man and
  # /info, so the site documents the exact revision users get.
  emacs = pkgs.jotainEmacs;

  # docs navigation, from the same docs.json Mintlify uses
  docsNav = builtins.fromJSON (builtins.readFile (src + "/docs/docs.json"));
  navGroups = (builtins.head docsNav.navigation.tabs).groups;
  pagesFlat = lib.concatMap (
    g:
    map (p: {
      inherit (g) group;
      id = p;
    }) g.pages
  ) navGroups;
  pageCount = builtins.length pagesFlat;
  pageAt = i: builtins.elemAt pagesFlat i;
  withNav = lib.imap0 (
    i: p:
    p
    // {
      prev = if i == 0 then "" else (pageAt (i - 1)).id;
      next = if i == pageCount - 1 then "" else (pageAt (i + 1)).id;
    }
  ) pagesFlat;

  scanLines = lib.concatMapStringsSep "\n" (p: "  scan_page ${lib.escapeShellArg p.id}") withNav;
  renderLines = lib.concatMapStringsSep "\n" (
    p:
    "  render_page ${lib.escapeShellArg p.id} ${lib.escapeShellArg p.group} "
    + "${lib.escapeShellArg p.prev} ${lib.escapeShellArg p.next}"
  ) withNav;

  # Restrict store inputs to what the build actually reads.
  docsSrc = lib.fileset.toSource {
    root = src;
    fileset = lib.fileset.intersection (lib.fileset.maybeMissing (src + "/docs")) (
      lib.fileset.fileFilter (f: lib.hasSuffix ".md" f.name || lib.hasSuffix ".mdx" f.name) src
    );
  };
  webSrc = lib.fileset.toSource {
    root = src;
    fileset = lib.fileset.maybeMissing (src + "/website/public");
  };

  # The GNU Emacs + Elisp manuals, rendered from the exact source
  # revision the site's Emacs is built from.  Separate derivation so a
  # docs/ edit doesn't re-render ~200 chapters of upstream manual.
  emacsManualsHtml =
    pkgs.runCommand "emacs-manuals-html"
      {
        nativeBuildInputs = [ pkgs.texinfo ];
        inherit (emacs) src version;
        meta.description = "GNU Emacs and Elisp manuals as HTML (from the pinned Emacs source)";
      }
      ''
        set -eu

        # emacs.src is a release tarball on some nixpkgs revisions and a
        # bare source checkout on others — only doc/ is needed either way.
        if [ -d "$src" ]; then
          cp -r "$src/doc" doc
        else
          tar xf "$src"
          mv emacs-*/doc doc
        fi
        chmod -R u+w doc

        # Release tarballs ship the generated emacsver.texi; a git-style
        # checkout only has emacsver.texi.in. Both manuals @include it.
        [ -f doc/emacs/emacsver.texi ] \
          || printf '@set EMACSVER %s\n' "$version" > doc/emacs/emacsver.texi

        mkdir -p "$out"
        ( cd doc/emacs && makeinfo --html --split=chapter --css-ref=../manual.css \
            -o "$out/emacs" emacs.texi )
        # elisp.texi @includes docstyle.texi/emacsver.texi from doc/emacs
        # (upstream's Makefile passes the same -I).
        ( cd doc/lispref && makeinfo -I ../emacs --html --split=chapter --css-ref=../manual.css \
            -o "$out/elisp" elisp.texi )
      '';
in
pkgs.runCommand "jotain-site"
  {
    nativeBuildInputs = [
      pkgs.pandoc
      pkgs.mandoc
      pkgs.gzip
    ];
    inherit docsSrc webSrc;
    meta.description = "page.jylhis.com/jotain static site (landing SPA + generated docs)";
  }
  ''
    set -eu

    mkdir -p "$out/public"
    cp -r "$webSrc/website/public/." "$out/public/"
    chmod -R u+w "$out/public"

    # The shared design-system CSS is @import-ed with a root-absolute path;
    # rewrite it to the deploy base path. These stylesheets are copied to
    # varying depths (/manual/, /options/, /info/, /help/packages/), so a
    # relative @import cannot be uniform.
    sed -i 's|@import url("/ds/|@import url("${baseHref}/ds/|' \
      "$out/public/css/manual.css" "$out/public/css/pandoc-page.css"

    # shared page chrome
    # write_page OUT TITLE BUFNAME MODE CONTENT_FILE NAV_HTML
    # Emits the Emacs-frame chrome (tab bar, modeline, echo area)
    # around a rendered HTML fragment.
    write_page() {
      local outfile="$1" title="$2" bufname="$3" mode="$4" content="$5" navhtml="$6"
      mkdir -p "$(dirname "$outfile")"
      {
        cat <<HEAD
    <!DOCTYPE html>
    <html lang="en">
    <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>''${title} — jotain</title>
    <link rel="canonical" href="https://page.jylhis.com${baseHref}/">
    <meta name="theme-color" media="(prefers-color-scheme: light)" content="#f6f8fb">
    <meta name="theme-color" media="(prefers-color-scheme: dark)" content="#0d0f14">
    <link rel="icon" type="image/svg+xml" href="${baseHref}/favicon.svg">
    <script src="${baseHref}/js/theme-init.js"></script>
    <link rel="stylesheet" href="${baseHref}/css/site.css">
    <link rel="stylesheet" href="${baseHref}/css/docs.css">
    </head>
    <body>
    <div class="frame">
      <header class="tab-bar">
        <div class="tab-brand"><a class="tab-brand-link" href="${baseHref}/"><span class="tab-prompt">jy ❯</span></a></div>
        <nav class="tabs" aria-label="buffers">
          <a class="tab" href="${baseHref}/#readme">README.org</a>
          <a class="tab" href="${baseHref}/#man">*Man JOTAIN(7)*</a>
          <a class="tab" href="${baseHref}/#keys">keybindings</a>
          <span class="tab tab-current" aria-current="page">''${bufname}</span>
        </nav>
        <div class="tab-actions">
          <a class="gh-link" href="https://github.com/Jylhis/jotain">github ⑂</a>
          <button type="button" class="theme-btn" id="theme-btn" aria-label="toggle theme">☾</button>
        </div>
      </header>
      <main class="buffer">
        <article class="page docs-prose">
    HEAD
        cat "$content"
        cat <<FOOT
        <nav class="keys-nav docs-nav">''${navhtml}</nav>
        </article>
      </main>
      <footer class="modeline">
        <span>-UUU:**--</span>
        <span class="modeline-buffer">''${bufname}</span>
        <span class="modeline-pos">Top L1</span>
        <span>''${mode}</span>
        <span class="modeline-version">U:jotain 2026.07</span>
      </footer>
      <div class="minibuffer">
        <span class="echo">C-x C-f <a href="${baseHref}/docs/">/docs/</a> · <a href="${baseHref}/manual/">manual</a> · <a href="${baseHref}/man/">man</a> · <a href="${baseHref}/info/">info</a> · ☾ theme</span>
      </div>
    </div>
    <script src="${baseHref}/js/docs.js"></script>
    </body>
    </html>
    FOOT
      } > "$outfile"
    }

    # Make generated third-party pages (pandoc/makeinfo output) pick up
    # the favicon and the stored theme before first paint.
    inject_head() {
      sed -i 's|</head>|<link rel="icon" type="image/svg+xml" href="${baseHref}/favicon.svg"><script src="${baseHref}/js/theme-init.js"></script></head>|' "$@"
    }

    # /docs/ — rendered MDX pages
    # Preprocess one MDX file to plain GFM: strip YAML frontmatter,
    # keep the leading H1, turn <Note> blocks into blockquotes (styled
    # as callouts by docs.css).
    preprocess() {
      awk '
        BEGIN { in_fm = 0; in_note = 0 }
        NR == 1 && /^---$/ { in_fm = 1; next }
        in_fm && /^---$/   { in_fm = 0; next }
        in_fm              { next }
        {
          line = $0
          if (match(line, /<Note>[[:space:]]*([^<]*)[[:space:]]*<\/Note>/)) {
            gsub(/<Note>[[:space:]]*/, "> ", line)
            gsub(/[[:space:]]*<\/Note>/, "",  line)
            print line
            next
          }
          if (line ~ /^<Note>[[:space:]]*$/)   { in_note = 1; next }
          if (line ~ /^<\/Note>[[:space:]]*$/) { in_note = 0; next }
          if (in_note) { print "> " line; next }
          print line
        }
      ' "$1"
    }

    declare -A TITLE DESC
    scan_page() {
      local id="$1" sf="$docsSrc/docs/$1.mdx"
      TITLE[$id]="$(sed -n 's/^title:[[:space:]]*//p' "$sf" | head -1)"
      DESC[$id]="$(sed -n 's/^description:[[:space:]]*//p' "$sf" | head -1)"
      [ -n "''${TITLE[$id]}" ] || TITLE[$id]="$id"
    }
    ${"\n" + scanLines}

    docs_index_body="$(mktemp)"
    current_group=""
    render_page() {
      local id="$1" group="$2" prev="$3" next="$4"
      local sf="$docsSrc/docs/$id.mdx"
      local body nav

      body="$(mktemp)"
      preprocess "$sf" > body.md
      pandoc body.md -f gfm -t html --wrap=none --no-highlight -o "$body"
      # Mintlify-absolute page links (/architecture/modules) → site
      # paths (/docs/architecture/modules/), leaving external URLs,
      # anchors, and already-correct /docs/ links alone.
      sed -i -E \
        -e 's|href="/docs/|href="__KEEP__|g' \
        -e 's|href="/([a-zA-Z][a-zA-Z0-9/_-]*)(#[^"]*)?"|href="${baseHref}/docs/\1/\2"|g' \
        -e 's|href="__KEEP__|href="${baseHref}/docs/|g' \
        "$body"

      nav=""
      if [ -n "$prev" ]; then
        nav="<a href=\"${baseHref}/docs/$prev/\">← ''${TITLE[$prev]}</a>"
      else
        nav="<a href=\"${baseHref}/docs/\">↑ index</a>"
      fi
      if [ -n "$next" ]; then
        nav="$nav<a href=\"${baseHref}/docs/$next/\">''${TITLE[$next]} →</a>"
      else
        nav="$nav<a href=\"${baseHref}/docs/\">index ↑</a>"
      fi

      write_page "$out/public/docs/$id/index.html" \
        "''${TITLE[$id]}" "docs/$id.mdx" "(Markdown)" "$body" "$nav"

      if [ "$group" != "$current_group" ]; then
        printf '<div class="man-label">%s</div>\n' "''${group^^}" >> "$docs_index_body"
        current_group="$group"
      fi
      printf '<div class="man-entry"><a href="${baseHref}/docs/%s/">%s</a><span class="man-dots">·····································································</span><span class="man-desc">%s</span></div>\n' \
        "$id" "''${TITLE[$id]}" "''${DESC[$id]}" >> "$docs_index_body"
    }
    ${"\n" + renderLines}

    {
      echo '<h1 class="h1"><span class="star">*</span> Documentation</h1>'
      echo '<p class="docs-index-lede">Generated from the Markdown sources under <code>docs/</code> — the same sources that produce the <a href="${baseHref}/manual/">manual</a>, the <a href="${baseHref}/manual/jotain.info">Info file</a>, and <a href="${baseHref}/man/jotain.7.html">jotain(7)</a>.</p>'
      cat "$docs_index_body"
    } > index_body.html
    write_page "$out/public/docs/index.html" "Documentation" "*docs dired*" "(Dired)" index_body.html \
      '<a href="${baseHref}/#man">← JOTAIN(7)</a><a href="${baseHref}/manual/">the manual →</a>'

    # /manual/ — the Jotain manual as HTML + Info
    mkdir -p "$out/public/manual"
    cp -r "${infoManual}/share/doc/jotain/html/." "$out/public/manual/"
    chmod -R u+w "$out/public/manual"
    cp "${infoManual}/share/info/jotain.info" "$out/public/manual/jotain.info"
    cp "$out/public/css/manual.css" "$out/public/manual/manual.css"
    inject_head "$out/public/manual/"*.html

    # /options/ and /help/packages/ — pandoc standalone pages
    mkdir -p "$out/public/options" "$out/public/help/packages"
    cp "${optionsDoc}/index.html" "$out/public/options/index.html"
    cp "${packagesDoc}/index.html" "$out/public/help/packages/index.html"
    chmod u+w "$out/public/options/index.html" "$out/public/help/packages/index.html"
    # Both pages link a sibling style.css; ship the design-system
    # version instead of their bundled purple one.
    cp "$out/public/css/pandoc-page.css" "$out/public/options/style.css"
    cp "$out/public/css/pandoc-page.css" "$out/public/help/packages/style.css"
    inject_head "$out/public/options/index.html" "$out/public/help/packages/index.html"

    ${lib.optionalString withApiDoc ''
      # /help/api/ — generated per-package API reference (etc/elisp-doc).
      # The pages are self-contained and theme-aware via their own CSS
      # (style.css + emacs.css, shipped inside the tree), so they are copied
      # verbatim rather than run through inject_head over thousands of files.
      mkdir -p "$out/public/help/api"
      cp -r "${emacsApiDoc}/html/." "$out/public/help/api/"
      chmod -R u+w "$out/public/help/api"
    ''}

    # /help/ index — C-h, as a directory listing.
    cat > help_body.html <<'HELP'
    <h1 class="h1"><span class="star">*</span> Help</h1>
    <p class="docs-index-lede">The <code>C-h</code> map, rendered for the web.</p>
    <div class="man-entry"><a href="${baseHref}/help/packages/">C-h P — package reference</a><span class="man-dots">·····································································</span><span class="man-desc">every package Jotain ships, and why</span></div>
    ${lib.optionalString withApiDoc apiHelpRow}
    <div class="man-entry"><a href="${baseHref}/options/">nix options</a><span class="man-dots">·····································································</span><span class="man-desc">Home Manager · NixOS/nix-darwin · devenv</span></div>
    <div class="man-entry"><a href="${baseHref}/manual/">C-h i d m Jotain RET — the manual</a><span class="man-dots">·····································································</span><span class="man-desc">HTML, one page per chapter</span></div>
    <div class="man-entry"><a href="${baseHref}/info/">C-h i — info directory</a><span class="man-dots">·····································································</span><span class="man-desc">GNU Emacs + Elisp manuals</span></div>
    <div class="man-entry"><a href="${baseHref}/man/">M-x man — man pages</a><span class="man-dots">·····································································</span><span class="man-desc">jotain(7) and the Emacs man pages</span></div>
    HELP
    write_page "$out/public/help/index.html" "Help" "*Help*" "(Help)" help_body.html \
      '<a href="${baseHref}/#readme">← README.org</a><a href="${baseHref}/docs/">documentation →</a>'

    # /man/ — man pages via mandoc
    mkdir -p "$out/public/man"
    man_index_body="$(mktemp)"

    render_man() {
      local roff="$1" name="$2" desc="$3"
      local body
      body="$(mktemp)"
      mandoc -T html -O fragment "$roff" > "$body"
      write_page "$out/public/man/$name.html" "$name" "*Man $name*" "(Man)" "$body" \
        '<a href="${baseHref}/man/">← man index</a><a href="${baseHref}/#man">JOTAIN(7) →</a>'
      printf '<div class="man-entry"><a href="${baseHref}/man/%s.html">%s</a><span class="man-dots">·····································································</span><span class="man-desc">%s</span></div>\n' \
        "$name" "$name" "$desc" >> "$man_index_body"
    }

    pandoc "$docsSrc/docs/jotain.7.md" -s -f markdown -t man -o jotain.7
    cp jotain.7 "$out/public/man/jotain.7"
    render_man jotain.7 "jotain.7" "the Jotain manual, troff edition"

    for f in ${emacs}/share/man/man1/*.1.gz; do
      name="$(basename "$f" .gz)"
      zcat "$f" > page.roff
      desc="$(sed -n '/^\.SH NAME/{n;p;}' page.roff | head -1 \
        | sed -e 's/\\-/-/g' -e 's/^[^-]*- *//' -e 's/\\//g')"
      [ -n "$desc" ] || desc="from the Emacs distribution"
      render_man page.roff "$name" "$desc"
    done

    {
      echo '<h1 class="h1"><span class="star">*</span> Man pages</h1>'
      echo '<p class="docs-index-lede">Rendered with mandoc. The Emacs pages come from the exact Emacs build Jotain ships; <a href="${baseHref}/man/jotain.7">jotain.7</a> is also available as raw troff.</p>'
      cat "$man_index_body"
    } > man_body.html
    write_page "$out/public/man/index.html" "Man pages" "*Man apropos*" "(Man)" man_body.html \
      '<a href="${baseHref}/#man">← JOTAIN(7)</a><a href="${baseHref}/info/">info manuals →</a>'

    # /info/ — the GNU Emacs + Elisp manuals
    mkdir -p "$out/public/info"
    cp -r "${emacsManualsHtml}/emacs" "$out/public/info/emacs"
    cp -r "${emacsManualsHtml}/elisp" "$out/public/info/elisp"
    chmod -R u+w "$out/public/info"
    cp "$out/public/css/manual.css" "$out/public/info/manual.css"
    inject_head "$out/public/info/emacs/"*.html "$out/public/info/elisp/"*.html

    cat > info_body.html <<'INFO'
    <h1 class="h1"><span class="star">*</span> Info directory</h1>
    <p class="docs-index-lede">Rendered from the exact Emacs source revision Jotain builds — what <code>C-h i</code> would show you, on the web.</p>
    <div class="man-label">MANUALS</div>
    <div class="man-entry"><a href="${baseHref}/manual/">jotain</a><span class="man-dots">·····································································</span><span class="man-desc">the Jotain manual</span></div>
    <div class="man-entry"><a href="${baseHref}/info/emacs/">emacs</a><span class="man-dots">·····································································</span><span class="man-desc">the GNU Emacs manual</span></div>
    <div class="man-entry"><a href="${baseHref}/info/elisp/">elisp</a><span class="man-dots">·····································································</span><span class="man-desc">the Emacs Lisp reference manual</span></div>
    <div class="man-label">FILES</div>
    <div class="man-entry"><a href="${baseHref}/manual/jotain.info">jotain.info</a><span class="man-dots">·····································································</span><span class="man-desc">Info file — C-h i d m Jotain RET</span></div>
    INFO
    write_page "$out/public/info/index.html" "Info directory" "*info* (dir)" "(Info)" info_body.html \
      '<a href="${baseHref}/man/">← man pages</a><a href="${baseHref}/docs/">documentation →</a>'
  ''
