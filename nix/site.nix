# nix/site.nix — Assemble the static site for emacs.j10s.io.
#
# The landing page (website/public/index.html) and the documentation pages
# share one Org source: nix/site-export.el exports each chapter that
# docs/jotain.org includes, README.org and a Module Options page to HTML
# fragments, and this file wraps them in the page chrome.
#
#   /                 landing page; its README and keybindings buffers are
#                     the exports of README.org and docs/keybindings.org
#   /docs/            index of the chapters, in the order of the manual
#   /docs/<slug>/     one chapter, slug = docs/<slug>.org
#   /docs/module-options/  the generated options appendix
#   /jotain.info      the manual as an Info file
#   /404.html         the not-found page of the Worker
#
# The design system assets in ds/ are the vendored copy in website/public/ds.
# The site is $out/public/, the assets directory of the emacs-docs Worker.
# Preview it with `just serve-site`.
{
  pkgs,
  lib ? pkgs.lib,
  src ? ../.,
  # Prefix of every internal absolute URL, without a trailing slash. The site
  # is served at the root of emacs.j10s.io, so the default is empty.
  baseHref ? "",
}:
let
  fs = lib.fileset;
  infoManual = import ./info-manual.nix { inherit pkgs src; };
  release = builtins.fromJSON (builtins.readFile (src + "/release.json"));

  webSrc = fs.toSource {
    root = src;
    fileset = src + "/website/public";
  };

  exportSrc = fs.toSource {
    root = src;
    fileset = fs.unions [
      (src + "/README.org")
      (src + "/nix/org-export.el")
      (src + "/nix/site-export.el")
    ];
  };

  fragments =
    pkgs.runCommand "jotain-site-fragments"
      {
        nativeBuildInputs = [ pkgs.jotainEmacs ];
      }
      ''
        cp -r --no-preserve=mode ${infoManual.docsTree}/. .
        cp ${exportSrc}/README.org docs/
        cd docs
        JOTAIN_BASE=${lib.escapeShellArg baseHref} JOTAIN_DOCS=$PWD emacs -Q --batch \
          -l ${exportSrc}/nix/org-export.el -l ${exportSrc}/nix/site-export.el \
          --eval "(jotain-site-export \"$out\")"
      '';

in
pkgs.runCommand "jotain-site"
  {
    inherit webSrc;
    meta.description = "emacs.j10s.io static site (landing page + manual chapters)";
  }
  ''
    set -eu

    mkdir -p "$out/public"
    cp -r "$webSrc/website/public/." "$out/public/"
    chmod -R u+w "$out/public"
    cp ${infoManual}/share/info/jotain.info "$out/public/jotain.info"

    esc() { sed -e 's/&/\&amp;/g' -e 's/</\&lt;/g' -e 's/>/\&gt;/g'; }

    # write_page OUT TITLE BUFNAME MODE CONTENT_FILE NAV_HTML
    write_page() {
      local outfile="$1" title="$2" bufname="$3" mode="$4" content="$5" navhtml="$6"
      mkdir -p "$(dirname "$outfile")"
      local path="''${outfile#"$out/public"}"
      path="''${path%index.html}"
      {
        cat <<HEAD
    <!DOCTYPE html>
    <html lang="en">
    <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>''${title} | jotain</title>
    <link rel="canonical" href="https://emacs.j10s.io${baseHref}''${path}">
    <meta name="theme-color" media="(prefers-color-scheme: light)" content="#f4efe4">
    <meta name="theme-color" media="(prefers-color-scheme: dark)" content="#0b0b0b">
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
          <a class="tab" href="${baseHref}/#keys">keybindings</a>
          <span class="tab tab-current" aria-current="page">''${bufname}</span>
        </nav>
        <div class="tab-actions">
          <a class="gh-link" href="https://github.com/${release.repo}">github ⑂</a>
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
        <span class="modeline-version">U:jotain ${release.version}</span>
      </footer>
      <div class="minibuffer">
        <span class="echo">C-x C-f <a href="${baseHref}/docs/">/docs/</a> · <a href="${baseHref}/docs/module-options/">options</a> · <a href="${baseHref}/jotain.info">jotain.info</a> · ☾ theme</span>
      </div>
    </div>
    <script src="${baseHref}/js/docs.js"></script>
    </body>
    </html>
    FOOT
      } > "$outfile"
    }

    frag=${fragments}
    slugs=() titles=() descs=()
    while IFS=$'\t' read -r slug title desc; do
      slugs+=("$slug")
      titles+=("$(printf '%s' "$title" | esc)")
      descs+=("$(printf '%s' "$desc" | esc)")
    done < "$frag/manifest.tsv"

    index_body=$(mktemp)
    {
      echo '<h1 class="h1"><span class="star">*</span> Documentation</h1>'
      echo '<p class="docs-index-lede">The chapters of the Jotain manual, in reading order. Inside Emacs the same text is the Info manual: <code>C-h i d m Jotain RET</code>, or download <a href="${baseHref}/jotain.info">jotain.info</a>.</p>'
    } > "$index_body"

    last=$(( ''${#slugs[@]} - 1 ))
    for i in "''${!slugs[@]}"; do
      slug="''${slugs[$i]}"
      if [ "$i" -gt 0 ]; then
        nav="<a href=\"${baseHref}/docs/''${slugs[$((i - 1))]}/\">← ''${titles[$((i - 1))]}</a>"
      else
        nav="<a href=\"${baseHref}/docs/\">↑ index</a>"
      fi
      if [ "$i" -lt "$last" ]; then
        nav="$nav<a href=\"${baseHref}/docs/''${slugs[$((i + 1))]}/\">''${titles[$((i + 1))]} →</a>"
      else
        nav="$nav<a href=\"${baseHref}/docs/\">index ↑</a>"
      fi
      write_page "$out/public/docs/$slug/index.html" "''${titles[$i]}" "docs/$slug.org" "(Org)" \
        "$frag/$slug.html" "$nav"
      printf '<div class="man-entry"><a href="${baseHref}/docs/%s/">%s</a><span class="man-dots">·····································································</span><span class="man-desc">%s</span></div>\n' \
        "$slug" "''${titles[$i]}" "''${descs[$i]}" >> "$index_body"
    done
    write_page "$out/public/docs/index.html" "Documentation" "*docs dired*" "(Dired)" "$index_body" \
      '<a href="${baseHref}/#readme">← README.org</a><a href="${baseHref}/docs/introduction/">introduction →</a>'

    not_found=$(mktemp)
    {
      echo '<h1 class="h1"><span class="star">*</span> Not found</h1>'
      echo '<p class="docs-index-lede">There is no such page. Try the <a href="${baseHref}/docs/">documentation index</a>.</p>'
    } > "$not_found"
    write_page "$out/public/404.html" "Not found" "*scratch*" "(Fundamental)" "$not_found" \
      '<a href="${baseHref}/">← home</a><a href="${baseHref}/docs/">docs →</a>'

    # Landing page: release data, then the Org exports in place of the
    # placeholder lines.
    substituteInPlace "$out/public/index.html" \
      --replace-fail '@VERSION@' ${lib.escapeShellArg (lib.escapeXML release.version)} \
      --replace-fail '@FLAKE@' ${lib.escapeShellArg (lib.escapeXML "github:${release.repo}")} \
      --replace-fail '@REPO_URL@' ${lib.escapeShellArg (lib.escapeXML "https://github.com/${release.repo}")} \
      --replace-fail '@REPO@' ${lib.escapeShellArg (lib.escapeXML release.repo)} \
      --replace-fail '@SUPPORT@' ${lib.escapeShellArg (lib.escapeXML release.support)}

    sed 's|href="LICENSE"|href="https://github.com/${release.repo}/blob/main/LICENSE"|' \
      "$frag/readme.html" > readme.html
    sed -i \
      -e '/^@README@$/{r readme.html' -e 'd}' \
      -e '/^@KEYBINDINGS@$/{r '"$frag"'/keybindings.html' -e 'd}' \
      "$out/public/index.html"
  ''
