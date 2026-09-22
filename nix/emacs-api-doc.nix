# nix/emacs-api-doc.nix — Generated Emacs Lisp API reference.
#
# Forks the elisp-doc engine (etc/elisp-doc/, vendored from gudzpoz's
# https://codeberg.org/gudzpoz/elisp-doc) and drives it, scoped to the
# packages *this* configuration bundles, to produce docstring-level
# reference pages for every function, command, variable, user option and
# face those packages define.
#
# Per-package caching
# -------------------
# Generation is split into one small derivation per package plus a cheap
# aggregation derivation, so a single package bump only rebuilds that
# package's fragment (everything else substitutes from the cache):
#
#   • jotain-api-doc-<name>  runs a *minimal* doc Emacs — bare Emacs +
#     just that one package (and its declared deps) + the engine tooling —
#     in the driver's "package" mode.  It emits that package's per-symbol
#     pages (html/{fun,var,face}/…), its package index (html/pkg/<name>.html),
#     its markdown (md/<name>.md) and an enriched `listing.eld` (the summary
#     column + stylized name captured while the symbols are live).  It
#     deliberately does NOT depend on the combined package closure, whose
#     store path changes on any package bump.
#
#   • jotain-emacs-api-doc  unions the per-package trees and runs the driver
#     once more in "aggregate" mode (engine + tooling only, no packages) to
#     build the cross-package global indexes (symbols.json, the fun/var/face
#     type indexes, shortdoc.html) and the top index.html from the merged
#     listings.  This closure is stable across package bumps, so this step
#     is cheap: it only re-cp's the fragments and re-runs a tooling-only Emacs.
#
# Outputs (unchanged contract):
#   • $out/html/           the browsable site: index.html, per-package
#                          pages under pkg/, per-symbol pages under
#                          fun/ var/ face/, global fun|var|face indexes,
#                          shortdoc.html, symbols.json, style.css, emacs.css.
#                          Mounted at ${mountPath} by nix/site.nix.
#   • $out/jotain-elisp-api.texi   Texinfo fragment (@included by
#                          docs/jotain.texi as an appendix). passthru.
#   • passthru.perPackage  the individual per-package doc derivations, keyed
#                          by feature name (independently buildable/cacheable).
{
  pkgs,
  src ? ../.,
  # Where nix/site.nix mounts $out/html. Absolute site paths in the
  # generated HTML (/style.css, /emacs.css) are rewritten to this prefix.
  mountPath ? "/help/api",
}:
let
  inherit (pkgs) lib;

  # Shared resolution of the config's package set (nix/emacs-package-set.nix):
  # the lisp/ use-package scan (:ensure aliases resolved) plus the
  # Nix-provided extras — the same set legacyPackages.emacs-packages
  # exposes. The resolution logic lives there, not here. `lispDir` keeps
  # this file's `src` parameter used (the flake passes src = self, the
  # same store tree ../lisp resolves to).
  packageSet = import ./emacs-package-set.nix {
    inherit pkgs;
    lispDir = src + "/lisp";
  };
  inherit (packageSet) scope;
  pairs = packageSet.features;
  featureNames = map (f: f.feature) pairs;

  # Tooling the vendored elisp-doc engine requires, on every doc Emacs.
  toolingPkgs =
    epkgs: with epkgs; [
      helpful
      htmlize
      elisp-demos
      highlight-numbers
      highlight-quoted
      rainbow-delimiters
    ];

  # Narrow the store input to just the engine files, so an unrelated repo
  # edit (or a flake.lock bump) never invalidates the per-package fragments —
  # each fragment then depends only on {its minimal doc Emacs, engineSrc}.
  # Path literals (relative to this file) are used rather than `src + "/…"`:
  # `lib.fileset` requires a real path root, and the flake passes the
  # string-like `src = self`, which it rejects. The literals resolve inside
  # whatever store copy this file is evaluated from, and toSource emits a
  # tree keyed only on the six files' contents.
  engineSrc = lib.fileset.toSource {
    root = ../etc/elisp-doc;
    fileset = lib.fileset.unions [
      ../etc/elisp-doc/jotain-elisp-doc.el
      ../etc/elisp-doc/elisp-doc-extract.el
      ../etc/elisp-doc/elisp-doc-index.el
      ../etc/elisp-doc/elisp-doc-shortdoc.el
      ../etc/elisp-doc/style.css
      ../etc/elisp-doc/emacs.css
    ];
  };
  elispDir = "${engineSrc}";

  # A minimal doc Emacs + a runCommand per package. Plain `runCommand` (not
  # runCommandLocal) so the jylhis cachix cache can substitute these — each
  # still pulls an Emacs closure (see the reasoning in nix/checks.nix).
  docFor =
    { feature, pkg }:
    let
      docEmacs = scope.withPackages (_: [ pkg ] ++ toolingPkgs scope);
      featureFile = pkgs.writeText "jotain-doc-feature-${feature}" (feature + "\n");
    in
    pkgs.runCommand "jotain-api-doc-${feature}"
      {
        # git on PATH: magit/forge/magit-todos shell out to git while loading,
        # so `require` errors headless without it (a real Emacs always has it).
        nativeBuildInputs = [ pkgs.git ];
        meta.description = "API doc fragment for the Emacs package ${feature}";
      }
      ''
        set -eu
        export HOME="$(mktemp -d)"
        mkdir -p "$out/html"

        # Package mode: emits html/{fun,var,face,pkg}, md/, and listing.eld.
        # Load failures are trapped per-package inside the driver, so this
        # must not fail the build.
        ELISP_DOC_OUTPUT_DIR="$out/html" \
        JOTAIN_DOC_MODE=package \
        JOTAIN_PKG_FEATURES="$(cat ${featureFile})" \
        ${docEmacs}/bin/emacs --batch -q \
          -L ${elispDir} \
          -l jotain-elisp-doc 2>&1 | tee "$out/generate.log" || true

        # Guarantee a listing so the aggregate pass always has an input,
        # even if the batch Emacs died before writing one.
        [ -e "$out/listing.eld" ] \
          || echo '(:package "${feature}" :skipped nil :symbols nil)' > "$out/listing.eld"

        rm -rf "$out/html/cache"
      '';

  # Individual per-package doc derivations, keyed by feature name. Attr
  # order follows the sorted feature list, giving a deterministic union.
  perPackage = lib.listToAttrs (
    map (p: {
      name = p.feature;
      value = docFor p;
    }) pairs
  );
  perPackageList = lib.attrValues perPackage;

  # The aggregate Emacs needs only the engine tooling — no packages — so its
  # closure never changes when a package bumps.
  aggEmacs = scope.withPackages (_: toolingPkgs scope);
in
pkgs.runCommand "jotain-emacs-api-doc"
  {
    nativeBuildInputs = [ pkgs.pandoc ];
    passthru = {
      texinfoFragment = "jotain-elisp-api.texi";
      inherit featureNames perPackage;
      # Kept for attribute compatibility; now the tooling-only aggregate
      # Emacs rather than the (removed) full-closure doc Emacs.
      docEmacs = aggEmacs;
    };
    meta = {
      description = "Generated Emacs Lisp API reference for jotain's bundled packages";
    };
  }
  ''
    set -eu
    export HOME="$(mktemp -d)"
    mkdir -p "$out/html"

    # 1. Union the per-package trees. Deterministic sorted order + `cp -n`
    #    (first-writer-wins) makes colliding symbol pages reproducible.
    #    Collect the listing paths for the aggregate pass.
    # `--no-preserve=mode`: the fragments live in the read-only store, so a
    # plain `cp -r` would recreate their directories read-only and the next
    # fragment could not add files to them.
    : > listings.txt
    for d in ${lib.concatStringsSep " " perPackageList}; do
      if [ -d "$d/html" ]; then cp -rn --no-preserve=mode "$d/html/." "$out/html/" || true; fi
      if [ -e "$d/listing.eld" ]; then printf '%s\n' "$d/listing.eld" >> listings.txt; fi
    done

    # The driver writes per-package markdown under html/md; lift it out so it
    # feeds the texi fragment but is never served (matches the old monolith).
    if [ -d "$out/html/md" ]; then mv "$out/html/md" "$out/md"; fi
    chmod -R u+w "$out/html"
    if [ -d "$out/md" ]; then chmod -R u+w "$out/md"; fi

    # 2. Aggregate mode: global fun/var/face indexes, symbols.json,
    #    shortdoc.html and the top index.html, from the merged listings.
    #    No package is loaded here — only the engine and its tooling.
    #
    #    Note: shortdoc.html reflects the built-in `shortdoc--groups' only;
    #    a package-contributed `define-short-documentation-group' would be
    #    missed here (rare — shortdoc is a core facility), the price of
    #    keeping this page independent of package bumps.
    ELISP_DOC_OUTPUT_DIR="$out/html" \
    JOTAIN_DOC_MODE=aggregate \
    JOTAIN_DOC_LISTINGS="$(cat listings.txt)" \
    ${aggEmacs}/bin/emacs --batch -q \
      -L ${elispDir} \
      -l jotain-elisp-doc 2>&1 | tee "$out/generate.log" || true

    if [ ! -e "$out/html/index.html" ]; then
      echo "emacs-api-doc: aggregate produced no index.html — see generate.log" >&2
      exit 1
    fi
    rm -rf "$out/html/cache"

    # 3. Ship the stylesheets and point the absolute links at the mount path.
    cp ${elispDir}/style.css "$out/html/style.css"
    cp ${elispDir}/emacs.css "$out/html/emacs.css"
    find "$out/html" -name '*.html' -type f -print0 \
      | xargs -0 sed -i \
          -e 's|href="/style.css"|href="${mountPath}/style.css"|g' \
          -e 's|href="/emacs.css"|href="${mountPath}/emacs.css"|g'

    # 4. Texinfo fragment for the Info manual. Concatenate the per-package
    #    markdown under one H1, convert, and strip the @node/@menu/@top
    #    scaffolding + flatten @ref{} exactly as nix/packages-doc.nix does.
    {
      echo "# Emacs Package API Reference"
      echo
      if [ -d "$out/md" ]; then
        for f in $(ls "$out/md"/*.md 2>/dev/null | sort); do
          cat "$f"
          echo
        done
      fi
    } > combined.md

    pandoc combined.md \
      -f gfm \
      -t texinfo \
      --shift-heading-level-by=1 \
      --wrap=none \
    | awk '
        /^@menu$/     { in_menu = 1; next }
        /^@end menu$/ { in_menu = 0; next }
        in_menu       { next }
        /^@node /     { next }
        /^@top /      { next }
        { print }
      ' \
    | sed -E 's/@ref\{[^,}]*,,([^}]*)\}/\1/g; s/@ref\{([^}]*)\}/\1/g' \
      > "$out/jotain-elisp-api.texi"

    touch "$out/html/.nojekyll"
  ''
