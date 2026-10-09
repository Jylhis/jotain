# nix/emacs-api-doc.nix — Generated Emacs Lisp API reference.
#
# Drives the forked elisp-doc engine (etc/elisp-doc/, vendored from
# https://codeberg.org/gudzpoz/elisp-doc) over the packages this config
# bundles, producing a page per function, command, variable, user option
# and face.
#
# Per-package caching: a package bump rebuilds only that package's
# fragment.
#   • jotain-api-doc-<name>  runs a minimal doc Emacs (bare Emacs + that
#     package and its deps + engine tooling) in "package" mode, emitting
#     its per-symbol pages, html/pkg/<name>.html, md/<name>.md and an
#     enriched `listing.eld`. It deliberately avoids the combined package
#     closure, whose store path changes on any bump.
#   • jotain-emacs-api-doc  unions the fragments and runs a tooling-only
#     Emacs in "aggregate" mode for the global indexes (symbols.json,
#     fun/var/face indexes, shortdoc.html) and index.html. Its closure is
#     stable across package bumps, so this step is cheap.
#
# Outputs:
#   • $out/html/           the browsable tree, mounted at ${mountPath} by
#                          nix/site.nix.
#   • $out/jotain-elisp-api.texi   Texinfo fragment (not yet @included by
#                          docs/jotain.texi).
#   • passthru.perPackage  the per-package derivations, keyed by feature.
{
  pkgs,
  src ? ../.,
  # Where nix/site.nix mounts $out/html. Absolute site paths in the
  # generated HTML (/style.css, /emacs.css) are rewritten to this prefix.
  mountPath ? "/help/api",
}:
let
  texi = import ./texi-fragment.nix;

  inherit (pkgs) lib;

  # The same package set legacyPackages.emacs-packages exposes. `lispDir`
  # derives from `src` (the flake passes src = self, the same store tree).
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

  # Just the engine files, so unrelated edits never invalidate the
  # fragments. Path literals, not `src + "/…"`: lib.fileset rejects the
  # string-like flake `src`.
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

  # Plain `runCommand`, not runCommandLocal, so cachix can substitute
  # these (see nix/checks.nix).
  docFor =
    { feature, pkg }:
    let
      docEmacs = scope.withPackages (_: [ pkg ] ++ toolingPkgs scope);
      featureFile = pkgs.writeText "jotain-doc-feature-${feature}" (feature + "\n");
    in
    pkgs.runCommand "jotain-api-doc-${feature}"
      {
        # magit/forge/magit-todos shell out to git while loading.
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

  # Sorted by feature, giving a deterministic union.
  perPackage = lib.listToAttrs (
    map (p: {
      name = p.feature;
      value = docFor p;
    }) pairs
  );
  perPackageList = lib.attrValues perPackage;

  # Tooling only, so its closure never changes when a package bumps.
  aggEmacs = scope.withPackages (_: toolingPkgs scope);
in
pkgs.runCommand "jotain-emacs-api-doc"
  {
    nativeBuildInputs = [ pkgs.pandoc ];
    passthru = {
      texinfoFragment = "jotain-elisp-api.texi";
      inherit featureNames perPackage;
      # The tooling-only aggregate Emacs used for the doc passes.
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

    # 1. Union the fragments. Sorted order + `cp -n` (first writer wins)
    #    keeps colliding symbol pages reproducible. `--no-preserve=mode`:
    #    otherwise the store's read-only dirs block the next fragment.
    : > listings.txt
    for d in ${lib.concatStringsSep " " perPackageList}; do
      if [ -d "$d/html" ]; then cp -rn --no-preserve=mode "$d/html/." "$out/html/" || true; fi
      if [ -e "$d/listing.eld" ]; then printf '%s\n' "$d/listing.eld" >> listings.txt; fi
    done

    # Move html/md out: it feeds the texi fragment, never the site.
    if [ -d "$out/html/md" ]; then mv "$out/html/md" "$out/md"; fi
    chmod -R u+w "$out/html"
    if [ -d "$out/md" ]; then chmod -R u+w "$out/md"; fi

    # 2. Aggregate mode over the merged listings. shortdoc.html covers
    #    only the built-in `shortdoc--groups' (package-defined groups are
    #    missed), the price of independence from package bumps.
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

    # 3. Stylesheets, with absolute links rewritten to the mount path.
    cp ${elispDir}/style.css "$out/html/style.css"
    cp ${elispDir}/emacs.css "$out/html/emacs.css"
    find "$out/html" -name '*.html' -type f -print0 \
      | xargs -0 sed -i \
          -e 's|href="/style.css"|href="${mountPath}/style.css"|g' \
          -e 's|href="/emacs.css"|href="${mountPath}/emacs.css"|g'

    # 4. Texinfo fragment: per-package markdown under one H1, converted
    #    and cleaned with nix/texi-fragment.nix.
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
    | awk '${texi.stripScaffolding}' \
    | sed -E '${texi.flattenRefs}' \
      > "$out/jotain-elisp-api.texi"

    touch "$out/html/.nojekyll"
  ''
