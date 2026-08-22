# nix/emacs-api-doc.nix — Generated Emacs Lisp API reference.
#
# Forks the elisp-doc engine (etc/elisp-doc/, vendored from gudzpoz's
# https://codeberg.org/gudzpoz/elisp-doc) and drives it, scoped to the
# packages *this* configuration bundles, to produce docstring-level
# reference pages for every function, command, variable, user option and
# face those packages define.
#
# Outputs:
#   • $out/html/           the browsable site: index.html, per-package
#                          pages under pkg/, per-symbol pages under
#                          fun/ var/ face/, global fun|var|face indexes,
#                          shortdoc.html, symbols.json, style.css, emacs.css.
#                          Mounted at ${mountPath} by nix/site.nix.
#   • $out/jotain-elisp-api.texi   Texinfo fragment (@included by
#                          docs/jotain.texi as an appendix). passthru.
#
# This realizes a dedicated "doc Emacs": the config's package set (scanned
# from lisp/ exactly as nix/mk-overlay.nix does) plus the extra tooling the
# vendored engine needs (helpful, htmlize, elisp-demos, highlighters). The
# tree-sitter grammar farm is intentionally omitted — docgen never needs it,
# and dropping it keeps this closure far smaller than jotainEmacsPackages.
{
  pkgs,
  src ? ../.,
  # Where nix/site.nix mounts $out/html. Absolute site paths in the
  # generated HTML (/style.css, /emacs.css) are rewritten to this prefix.
  mountPath ? "/help/api",
}:
let
  inherit (pkgs) lib;

  up = import ./use-package.nix { inherit lib; };
  extraPackages = import ./extra-packages.nix { inherit pkgs; };

  # Packages provided by Nix (not via a package archive), mirrored from
  # nix/mk-overlay.nix's `extraEmacsPackages`. They are not found by the
  # lisp/ scan, so we both add them to the doc Emacs and list them as
  # features to document.
  extraFeatureNames = [
    "claude-code-ide"
    "combobulate"
    "jylhis-emacs-themes"
    "majutsu"
    "nix-ts-mode"
    "tagref"
  ];

  # The feature list to document: every fetched (`:ensure` non-nil) package
  # from the lisp/ scan, plus the Nix-provided extras. `:ensure nil`
  # built-ins are excluded — they are core Emacs, already covered by the
  # bundled Emacs/Elisp manuals on the site.
  scanned = up.scanDirectoryWithDoc (src + "/lisp");
  allEntries = lib.concatMap (s: s.entries) scanned;
  fetchedNames = lib.unique (map (e: e.name) (lib.filter (e: !e.ensureNil) allEntries));
  featureNames = lib.sort (a: b: a < b) (lib.unique (fetchedNames ++ extraFeatureNames));
  featuresFile = pkgs.writeText "jotain-doc-features" (lib.concatStringsSep "\n" featureNames + "\n");

  # The doc Emacs: config package set + the engine's tooling deps.
  docEmacs = up.emacsWithPackagesFromUsePackage {
    config = src + "/lisp";
    package = pkgs.jotainEmacs;
    inherit (pkgs) emacsPackagesFor;
    override = extraPackages;
    extraEmacsPackages = epkgs: [
      # The Nix-provided config packages (as in mk-overlay.nix), minus the
      # grammar farm which docgen does not need.
      epkgs.claude-code-ide
      epkgs.combobulate
      epkgs.jylhis-emacs-themes
      epkgs.majutsu
      epkgs.nix-ts-mode
      epkgs.tagref
      # Tooling the vendored elisp-doc engine requires.
      epkgs.helpful
      epkgs.htmlize
      epkgs.elisp-demos
      epkgs.highlight-numbers
      epkgs.highlight-quoted
      epkgs.rainbow-delimiters
    ];
  };

  elispDir = src + "/etc/elisp-doc";
in
pkgs.runCommand "jotain-emacs-api-doc"
  {
    nativeBuildInputs = [ pkgs.pandoc ];
    passthru = {
      texinfoFragment = "jotain-elisp-api.texi";
      inherit docEmacs featureNames;
    };
    meta = {
      description = "Generated Emacs Lisp API reference for jotain's bundled packages";
    };
  }
  ''
    set -eu
    export HOME="$(mktemp -d)"
    mkdir -p "$out/html"

    # Run the batch generator. It writes html pages + md/ under
    # ELISP_DOC_OUTPUT_DIR. Package load failures are trapped per-package
    # inside the driver, so this must not fail the build.
    ELISP_DOC_OUTPUT_DIR="$out/html" \
    JOTAIN_PKG_FEATURES="$(cat ${featuresFile})" \
    ${docEmacs}/bin/emacs --batch -q \
      -L ${elispDir} \
      -l jotain-elisp-doc 2>&1 | tee "$out/generate.log" || true

    if [ ! -e "$out/html/index.html" ]; then
      echo "emacs-api-doc: generator produced no index.html — see generate.log" >&2
      exit 1
    fi

    # Separate the markdown (manual source) and drop the empty cache dir.
    if [ -d "$out/html/md" ]; then
      mv "$out/html/md" "$out/md"
    fi
    rm -rf "$out/html/cache"

    # Ship the stylesheets and point the absolute links at the mount path.
    cp ${elispDir}/style.css "$out/html/style.css"
    cp ${elispDir}/emacs.css "$out/html/emacs.css"
    find "$out/html" -name '*.html' -type f -print0 \
      | xargs -0 sed -i \
          -e 's|href="/style.css"|href="${mountPath}/style.css"|g' \
          -e 's|href="/emacs.css"|href="${mountPath}/emacs.css"|g'

    # Texinfo fragment for the Info manual. Concatenate the per-package
    # markdown under one H1, convert, and strip the @node/@menu/@top
    # scaffolding + flatten @ref{} exactly as nix/packages-doc.nix does.
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
