# nix/info-manual.nix — Build the bundled Info manual for Jotain.
#
# Converts the Markdown/MDX sources under docs/ to a single jotain.info
# manual that Emacs discovers via the normal Info-directory-list search.
#
# Pipeline:
#   docs/*.mdx  --preprocess-->  plain GFM  --pandoc-->  .texi fragment
#                                                               |
#                    docs/jotain.texi (@include ...)  <---------+
#                                |
#                             makeinfo
#                                |
#                           jotain.info + install-info dir entry
#
# The options and package references come in as Texinfo fragments from
# nix/options-doc.nix and nix/packages-doc.nix.
#
# Usage:
#   nix build .#info
#   info --file=result/share/info/jotain.info
{
  pkgs,
  lib ? pkgs.lib,
  src ? ../.,
}:
let
  texi = import ./texi-fragment.nix;

  # Only docs/ is read; narrowing also avoids files Nix refuses to copy
  # (e.g. sockets).
  docsSrc = lib.fileset.toSource {
    root = src;
    # docs/reviews and docs/design are working notes, not chapters:
    # leaving them out keeps their edits from changing jotainInfo, and
    # with it every distribution's store path.
    fileset =
      lib.fileset.intersection
        (lib.fileset.difference (lib.fileset.maybeMissing (src + "/docs")) (
          lib.fileset.unions [
            (lib.fileset.maybeMissing (src + "/docs/reviews"))
            (lib.fileset.maybeMissing (src + "/docs/design"))
          ]
        ))
        (
          lib.fileset.fileFilter (
            f: lib.hasSuffix ".md" f.name || lib.hasSuffix ".mdx" f.name || f.name == "jotain.texi"
          ) src
        );
  };

  optionsDoc = import ./options-doc.nix { inherit pkgs src; };
  packagesDoc = import ./packages-doc.nix { inherit pkgs src; };

  # Chapters of jotain.texi: { src (relative to docs/), node (its
  # @node in jotain.texi), out (the @include name) }. Order mirrors
  # docs/docs.json.
  chapters = [
    {
      src = "introduction.mdx";
      node = "Introduction";
      out = "introduction.texi";
    }
    {
      src = "installation.mdx";
      node = "Installation";
      out = "installation.texi";
    }
    {
      src = "quickstart.mdx";
      node = "Quickstart";
      out = "quickstart.texi";
    }
    {
      src = "architecture/overview.mdx";
      node = "Architecture Overview";
      out = "architecture-overview.texi";
    }
    {
      src = "architecture/nix-build.mdx";
      node = "Nix Build Layer";
      out = "architecture-nix-build.texi";
    }
    {
      src = "architecture/modules.mdx";
      node = "Module Reference";
      out = "architecture-modules.texi";
    }
    {
      src = "configuration/init.mdx";
      node = "Init File";
      out = "configuration-init.texi";
    }
    {
      src = "configuration/early-init.mdx";
      node = "Early Init";
      out = "configuration-early-init.texi";
    }
    {
      src = "configuration/packages.mdx";
      node = "Packages";
      out = "configuration-packages.texi";
    }
    # configuration/package-reference.mdx is skipped: packages-doc.nix's
    # own texinfo fragment is copied in below.
    {
      src = "usage/launching.mdx";
      node = "Launching Emacs";
      out = "usage-launching.texi";
    }
    {
      src = "usage/devenv.mdx";
      node = "Devenv Integration";
      out = "usage-devenv.texi";
    }
    {
      src = "usage/review.mdx";
      node = "Review Queue";
      out = "usage-review.texi";
    }
    {
      src = "usage/notebooks.mdx";
      node = "Notebooks";
      out = "usage-notebooks.texi";
    }
    {
      src = "usage/ai-screenshot.mdx";
      node = "AI Screenshots";
      out = "usage-ai-screenshot.texi";
    }
    {
      src = "finding-information-in-emacs.mdx";
      node = "Finding Information in Emacs";
      out = "finding-information-in-emacs.texi";
    }
    {
      src = "compilation-mode.mdx";
      node = "Compilation Mode";
      out = "compilation-mode.texi";
    }
    {
      src = "keybindings.mdx";
      node = "Keybindings";
      out = "keybindings.texi";
    }
    {
      src = "setting-variables.mdx";
      node = "Setting Variables";
      out = "setting-variables.texi";
    }
    {
      src = "ergonomics.mdx";
      node = "Ergonomics";
      out = "ergonomics.texi";
    }
    {
      src = "inspiration.mdx";
      node = "Inspiration";
      out = "inspiration.texi";
    }
  ];

  # Links between pages use the docs site's routes ([x](/usage/launching)),
  # which pandoc keeps as @uref{/usage/launching,x}. Point each route at its
  # chapter node; package-reference is the packages-doc.nix fragment.
  packageReference = {
    src = "configuration/package-reference.mdx";
    node = "Package Reference";
  };
  routeRef =
    c: "s|@uref\\{/${lib.removeSuffix ".mdx" c.src}(#[^,}]*)?,([^}]*)\\}|@ref{${c.node},,\\2}|g; ";
  routeRefs = lib.concatMapStrings routeRef (chapters ++ [ packageReference ]);

  # One `convert "docs/foo.mdx" "foo.texi"` line per chapter, emitted
  # into the build script below.
  convertLines = lib.concatMapStringsSep "\n" (
    c: "  convert ${lib.escapeShellArg c.src} ${lib.escapeShellArg c.out}"
  ) chapters;
in
pkgs.runCommand "jotain-info"
  {
    nativeBuildInputs = [
      pkgs.pandoc
      pkgs.texinfo
    ];
    src = docsSrc;
    optionsFragment = "${optionsDoc}/jotain-options.texi";
    packagesFragment = "${packagesDoc}/jotain-packages.texi";
    meta = {
      description = "Jotain Info manual (generated from docs/)";
    };
  }
  ''
        set -eu

        mkdir -p build
        cp -r "$src/docs" build/docs
        chmod -R u+w build
        cd build

        # Preprocess one Markdown/MDX file into plain GFM, then pandoc it
        # into a Texinfo fragment. The awk pass handles the MDX-isms used
        # under docs/:
        #
        #   1. YAML frontmatter is stripped.
        #   2. <Note>…</Note> (single- or multi-line) becomes a blockquote.
        #   3. The leading `# Title` is dropped: docs/jotain.texi's @chapter
        #      provides it, and the h2s then map to @section unshifted.
        convert() {
          local inpath="$1"
          local outname="$2"
          local tmpfile
          tmpfile="$(mktemp)"

          awk '
            BEGIN { in_fm = 0; fm_done = 0; stripped_title = 0 }
            # Strip a YAML frontmatter block at the very top of the file.
            NR == 1 && /^---$/ { in_fm = 1; next }
            in_fm && /^---$/   { in_fm = 0; fm_done = 1; next }
            in_fm              { next }
            # Strip the first-and-only leading `# Title` line (and the
            # optional blank line that follows it).
            !stripped_title && /^# / {
              stripped_title = 1
              getline nxt
              if (nxt !~ /^[[:space:]]*$/) print nxt
              next
            }
            !stripped_title && /^[[:space:]]*$/ { print; next }
            !stripped_title { stripped_title = 1 }
            # <Note>…</Note> -> blockquote.  Handles same-line and block forms.
            {
              line = $0
              # Same-line: <Note>text</Note>
              if (match(line, /<Note>[[:space:]]*([^<]*)[[:space:]]*<\/Note>/)) {
                gsub(/<Note>[[:space:]]*/, "> ", line)
                gsub(/[[:space:]]*<\/Note>/, "",  line)
                print line
                next
              }
              # Block-open: <Note>
              if (line ~ /^<Note>[[:space:]]*$/) { in_note = 1; next }
              if (line ~ /^<\/Note>[[:space:]]*$/) { in_note = 0; next }
              if (in_note) { print "> " line; next }
              print line
            }
          ' "$inpath" > "$tmpfile"

          # See nix/texi-fragment.nix.
          pandoc "$tmpfile" \
            -f gfm \
            -t texinfo \
            --wrap=none \
          | awk '${texi.stripScaffolding}' \
          | sed -E '${texi.flattenRefs}' \
          | sed -E '${routeRefs}' \
            > "$outname"

          rm -f "$tmpfile"
        }

        cd docs
    ${convertLines}
        cd ..

        # Put the generated fragments on the include path.
        cp "$optionsFragment"  docs/jotain-options.texi
        sed -E '${routeRefs}' "$packagesFragment" > docs/jotain-packages.texi

        # A short manual: one unsplit .info file.
        mkdir -p "$out/share/info"
        makeinfo --no-split \
          -o "$out/share/info/jotain.info" \
          docs/jotain.texi

        # A `dir` file so Emacs's Info directory merge lists the manual.
        install-info \
          --dir-file="$out/share/info/dir" \
          "$out/share/info/jotain.info"

        # HTML, one page per chapter, for the site's /manual/. The relative
        # css-ref names the manual.css installed beside the pages;
        # nix/site.nix writes its own over it.
        # makeinfo only creates the last path component itself.
        mkdir -p "$out/share/doc/jotain"
        makeinfo --html \
          --split=chapter \
          --css-ref=manual.css \
          -o "$out/share/doc/jotain/html" \
          docs/jotain.texi
        install -m444 ${./manual.css} "$out/share/doc/jotain/html/manual.css"
  ''
