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
# The options appendix comes from nix/options-doc.nix which exposes a
# Texinfo fragment alongside its HTML output.
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
  # Only the docs/ tree is consumed by makeinfo; restrict the store
  # path to it so we don't drag in irrelevant files (e.g. .git IPC
  # sockets, build artefacts) that Nix import refuses to copy.
  docsSrc = lib.fileset.toSource {
    root = src;
    fileset = lib.fileset.intersection (lib.fileset.maybeMissing (src + "/docs")) (
      lib.fileset.fileFilter (
        f: lib.hasSuffix ".md" f.name || lib.hasSuffix ".mdx" f.name || f.name == "jotain.texi"
      ) src
    );
  };

  optionsDoc = import ./options-doc.nix { inherit pkgs src; };

  # Pages that flow into @chapter sections of jotain.texi.  Order and
  # grouping mirror docs/docs.json.  Each entry is { src, out }: the
  # path relative to docs/ and the @include filename referenced from
  # docs/jotain.texi.
  chapters = [
    {
      src = "introduction.mdx";
      out = "introduction.texi";
    }
    {
      src = "installation.mdx";
      out = "installation.texi";
    }
    {
      src = "quickstart.mdx";
      out = "quickstart.texi";
    }
    {
      src = "architecture/overview.mdx";
      out = "architecture-overview.texi";
    }
    {
      src = "architecture/nix-build.mdx";
      out = "architecture-nix-build.texi";
    }
    {
      src = "architecture/modules.mdx";
      out = "architecture-modules.texi";
    }
    {
      src = "configuration/init.mdx";
      out = "configuration-init.texi";
    }
    {
      src = "configuration/early-init.mdx";
      out = "configuration-early-init.texi";
    }
    {
      src = "configuration/packages.mdx";
      out = "configuration-packages.texi";
    }
    {
      src = "finding-information-in-emacs.mdx";
      out = "finding-information-in-emacs.texi";
    }
    {
      src = "compilation-mode.mdx";
      out = "compilation-mode.texi";
    }
    {
      src = "setting-variables.mdx";
      out = "setting-variables.texi";
    }
    {
      src = "inspiration.mdx";
      out = "inspiration.texi";
    }
  ];

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
        # into a Texinfo fragment.  The preprocessor handles the only three
        # MDX-isms currently used under docs/:
        #
        #   1. YAML frontmatter (`---\n...\n---`) is stripped.
        #   2. <Note>…</Note> (single- or multi-line) becomes a blockquote.
        #   3. The leading `# Title` that every MDX page opens with is
        #      dropped — the @chapter wrapper in docs/jotain.texi already
        #      provides that heading, and removing it here means remaining
        #      h2 headings map to @section (pandoc default) without needing
        #      --shift-heading-level-by.
        convert() {
          local inpath="$1"
          local outname="$2"
          local tmpfile
          tmpfile="$(mktemp)"

          # 1+2+3 are all regex-small enough to fit in one awk pass.
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

          # Pandoc's texinfo writer emits a full @node/@top/@menu
          # structure, which clashes with the @include-into-@chapter
          # layout in docs/jotain.texi.  Strip those so only the
          # @section/@subsection sectioning survives — makeinfo then
          # assigns nodes automatically under the master's @chapter.
          # Also flatten any surviving @ref{name,,text} (they point to
          # nodes we just removed) into plain text.
          pandoc "$tmpfile" \
            -f gfm \
            -t texinfo \
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
            > "$outname"

          rm -f "$tmpfile"
        }

        cd docs
    ${convertLines}
        cd ..

        # Bring the options-doc texinfo fragment into the include search path.
        cp "$optionsFragment" docs/jotain-options.texi

        # Emit the final .info.  Jotain is a short manual — one node per
        # chapter is enough, no need for --split-size gymnastics.
        mkdir -p "$out/share/info"
        makeinfo --no-split \
          -o "$out/share/info/jotain.info" \
          docs/jotain.texi

        # Populate a `dir` file so Emacs's Info-insert-dir merger picks us
        # up.  install-info is idempotent and won't clobber other packages'
        # entries when Emacs scans a superset of info directories.
        install-info \
          --dir-file="$out/share/info/dir" \
          "$out/share/info/jotain.info"
  ''
