# nix/packages-doc.nix — Generate the per-package reference.
#
# Walks lisp/init-*.el via nix/use-package.nix, extracts the `;;; @doc`
# block immediately above each `(use-package …)` form, and emits the
# same content in three shapes:
#
#   • $out/index.html         — standalone HTML page (for GitHub Pages,
#                                paired with options-doc/index.html).
#   • $out/jotain-packages.texi — Texinfo fragment, @included by
#                                 docs/jotain.texi as a chapter so the
#                                 reference also lives inside jotain.info.
#   • $out/package-reference.mdx — Mintlify-shaped `.mdx` for the docs
#                                  site. Checked in via
#                                  docs/configuration/package-reference.mdx;
#                                  CI verifies the in-tree copy matches.
#
# Authoring source of truth: the comments in lisp/init-*.el. Refresh the
# checked-in `.mdx` with `just docs-refresh-packages`.
{ pkgs, src }:
let
  inherit (pkgs) lib;

  up = import ./use-package.nix { inherit lib; };

  # Load-order mirrors init.el and docs/architecture/modules.mdx.
  # Files not listed here are appended at the end of the body so a
  # newly added module never silently drops off the reference.
  moduleOrder = [
    {
      file = "init-core.el";
      title = "Core baseline";
    }
    {
      file = "init-keys.el";
      title = "Global keys";
    }
    {
      file = "init-ui.el";
      title = "UI: theme, modeline, fonts";
    }
    {
      file = "init-help.el";
      title = "Help system";
    }
    {
      file = "init-docs.el";
      title = "Info manual discovery";
    }
    {
      file = "init-editing.el";
      title = "Editing primitives";
    }
    {
      file = "init-completion.el";
      title = "Minibuffer + in-buffer completion";
    }
    {
      file = "init-navigation.el";
      title = "dired, project, windows";
    }
    {
      file = "init-vc.el";
      title = "Version control";
    }
    {
      file = "init-prog.el";
      title = "prog-mode, treesit, eglot";
    }
    {
      file = "init-project.el";
      title = "Per-project commands";
    }
    {
      file = "init-ai.el";
      title = "AI assistants";
    }
    {
      file = "init-shell.el";
      title = "Shells";
    }
    {
      file = "init-systems.el";
      title = "Sysadmin tools";
    }
    {
      file = "init-tracking.el";
      title = "Activity tracking";
    }
    {
      file = "init-writing.el";
      title = "Prose & notes";
    }
    {
      file = "init-org.el";
      title = "Org";
    }
    {
      file = "init-lang-nix.el";
      title = "Nix";
    }
    {
      file = "init-lang-rust.el";
      title = "Rust";
    }
    {
      file = "init-lang-python.el";
      title = "Python";
    }
    {
      file = "init-lang-web.el";
      title = "Web (TypeScript, JS, CSS, HTML)";
    }
    {
      file = "init-lang-devops.el";
      title = "DevOps (CI, IaC, containers)";
    }
    {
      file = "init-lang-data.el";
      title = "Data formats";
    }
    {
      file = "init-lang-systems.el";
      title = "Systems (Go, C/C++, Haskell)";
    }
  ];

  # { file = entries } map.
  scanned = up.scanDirectoryWithDoc (src + "/lisp");
  byFile = lib.listToAttrs (map (s: lib.nameValuePair s.file s.entries) scanned);

  # Files we know about (in load order) plus any leftover that the
  # moduleOrder list didn't account for, in alphabetical order.
  knownFiles = map (m: m.file) moduleOrder;
  scannedFiles = lib.attrNames byFile;
  leftover = lib.filter (f: !lib.elem f knownFiles) scannedFiles;
  orderedModules =
    moduleOrder
    ++ map (file: {
      inherit file;
      title = "Other";
    }) leftover;

  # Render one (name, doc, ensureNil) entry as a markdown block.
  renderEntry =
    e:
    let
      tag = lib.optionalString e.ensureNil " *(built-in / Nix)*";
      heading = "### `${e.name}`${tag}";
      body = if e.doc == "" then "_Undocumented._" else e.doc;
    in
    "${heading}\n\n${body}\n";

  renderModule =
    m:
    let
      entries = byFile.${m.file} or [ ];
    in
    if entries == [ ] then
      ""
    else
      ''
        ## `${m.file}` — ${m.title}

      ''
      + lib.concatMapStringsSep "\n" renderEntry entries;

  # Drop empty render results before joining so a module with zero
  # use-package blocks (e.g. init-docs.el) doesn't insert a phantom
  # extra blank line between its neighbours.
  body = lib.concatStringsSep "\n" (lib.filter (s: s != "") (map renderModule orderedModules));

  intro = ''
    Auto-generated from `;;; @doc` markers immediately above each
    `(use-package …)` form in `lisp/init-*.el`. To change a description,
    edit the source file and run `just docs-refresh-packages`.

    See the module-level narrative under
    [Architecture → Modules](/architecture/modules) for higher-level
    grouping.
  '';

  combined = ''
    # Package Reference

    ${intro}
    ${body}'';

  mdxFrontmatter = ''
    ---
    title: Package Reference
    description: Every Emacs package Jotain ships, with the reason it is included
    ---

    {/* Auto-generated by nix/packages-doc.nix from `;;; @doc` markers in lisp/init-*.el. */}
    {/* Refresh with `just docs-refresh-packages`. The flake check `packages-doc-in-sync` verifies this file matches the source. */}

  '';

  combinedMd = pkgs.writeText "jotain-packages-combined.md" combined;
  mdxFront = pkgs.writeText "jotain-packages-frontmatter.mdx" mdxFrontmatter;

in
pkgs.runCommand "jotain-packages-doc"
  {
    nativeBuildInputs = [ pkgs.pandoc ];
    passthru.texinfoFragment = "jotain-packages.texi";
    meta = {
      description = "Jotain package reference (generated from ;;; @doc markers)";
    };
  }
  ''
        set -eu
        mkdir -p $out

        cp ${combinedMd} combined.md

        # Mintlify .mdx — frontmatter + body, with no extra blank line
        # the awk roundtripper would have to fix later.
        cat ${mdxFront} combined.md > $out/package-reference.mdx

        cat > $out/style.css <<'CSS'
    :root {
      --max-width: 52rem;
      --fg: #1a1a2e;
      --bg: #fafafa;
      --accent: #7C3AED;
      --code-bg: #f0f0f5;
      --border: #e2e2e8;
    }
    @media (prefers-color-scheme: dark) {
      :root {
        --fg: #e0e0e8;
        --bg: #16161e;
        --code-bg: #1e1e28;
        --border: #2a2a3a;
      }
    }
    * { box-sizing: border-box; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto,
                   "Helvetica Neue", sans-serif;
      max-width: var(--max-width);
      margin: 2rem auto;
      padding: 0 1.5rem;
      color: var(--fg);
      background: var(--bg);
      line-height: 1.65;
    }
    h1 {
      font-size: 1.6rem;
      border-bottom: 2px solid var(--accent);
      padding-bottom: 0.4rem;
      margin-top: 2.5rem;
    }
    h1:first-of-type { margin-top: 0; }
    h2 {
      font-family: "SF Mono", "Fira Code", Menlo, Consolas, monospace;
      font-size: 1rem;
      font-weight: 600;
      margin-top: 2rem;
      padding: 0.4rem 0.6rem;
      background: var(--code-bg);
      border-left: 3px solid var(--accent);
      border-radius: 0 4px 4px 0;
      overflow-x: auto;
    }
    h3 {
      font-family: "SF Mono", "Fira Code", Menlo, Consolas, monospace;
      font-size: 0.95rem;
      font-weight: 600;
      margin-top: 1.4rem;
    }
    code {
      font-family: "SF Mono", "Fira Code", Menlo, Consolas, monospace;
      background: var(--code-bg);
      padding: 0.15rem 0.35rem;
      border-radius: 3px;
      font-size: 0.88em;
    }
    pre {
      background: var(--code-bg);
      padding: 1rem 1.2rem;
      border-radius: 6px;
      overflow-x: auto;
      border: 1px solid var(--border);
    }
    pre code { background: none; padding: 0; }
    a { color: var(--accent); text-decoration: none; }
    a:hover { text-decoration: underline; }
    hr { border: none; border-top: 1px solid var(--border); margin: 2.5rem 0; }
    p { margin: 0.6rem 0; }
    CSS

        # HTML — full standalone page for the GitHub Pages site.
        pandoc combined.md \
          -f gfm \
          -o $out/index.html \
          --standalone \
          --metadata title="Jotain — Package Reference" \
          --css style.css \
          --highlight-style=kate \
          --wrap=none

        # Texinfo fragment — pandoc emits the full @node/@top/@menu
        # scaffolding that would clash with the @chapter wrapper in
        # docs/jotain.texi, so strip them and flatten any leftover
        # @ref{} into plain text. --shift-heading-level-by=1 drops the
        # `# Package Reference` H1 down one level so it becomes a
        # @section under the master @chapter.
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
          > $out/jotain-packages.texi

        touch $out/.nojekyll
  ''
