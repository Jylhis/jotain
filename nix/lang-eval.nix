# nix/lang-eval.nix — Per-language IDE-feature evaluation.
#
# Reads etc/lang-eval/jotain-lang-registry.el (the declarative per-language
# feature standard) and produces three artifacts plus one gate:
#
#   • lang-eval-doc      the *declared* matrix rendered to a Mintlify `.mdx`
#                        (docs/reference/language-support.mdx). Cheap and
#                        deterministic: it needs neither the config nor any
#                        toolchain, so it is checked in and gated.
#   • lang-eval-matrix   the Tier-1 *live* matrix — the full Jotain config is
#                        loaded and introspected per language (actual mode
#                        routing, tree-sitter readiness, the eglot server eglot
#                        resolves, formatter mapping, on-PATH markers). Fails
#                        the build if a language's mode routing or an explicit
#                        eglot override regresses (JOTAIN_LANG_EVAL_STRICT).
#   • lang-eval-live     the Tier-2 end-to-end probe — a real eglot session is
#                        started against a fixture project for a curated subset
#                        whose servers this derivation bundles, and the
#                        negotiated LSP capabilities are recorded.
#   • lang-eval-doc-in-sync  a flake check: the checked-in `.mdx` must match a
#                        fresh render of the registry (like packages-doc-in-sync).
#
# lang-eval-matrix and lang-eval-live load the full config in batch and (for
# live) bundle heavy toolchains, so they are exposed as `legacyPackages`
# (buildable with `nix build .#lang-eval-matrix` / `.#lang-eval-live`) and are
# NOT part of `nix flake check`. The gates that run in CI are the cheap
# lang-eval-doc-in-sync check here and the ERT drift guard (test/lang-eval-test.el,
# via the elisp-test check).
{
  pkgs,
}:
let
  inherit (pkgs) lib;
  fs = lib.fileset;
  root = ../.;

  elIn = dir: fs.fileFilter (f: lib.hasSuffix ".el" f.name) (root + dir);

  # The evaluation engine: registry + renderers + probes. Narrowed so an
  # unrelated repo edit never invalidates these derivations.
  engineSrc = fs.toSource {
    root = root + "/etc/lang-eval";
    fileset = fs.unions [
      (root + "/etc/lang-eval/jotain-lang-registry.el")
      (root + "/etc/lang-eval/jotain-lang-doc.el")
      (root + "/etc/lang-eval/jotain-lang-eval.el")
      (root + "/etc/lang-eval/jotain-lang-live.el")
    ];
  };

  # Fixture projects for the Tier-2 live probe.
  fixturesSrc = fs.toSource {
    root = root + "/etc/lang-eval";
    fileset = root + "/etc/lang-eval/fixtures";
  };

  # The interpreted config tree for `--init-directory` (copied writable at
  # build time, the same shape `just run-built` launches).
  configSrc = fs.toSource {
    inherit root;
    fileset = fs.unions [
      (root + "/early-init.el")
      (root + "/init.el")
      (elIn "/lisp")
      (root + "/templates")
    ];
  };

  emacsFull = pkgs.jotainEmacsPackages; # wrapper: full closure + tree-sitter grammars
  emacsBare = pkgs.jotainEmacs; # enough to render the registry doc

  # Curated language servers/toolchains for the live probe. nixd/qmlls/likec4
  # already ride the distribution wrapper (nix/runtime-deps.nix), so nix is
  # covered without listing a server here; the rest are added explicitly.
  liveServers = with pkgs; [
    basedpyright
    ruff
    gopls
    go
    bash-language-server
    dockerfile-language-server
    yaml-language-server
    typescript-language-server
    typescript
    nodejs
  ];

  # 1. Declared matrix -> .mdx (registry only).
  lang-eval-doc =
    pkgs.runCommand "jotain-lang-eval-doc"
      {
        nativeBuildInputs = [ emacsBare ];
        meta.description = "Language-support reference (.mdx) generated from the registry";
      }
      ''
        set -euo pipefail
        export HOME="$(mktemp -d)"
        mkdir -p "$out"
        JOTAIN_LANG_DOC_OUT="$out/language-support.mdx" \
          emacs -Q --batch -L ${engineSrc} -l jotain-lang-doc
      '';

  # 2. Tier-1 live matrix (full config load), strict routing gate.
  lang-eval-matrix =
    pkgs.runCommand "jotain-lang-eval-matrix"
      {
        nativeBuildInputs = [
          emacsFull
          pkgs.pandoc
        ];
        meta.description = "Live per-language capability matrix (config introspection)";
      }
      ''
        set -euo pipefail
        export HOME="$(mktemp -d)"
        cp -r --no-preserve=mode ${configSrc}/. "$HOME/config"
        chmod -R u+w "$HOME/config"
        mkdir -p "$out"
        if ! JOTAIN_LANG_EVAL_OUT="$out" JOTAIN_LANG_EVAL_STRICT=1 \
             emacs --batch --init-directory="$HOME/config" \
               -L ${engineSrc} -l jotain-lang-eval > "$out/generate.log" 2>&1; then
          echo "lang-eval: strict gate failed — see the log:" >&2
          cat "$out/generate.log" >&2
          exit 1
        fi
        pandoc "$out/matrix.md" -f gfm -o "$out/index.html" --standalone \
          --metadata title="Jotain — Language support matrix" --wrap=none || true
      '';

  # 3. Tier-2 end-to-end live LSP probe (bundles the curated servers).
  lang-eval-live =
    pkgs.runCommand "jotain-lang-eval-live"
      {
        nativeBuildInputs = [
          emacsFull
          pkgs.git
        ]
        ++ liveServers;
        meta.description = "End-to-end live LSP probe for the curated language subset";
      }
      ''
        set -euo pipefail
        export HOME="$(mktemp -d)"
        cp -r --no-preserve=mode ${configSrc}/. "$HOME/config"
        chmod -R u+w "$HOME/config"
        mkdir -p "$out"
        JOTAIN_LANG_EVAL_OUT="$out" JOTAIN_LANG_LIVE=1 \
        JOTAIN_LANG_FIXTURES="${fixturesSrc}/fixtures" \
          emacs --batch --init-directory="$HOME/config" \
            -L ${engineSrc} -l jotain-lang-live > "$out/generate.log" 2>&1 || true
        if [ ! -e "$out/live.md" ]; then
          echo "lang-eval-live: probe produced no output — see the log:" >&2
          cat "$out/generate.log" >&2
          exit 1
        fi
      '';

  # 4. In-sync gate: the checked-in .mdx must match a fresh render.
  lang-eval-doc-in-sync =
    pkgs.runCommandLocal "check-lang-eval-doc-in-sync"
      {
        tracked = root + "/docs/reference/language-support.mdx";
        generated = "${lang-eval-doc}/language-support.mdx";
      }
      ''
        if ! diff -u "$tracked" "$generated"; then
          echo "" >&2
          echo "docs/reference/language-support.mdx is out of sync with" >&2
          echo "etc/lang-eval/jotain-lang-registry.el." >&2
          echo "Refresh it with: just docs-refresh-lang-matrix" >&2
          exit 1
        fi
        touch "$out"
      '';
in
{
  inherit
    lang-eval-doc
    lang-eval-matrix
    lang-eval-live
    lang-eval-doc-in-sync
    ;
}
