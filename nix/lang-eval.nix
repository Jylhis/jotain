# nix/lang-eval.nix — Per-language IDE-feature evaluation.
#
# Reads etc/lang-eval/jotain-lang-registry.el (the declarative per-language
# feature standard) and produces three artifacts:
#
#   lang-eval-doc     the declared matrix as language-support.org, which the
#                     manual includes (nix/info-manual.nix). Needs no config
#                     or toolchain.
#   lang-eval-matrix  Tier-1 live matrix: the full config is loaded and
#                     introspected per language (mode routing, tree-sitter
#                     readiness, resolved eglot server, formatter, on-PATH
#                     markers). Fails on a routing or explicit eglot
#                     override regression (JOTAIN_LANG_EVAL_STRICT).
#   lang-eval-live    Tier-2 probe: a real eglot session per fixture project
#                     for a curated subset whose servers it bundles,
#                     recording the negotiated capabilities.
#
# matrix and live are heavy, so they are `legacyPackages`, not part of
# `nix flake check`.
{
  pkgs,
}:
let
  inherit (pkgs) lib;
  fs = lib.fileset;
  root = ../.;

  elIn = dir: fs.fileFilter (f: lib.hasSuffix ".el" f.name) (root + dir);

  # Registry + renderers + probes, narrowed against unrelated edits.
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

  # The interpreted config for `--init-directory`, as `just run-built`
  # launches it.
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

  # Servers for the live probe. nixd/qmlls/likec4 already ride the
  # distribution wrapper (nix/runtime-deps.nix).
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

  # 1. Declared matrix -> Org (registry only).
  lang-eval-doc =
    pkgs.runCommand "jotain-lang-eval-doc"
      {
        nativeBuildInputs = [ emacsBare ];
        meta.description = "Language-support reference (Org) generated from the registry";
      }
      ''
        set -euo pipefail
        export HOME="$(mktemp -d)"
        mkdir -p "$out"
        JOTAIN_LANG_DOC_OUT="$out/language-support.org" \
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
in
{
  inherit
    lang-eval-doc
    lang-eval-matrix
    lang-eval-live
    ;
}
