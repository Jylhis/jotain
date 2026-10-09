{ pkgs, ... }:

let
  # rassumfrassum (`rass`) — LSP multiplexer by João Távora that lets eglot
  # drive multiple real language servers per buffer. Pure-Python, zero
  # runtime deps; not in nixpkgs so we build it from PyPI here. Consumed by
  # lisp/init-prog.el's eglot-server-programs (TS/TSX and Python).
  rassumfrassum = pkgs.python3Packages.buildPythonApplication rec {
    pname = "rassumfrassum";
    version = "0.3.3";
    pyproject = true;
    build-system = [ pkgs.python3Packages.setuptools ];
    src = pkgs.fetchPypi {
      inherit pname version;
      hash = "sha256-Gs2Qgwafj9m1tdVcw1k4UXTbxgbS5awTCINBkb5HIhc=";
    };
    meta = {
      description = "LSP/JSONRPC multiplexer for connecting one LSP client to multiple servers";
      homepage = "https://github.com/joaotavora/rassumfrassum";
      license = pkgs.lib.licenses.gpl3Plus;
      mainProgram = "rass";
    };
  };

  # ECA (Editor Code Assistant) server binary. The eca-emacs client
  # (lisp/init-ai.el) auto-detects `eca' on PATH instead of downloading it.
  # Built inline (not via the overlay) because the dev shell's `pkgs' has no
  # overlay applied — same approach as rassumfrassum above.
  eca = import ./nix/eca-server.nix { inherit pkgs; };
in
{
  # https://devenv.sh/packages/
  packages =
    with pkgs;
    [
      # Meson build tooling.  meson-mode and apheleia use the Meson CLI for
      # formatting, and compile-multi commands assume Ninja-backed builddirs.
      meson
      ninja

      # Bazel/Starlark formatter.  bazel-mode (C-c C-f) and apheleia
      # format-on-save shell out to buildifier for BUILD/WORKSPACE/.bzl
      # buffers.
      buildifier

      # SonarLint language server for in-editor code quality analysis.
      # Start in Emacs with M-x jotain-sonarlint.
      sonarlint-ls

      # rassumfrassum (`rass`) LSP multiplexer.  init-prog.el routes TS/TSX
      # and Python eglot connections through it when this binary is on PATH.
      rassumfrassum

      # ECA server (`eca`) for the eca-emacs client.  On PATH so eca-emacs
      # uses it directly instead of downloading a server at runtime.
      eca

      # tagref (`tagref`) cross-reference checker.  Backs the tagref.el Emacs
      # integration (M-x tagref-check, xref navigation) wired in init-prog.el.
      tagref

      # Dockerfile language server (`docker-langserver`) — Eglot auto-attaches
      # it in dockerfile-mode via the entry registered in init-prog.el.
      dockerfile-language-server

      # Documentation build chain (`just info`, `just docs`).  Declared
      # here so both the recipe and interactive invocations have them on
      # PATH; the Nix derivations still pull their own copies.
      pandoc
      texinfo

      # Fonts used by the Emacs configuration (init-ui.el looks them up by name).
      # These are only active while you're inside the devenv shell; on your real
      # system they come from home-manager or equivalent.
      # BlexMono is IBM Plex Mono with Nerd Font glyphs — the Jylhis
      # design system's mono role and the first default-face candidate.
      nerd-fonts.blex-mono
      nerd-fonts.jetbrains-mono
      nerd-fonts.iosevka
      # Only the families init-ui.el probes are pulled from the Google
      # Fonts collection (variable-pitch face); the full set is ~1 GB.
      (google-fonts.override {
        fonts = [
          "Hanken Grotesk"
          "Literata"
        ];
      })
    ]
    # Virtual X server for `just screenshot` — headless capture of the
    # Nix-built Emacs so an AI agent in a CI/cloud container can see the
    # rendered frame. Linux-only: Xvfb is X11.
    ++ lib.optionals stdenv.hostPlatform.isLinux [ xvfb-run ];

  # https://devenv.sh/languages/
  languages = {
    nix.enable = true;
  };

  # https://devenv.sh/binary-caching/
  # Pull from the personal jylhis cache and nix-community (the latter
  # hosts the emacs-overlay binaries used for Emacs 31). devenv
  # automatically adds `devenv` and `nixpkgs` caches, so only the
  # project-specific ones are declared here. Pushing is opt-in and
  # configured in CI (or via devenv.local.nix).
  cachix = {
    enable = true;
    pull = [
      "jylhis"
      "nix-community"
    ];
  };

  # https://devenv.sh/integrations/claude-code/
  # Wires up Claude Code (CLI) so that running `claude` from inside
  # the devenv shell picks up the project's tooling automatically.
  claude.code.enable = true;

  # https://devenv.sh/integrations/treefmt/
  treefmt = {
    enable = true;
    config.programs = import ./nix/treefmt.nix;
  };

  # https://devenv.sh/tests/
  # The dev shell has no Emacs, so Emacs-provenance is checked build-side:
  # `checks.<system>.emacs-binaries` in nix/checks.nix builds jotainEmacs
  # and verifies its binaries exist and run cleanly without leaking host
  # config.
  #
  # The remaining shell tooling still gets a sanity check so CI's
  # `devenv test` job doesn't pass green for the wrong reason.
  enterTest = ''
    set -euo pipefail

    # Every group asserts the same thing: the binary is on PATH and resolves
    # into the Nix store (not a host install that happens to shadow it).
    check_store() {
      echo "$1 on PATH and live in the Nix store"
      shift
      for bin in "$@"; do
        real="$(readlink -f "$(command -v "$bin")")"
        case "$real" in
          /nix/store/*) ;;
          *) echo "FAIL: $bin resolved to $real"; exit 1 ;;
        esac
      done
    }

    check_store "core Nix tools"        nil nixfmt statix deadnix
    check_store "Meson build tools"     meson ninja
    check_store "SonarLint LS"          sonarlint-ls
    check_store "rassumfrassum (rass)"  rass
    check_store "docs toolchain"        pandoc makeinfo
    check_store "eca server"            eca
    check_store "tagref"                tagref

    # No runtime assertion that `emacs` is absent from the dev shell: a
    # host Emacs installed via home-manager sits under /nix/store/ and
    # would false-trip it. The build-side guarantee (jotainEmacs produces
    # working binaries) lives in `checks.<system>.emacs-binaries`.

    echo "Dev-shell tooling checks passed."
  '';
}
