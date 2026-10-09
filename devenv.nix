{ pkgs, ... }:

let
  # rassumfrassum (`rass`): LSP multiplexer letting eglot drive several
  # servers per buffer (init-prog.el, TS/TSX and Python). Not in nixpkgs,
  # so built from PyPI.
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

  # ECA server for eca-emacs (lisp/init-ai.el). Imported directly: the dev
  # shell's `pkgs' has no overlay applied.
  eca = import ./nix/eca-server.nix { inherit pkgs; };
in
{
  # https://devenv.sh/packages/
  packages =
    with pkgs;
    [
      # apheleia formats Meson files with the Meson CLI; compile-multi
      # commands assume Ninja-backed builddirs.
      meson
      ninja

      # Bazel/Starlark formatter for bazel-mode (C-c C-f) and apheleia.
      buildifier

      # Started with M-x jotain-sonarlint.
      sonarlint-ls

      rassumfrassum
      # On PATH so eca-emacs does not download a server at runtime.
      eca
      # tagref CLI for tagref.el (init-prog.el).
      tagref
      # `docker-langserver`, registered for eglot in init-prog.el.
      dockerfile-language-server

      # Docs toolchain for interactive use; the Nix derivations bring
      # their own copies.
      pandoc
      texinfo

      # Fonts init-ui.el probes by name, active only inside the shell.
      # BlexMono (IBM Plex Mono + Nerd Font glyphs) is the first
      # default-face candidate.
      nerd-fonts.blex-mono
      nerd-fonts.jetbrains-mono
      nerd-fonts.iosevka
      # Only the families init-ui.el probes; the full set is ~1 GB.
      (google-fonts.override {
        fonts = [
          "Hanken Grotesk"
          "Literata"
        ];
      })
    ]
    # Virtual X server for `just screenshot` (Linux-only).
    ++ lib.optionals stdenv.hostPlatform.isLinux [ xvfb-run ];

  # https://devenv.sh/languages/
  languages = {
    nix.enable = true;
  };

  # https://devenv.sh/binary-caching/
  # nix-community hosts the emacs-overlay builds; devenv adds its own and
  # the nixpkgs caches itself. Pushing happens in CI (or devenv.local.nix).
  cachix = {
    enable = true;
    pull = [
      "jylhis"
      "nix-community"
    ];
  };

  # https://devenv.sh/integrations/claude-code/
  claude.code.enable = true;

  # https://devenv.sh/integrations/treefmt/
  treefmt = {
    enable = true;
    config.programs = import ./nix/treefmt.nix;
  };

  # https://devenv.sh/tests/
  # The shell has no Emacs; its binaries are checked build-side by
  # `checks.<system>.emacs-binaries`. This only checks the shell tooling.
  enterTest = ''
    set -euo pipefail

    # Each binary must be on PATH and resolve into the Nix store (not a
    # shadowing host install).
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

    # No assertion that `emacs` is absent: a home-manager Emacs also
    # lives under /nix/store/ and would false-trip it.

    echo "Dev-shell tooling checks passed."
  '';
}
