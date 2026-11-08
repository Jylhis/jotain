{ pkgs, lib, ... }:
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
      license = lib.licenses.gpl3Plus;
      mainProgram = "rass";
    };
  };

  # ECA server for eca-emacs (lisp/init-ai.el).
  eca = import ./nix/eca-server.nix { inherit pkgs; };

  # The Emacs tree is excluded from the root treefmt; `just fmt` runs this one.
  treefmt = (import ../../nix/inputs.nix { }).treefmt-nix.lib.mkWrapper pkgs {
    projectRootFile = "project.nix";
    programs = import ./nix/treefmt.nix;
  };
in
{
  packages =
    with pkgs;
    [
      treefmt

      # apheleia formats Meson files with the Meson CLI; compile-multi
      # commands assume Ninja-backed builddirs.
      meson
      ninja

      # Bazel/Starlark formatter for bazel-mode (C-c C-f) and apheleia.
      buildifier

      # Started with M-x my-sonarlint.
      sonarlint-ls

      rassumfrassum
      # On PATH so eca-emacs does not download a server at runtime.
      eca
      # tagref CLI for tagref.el (init-prog.el).
      tagref
      # `docker-langserver`, registered for eglot in init-prog.el.
      dockerfile-language-server

      nil

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
      jost
      fira-sans
      ibm-plex
      (google-fonts.override {
        fonts = [ "Literata" ];
      })
    ]
    # Virtual X server for `just screenshot` (Linux-only).
    ++ lib.optionals stdenv.hostPlatform.isLinux [ xvfb-run ];
}
