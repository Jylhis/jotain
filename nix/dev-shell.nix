# nix/dev-shell.nix: the development shell (`nix develop`, `nix-shell`).
# Tooling only, no Emacs: build and launch with `just run-built`.
#
# `pkgs` must carry the Jotain overlay (the flake's pkgsFor), for `eca`.
# `treefmt` is the wrapper built from nix/treefmt.nix.
{ pkgs, treefmt }:
pkgs.mkShell {
  name = "jotain-dev";

  packages = with pkgs; [
    nil
    nixfmt
    statix
    deadnix
    treefmt

    # apheleia formats Meson files with the Meson CLI; compile-multi
    # commands assume Ninja-backed builddirs.
    meson
    ninja

    # Bazel/Starlark formatter for bazel-mode (C-c C-f) and apheleia.
    buildifier

    # Started with M-x jotain-sonarlint.
    sonarlint-ls

    (import ./rassumfrassum.nix { inherit pkgs; })
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
    mandoc

    # Repository workflow.
    just
    jq
    changie
    convco
    prek
    gitleaks

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

    # Virtual X server for `just screenshot`.
    xvfb-run
  ];
}
