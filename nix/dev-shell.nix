# Tooling only, no Emacs: build and launch with `just run-built`. `pkgs`
# must carry the Jotain overlay (for `eca`).
{ pkgs, treefmt }:
pkgs.mkShell {
  name = "jotain-dev";

  packages = with pkgs; [
    nil
    nixfmt
    statix
    deadnix
    treefmt

    meson
    ninja
    buildifier
    sonarlint-ls

    rassumfrassum
    eca
    tagref
    dockerfile-language-server

    pandoc
    texinfo

    just
    jq
    changie
    convco
    prek
    gitleaks

    # Fonts init-ui.el probes by name.
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

    xvfb-run
  ];
}
