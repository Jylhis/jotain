# nix/info-manual.nix — Build the bundled Info manual for Jotain.
#
# docs/jotain.org includes every chapter. Org's texinfo exporter turns it
# into jotain.texi, makeinfo builds jotain.info and install-info writes the
# dir entry, so `C-h i d m Jotain RET` finds it. The passthru docsTree is the
# staged source tree the site build exports as well.
#
#   nix build .#info
#   info --file=result/share/info/jotain.info
{
  pkgs,
  lib ? pkgs.lib,
  src ? ../.,
}:
let
  fs = lib.fileset;

  # docs/design holds working notes, not chapters. Leaving it out keeps its
  # edits from changing jotainInfo, and with it every distribution's store
  # path.
  sources = fs.toSource {
    root = src;
    fileset = fs.unions [
      (fs.difference (fs.fileFilter (f: f.hasExt "org") (src + "/docs")) (src + "/docs/design"))
      (src + "/init.el")
      (src + "/nix/org-export.el")
    ];
  };

  optionsDoc = import ./options-doc.nix { inherit pkgs src; };
  inherit (import ./lang-eval.nix { inherit pkgs; }) lang-eval-doc;

  docsTree = pkgs.runCommand "jotain-docs-tree" { } ''
    mkdir -p $out/docs/generated
    cp ${sources}/docs/*.org $out/docs/
    cp ${sources}/init.el $out/
    cp ${optionsDoc}/options.org $out/docs/generated/
    sed '/^#+TITLE:/d' ${lang-eval-doc}/language-support.org > $out/docs/generated/language-support.org
  '';
in
pkgs.runCommand "jotain-info"
  {
    nativeBuildInputs = [
      pkgs.jotainEmacs
      pkgs.texinfo
    ];
    passthru = { inherit docsTree; };
    meta.description = "Jotain Info manual (generated from docs/)";
  }
  ''
    cp -r --no-preserve=mode ${docsTree}/. .
    cd docs
    JOTAIN_DOCS=$PWD emacs -Q --batch -l ${sources}/nix/org-export.el \
      --eval '(with-current-buffer (find-file-noselect "jotain.org")
                (org-export-to-file (quote texinfo) "jotain.texi"))'

    mkdir -p "$out/share/info"
    makeinfo --no-split -o "$out/share/info/jotain.info" jotain.texi
    install-info --dir-file="$out/share/info/dir" "$out/share/info/jotain.info"
  ''
