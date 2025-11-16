{ pkgs, ... }:

pkgs.stdenvNoCC.mkDerivation {
  pname = "jotain-config";
  version = "0.2.0";
  src = pkgs.lib.fileset.toSource {
    root = ./.;
    fileset = pkgs.lib.fileset.unions [
      ./init.el
      ./early-init.el
      ./elisp
    ];
  };
  installPhase = "cp -r $src $out";
}
