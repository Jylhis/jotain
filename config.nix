{ pkgs, ... }:
let
  ignore = pkgs.lib.fileset.unions [
    ./.claude
    ./.envrc
    ./.github
    ./.gitignore
    ./CLAUDE.md
    ./default.nix
    ./flake.lock
    ./flake.nix
    ./justfile
    ./nix
    ./nmt-tests
    ./tests
  ];
in
pkgs.stdenvNoCC.mkDerivation {
  pname = "emacs-config";
  version = "1.0.0";
  src = pkgs.lib.fileset.toSource {
    root = ./.;
    fileset = pkgs.lib.fileset.difference ./. ignore;
  };
  installPhase = "cp -r $src $out";
}
