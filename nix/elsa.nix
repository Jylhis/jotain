{
  pkgs,
  emacs,
}:
let
  emacsWithElsa = (pkgs.emacsPackagesFor emacs).withPackages (epkgs: [ epkgs.elsa ]);
in
pkgs.writeShellScriptBin "elsa" ''
  exec ${emacsWithElsa}/bin/emacs --batch --no-init-file --no-site-file \
    --eval "(setq gc-cons-threshold 100000000)" \
    --eval "(setq gc-cons-percentage 0.1)" \
    --load=elsa --funcall=elsa-run "$@"
''
