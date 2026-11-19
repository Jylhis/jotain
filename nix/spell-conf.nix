# nix/spell-conf.nix: the ASPELL_CONF value for aspell dictionaries.
# aspellWithDicts puts libaspell's data files and the dictionaries in one
# `lib/aspell', and jinx needs both dict-dir and data-dir pointed at it (see
# nix/mk-overlay.nix). Returns null for an empty list, so a wrapper keeps the
# distribution's bundled en/fi/de/fr default.
{ pkgs, dictionaries }:
if dictionaries == [ ] then
  null
else
  let
    env = pkgs.aspellWithDicts (_: dictionaries);
  in
  "dict-dir ${env}/lib/aspell; data-dir ${env}/lib/aspell"
