# nix/spell-conf.nix: the ASPELL_CONF value for services.emacsConfig.spell.dictionaries.
# The same dict-dir/data-dir pair as the bundled default in nix/mk-overlay.nix,
# but built from the given dictionary packages. Returns null for an empty
# list, so the distribution wrapper keeps its bundled en/fi/de/fr default.
{ pkgs, dictionaries }:
if dictionaries == [ ] then
  null
else
  let
    env = pkgs.aspellWithDicts (_: dictionaries);
  in
  "dict-dir ${env}/lib/aspell; data-dir ${env}/lib/aspell"
