# ASPELL_CONF for aspell dictionaries. aspellWithDicts puts libaspell's data
# files and the dictionaries in one `lib/aspell`, and jinx needs both
# dict-dir and data-dir pointed at it. Null for an empty list.
{ pkgs, dictionaries }:
if dictionaries == [ ] then
  null
else
  let
    env = pkgs.aspellWithDicts (_: dictionaries);
  in
  "dict-dir ${env}/lib/aspell; data-dir ${env}/lib/aspell"
