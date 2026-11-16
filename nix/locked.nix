# The root inputs of flake.lock as store paths, for the entry points that run
# without the flake command (default.nix, overlay.nix, emacs.nix).
let
  lock = builtins.fromJSON (builtins.readFile ../flake.lock);
in
builtins.mapAttrs (
  _: key:
  let
    l = lock.nodes.${key}.locked;
  in
  builtins.fetchTarball {
    url = "https://github.com/${l.owner}/${l.repo}/archive/${l.rev}.tar.gz";
    sha256 = l.narHash;
  }
) lock.nodes.${lock.root}.inputs
