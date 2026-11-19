# Non-flake entry point via flake-compat: `nix-build` builds the distribution,
# `nix-build -A emacs` the bare Emacs.
let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  flake-compat = fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/${
      lock.nodes."flake-compat".locked.rev
    }.tar.gz";
    sha256 = lock.nodes."flake-compat".locked.narHash;
  };
in
(import flake-compat { src = ./.; }).defaultNix
