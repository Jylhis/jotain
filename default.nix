# default.nix — Jotain non-flake entry point via flake-compat.
#
# Evaluates flake.nix using flake-compat (pinned in flake.lock).
# Packages for the current system are promoted to the top level, so:
#
#   nix-build                              # full distribution (jotainEmacsPackages)
#   nix-build -A emacs                     # bare Emacs only
#   nix-build emacs.nix                    # bare Emacs (cache-parity build)
#   nix-build emacs.nix --arg withPgtk true  # variant builds
let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  flake-compat = fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes."flake-compat".locked.rev}.tar.gz";
    sha256 = lock.nodes."flake-compat".locked.narHash;
  };
in
(import flake-compat { src = ./.; }).defaultNix
