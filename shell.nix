# shell.nix: `nix-shell` entry point, the flake's dev shell for the
# current system (see nix/dev-shell.nix).
(import ./default.nix).devShells.${builtins.currentSystem}.default
