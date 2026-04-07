{
  description = "Jotain — a custom GNU Emacs distribution";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        # `nix build` → the wrapped Emacs distribution.  Forwards into
        # the existing default.nix so the classic build path
        # (`nix-build default.nix`) and the flake build stay in sync.
        packages = {
          default = import ./default.nix { inherit system pkgs; };
          emacs-nox = import ./default.nix {
            inherit system pkgs;
            noGui = true;
          };
          emacs-pgtk = import ./default.nix {
            inherit system pkgs;
            withPgtk = true;
          };
        };

        # `nix develop` → a thin shell with just the host tools needed
        # to drive the Justfile.  The full devenv (treefmt, eask,
        # ellsp, git-hooks, ...) still lives in `devenv.nix` and is
        # entered via `devenv shell`.
        devShells.default = pkgs.mkShell {
          packages = [
            pkgs.just
            pkgs.nix
            pkgs.emacs-nox
          ];
        };

        formatter = pkgs.nixfmt-rfc-style;
      }
    );
}
