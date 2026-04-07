{ pkgs, lib, ... }:

let
  sources = import ./npins;
in
{
  # Expose the npins-managed nixpkgs to the rest of the config.
  # Anything that wants "the pinned nixpkgs" should use `pinned` instead
  # of the ambient `pkgs` (which is whatever devenv resolved from devenv.yaml).
  _module.args.pinned = import sources.nixpkgs-unstable {
    inherit (pkgs.stdenv.hostPlatform) system;
    config.allowUnfree = true;
  };

  # https://devenv.sh/packages/
  packages = with pkgs; [
    # Editor — the whole point of the shell.
    emacs

    # Pinning / input management
    npins

    # Nix tooling
    nil
    nixfmt-rfc-style

    # Fonts used by the Emacs configuration (init-ui.el looks them up by name).
    # These are only active while you're inside the devenv shell; on your real
    # system they come from home-manager or equivalent.
    nerd-fonts.jetbrains-mono
    nerd-fonts.iosevka
  ];

  # https://devenv.sh/languages/
  languages.nix.enable = true;

  # https://devenv.sh/integrations/treefmt/
  treefmt = {
    enable = true;
    config.programs.nixfmt.enable = true;
  };

  # https://devenv.sh/scripts/
  scripts = {
    emacs-smoke = {
      description = "Byte-compile the whole configuration, error on any warning.";
      exec = ''
        set -euo pipefail
        emacs --batch \
          --eval "(setq byte-compile-error-on-warn t)" \
          -f batch-byte-compile early-init.el init.el lisp/*.el
      '';
    };

    emacs-run = {
      description = "Launch this configuration in isolation (ignores ~/.emacs.d).";
      exec = ''
        set -euo pipefail
        emacs --init-directory="$DEVENV_ROOT" "$@"
      '';
    };
  };

  # https://devenv.sh/tasks/
  tasks."repo:update-pins" = {
    description = "Update all npins-managed sources.";
    exec = "${pkgs.npins}/bin/npins update";
  };
}
