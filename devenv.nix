{ pkgs, lib, ... }:

let
  sources = import ./npins;

  # The npins-managed nixpkgs. Anything in this file that wants "the
  # pinned nixpkgs" should use `pinned` instead of the ambient `pkgs`
  # (which is whatever devenv resolved from devenv.yaml).
  pinned = import sources.nixpkgs-unstable {
    inherit (pkgs.stdenv.hostPlatform) system;
    config.allowUnfree = true;
  };

  # Build Emacs from our local emacs.nix (the same builder the
  # `just build` recipes use). This is what the devenv shell ships,
  # so the editor in dev matches whatever the build flavours produce.
  # For the full distribution including ~275 tree-sitter grammars,
  # use `import ./default.nix { ... }` instead.
  jotainEmacs = import ./emacs.nix {
    inherit (pkgs.stdenv.hostPlatform) system;
    pkgs = pinned;
  };
in
{
  # The custom emacs-lisp language module lives in nix/. Importing it
  # adds `languages.emacs-lisp` to the option tree below.
  imports = [ ./nix/devenv-emacs-lisp.nix ];

  # Expose `pinned` to other modules that might want it.
  _module.args.pinned = pinned;

  # https://devenv.sh/packages/
  packages = with pkgs; [
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
  languages = {
    nix.enable = true;

    # Provides emacs + eask-cli. lsp (ellsp) and elsa are off by
    # default — both have `default = true' in the module's
    # mkEnableOption, so we have to flip them off explicitly. Toggle
    # to `true' if you want them in your shell.
    emacs-lisp = {
      enable = true;
      # Override the module's emacs-nox default with the local
      # emacs.nix build, defined in the let-binding above.
      package = jotainEmacs;
      lsp.enable = false;
      elsa.enable = false;
    };
  };

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
