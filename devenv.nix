{ pkgs, ... }:

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
  baseEmacs = import ./emacs.nix {
    inherit (pkgs.stdenv.hostPlatform) system;
    pkgs = pinned;
  };

  # Emacs Lisp packages not available on any archive (MELPA, GNU ELPA,
  # NonGNU ELPA). Nix provides them on load-path so use-package finds
  # them without touching the network; the :vc keyword in each
  # use-package block serves as a fallback for non-Nix installs.
  epkgs = (pinned.emacsPackagesFor baseEmacs).overrideScope (
    efinal: _eprev: {
      claude-code-ide = efinal.trivialBuild {
        pname = "claude-code-ide";
        version = "0.2.6";
        src = pinned.fetchFromGitHub {
          owner = "manzaltu";
          repo = "claude-code-ide.el";
          rev = "5f12e60c6d2d1802c8c1b7944bbdf935d5db1364";
          sha256 = "148xcrqff6khpwf8nnadcyvz8h6mk45xz1498k0wbzy80yzd2axn";
        };
        packageRequires = with efinal; [
          websocket
          web-server
        ];
        # transient is built-in to Emacs 30+
      };

      combobulate = efinal.trivialBuild {
        pname = "combobulate";
        version = "0-unstable-2026-01-26";
        src = pinned.fetchFromGitHub {
          owner = "mickeynp";
          repo = "combobulate";
          rev = "38773810b5e532f25d11c6d1af02c3a8dffeacd7";
          sha256 = "0j647m17bwj4hia32nq650z7bpnxcg5bflk0z8r867qzmg8j6vc1";
        };
        # All dependencies (seq, map, treesit) are built-in to Emacs 30+
      };
    }
  );

  jotainEmacs = epkgs.withPackages (ep: [
    ep.claude-code-ide
    ep.combobulate
    ep.treesit-grammars.with-all-grammars
  ]);
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
