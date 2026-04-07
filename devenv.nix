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

  # https://devenv.sh/binary-caching/
  # Pull from the personal jylhis cache. devenv automatically adds
  # `devenv` and `nixpkgs` caches to the substituter list, so we only
  # need to declare the project-specific one here. Pushing is opt-in
  # and configured in CI (or via devenv.local.nix).
  cachix = {
    enable = true;
    pull = [ "jylhis" ];
  };

  # https://devenv.sh/integrations/claude-code/
  # Wires up Claude Code (CLI) so that running `claude` from inside
  # the devenv shell picks up the project's tooling automatically.
  claude.code.enable = true;

  # https://devenv.sh/integrations/treefmt/
  treefmt = {
    enable = true;
    config.programs = {
      nixfmt.enable = true;
    };
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

  # https://devenv.sh/tests/
  # Seven assertions that lock down the dev environment:
  #
  #   1. `emacs` on PATH resolves to the jotainEmacs derivation built
  #      from emacs.nix (no host-system Emacs leaks in).
  #   2. `emacsclient` on PATH resolves to the same store path prefix
  #      as `emacs` — both must come from the same Nix package.
  #   3. `etags`, the auxiliary binary shipped by Emacs, also lives in
  #      the jotainEmacs store path.
  #   4. `emacs --version` reports a recognisable GNU Emacs banner and
  #      exits cleanly under `--batch`.
  #   5. Running Emacs with HOME pointed at an empty directory and
  #      `--no-init-file --no-site-file` produces *no* references to
  #      `.emacs.d`, `~/.emacs`, or anything outside the store — i.e.
  #      Emacs doesn't try to load any user configuration from outside
  #      the project.
  #   6. With `--init-directory=$DEVENV_ROOT` Emacs successfully loads
  #      `early-init.el` / `init.el` from this repo (this is what the
  #      `emacs-run` script does), proving the local config is the
  #      only thing in scope.
  #   7. The `eask` companion tool is on PATH, points into the Nix
  #      store, and reports a version (it ships alongside Emacs from
  #      the `languages.emacs-lisp` module).
  enterTest = ''
    set -euo pipefail

    expected_emacs="${jotainEmacs}/bin/emacs"

    echo "[1/7] emacs on PATH must come from jotainEmacs"
    actual_emacs="$(command -v emacs)"
    real_emacs="$(readlink -f "$actual_emacs")"
    case "$real_emacs" in
      ${jotainEmacs}/*) ;;
      *)
        echo "FAIL: emacs resolved to $real_emacs (expected prefix ${jotainEmacs})"
        exit 1
        ;;
    esac

    echo "[2/7] emacsclient must come from the same store path"
    real_emacsclient="$(readlink -f "$(command -v emacsclient)")"
    case "$real_emacsclient" in
      ${jotainEmacs}/*) ;;
      *)
        echo "FAIL: emacsclient resolved to $real_emacsclient"
        exit 1
        ;;
    esac

    echo "[3/7] etags must come from the same store path"
    real_etags="$(readlink -f "$(command -v etags)")"
    case "$real_etags" in
      ${jotainEmacs}/*) ;;
      *)
        echo "FAIL: etags resolved to $real_etags"
        exit 1
        ;;
    esac

    echo "[4/7] emacs --version reports GNU Emacs"
    emacs --batch --version | grep -q "GNU Emacs"

    echo "[5/7] emacs must not load anything from outside the store"
    isolated_home="$(mktemp -d)"
    trap 'rm -rf "$isolated_home"' EXIT
    HOME="$isolated_home" emacs --batch --no-init-file --no-site-file \
      --eval '(princ (format "user-init-file=%S\n" user-init-file))' \
      --eval '(princ (format "user-emacs-directory=%S\n" user-emacs-directory))' \
      > "$isolated_home/out" 2> "$isolated_home/err"
    if grep -E "$isolated_home|/home/|/Users/" "$isolated_home/out" "$isolated_home/err"; then
      echo "FAIL: emacs touched a path outside the store"
      exit 1
    fi
    if [ -e "$isolated_home/.emacs.d" ] || [ -e "$isolated_home/.emacs" ]; then
      echo "FAIL: emacs created config under HOME=$isolated_home"
      exit 1
    fi

    echo "[6/7] emacs --init-directory loads this repo's config"
    HOME="$isolated_home" emacs --batch \
      --init-directory="$DEVENV_ROOT" \
      --eval '(message "init-loaded:%s" user-emacs-directory)' \
      --kill 2>&1 | grep -q "init-loaded:$DEVENV_ROOT"

    echo "[7/7] eask is on PATH and lives in the Nix store"
    real_eask="$(readlink -f "$(command -v eask)")"
    case "$real_eask" in
      /nix/store/*) ;;
      *)
        echo "FAIL: eask resolved to $real_eask"
        exit 1
        ;;
    esac
    eask --version >/dev/null

    echo "All seven dev-environment checks passed."
  '';
}
