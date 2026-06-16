{ pkgs, ... }:

let
  # Emacs is temporarily NOT installed into the devenv shell — the
  # ~1 GB jotainEmacsPackages closure dominated `direnv allow` time.
  # The Nix-side builds (flake `packages.<system>.{default,emacs,…}`,
  # `just build*`, `nix flake check`) are unaffected; flake checks
  # still verify Emacs binaries via `checks.<system>.emacs-binaries`
  # (see nix/checks.nix). To re-enable, restore the bindings below,
  # the `imports` entry, the `languages.emacs-lisp` block, the
  # emacs-smoke/emacs-run scripts, and the Emacs-checking enterTest.
  #
  # emacsOverlay = import inputs.emacs-overlay;
  # pkgsWithOverlay = (pkgs.extend emacsOverlay).extend (import ./overlay.nix);
  # jotainEmacs = pkgsWithOverlay.jotainEmacsPackages;

  # rassumfrassum (`rass`) — LSP multiplexer by João Távora that lets eglot
  # drive multiple real language servers per buffer. Pure-Python, zero
  # runtime deps; not in nixpkgs so we build it from PyPI here. Consumed by
  # lisp/init-prog.el's eglot-server-programs (TS/TSX and Python).
  rassumfrassum = pkgs.python3Packages.buildPythonApplication rec {
    pname = "rassumfrassum";
    version = "0.3.3";
    pyproject = true;
    build-system = [ pkgs.python3Packages.setuptools ];
    src = pkgs.fetchPypi {
      inherit pname version;
      hash = "sha256-Gs2Qgwafj9m1tdVcw1k4UXTbxgbS5awTCINBkb5HIhc=";
    };
    meta = {
      description = "LSP/JSONRPC multiplexer for connecting one LSP client to multiple servers";
      homepage = "https://github.com/joaotavora/rassumfrassum";
      license = pkgs.lib.licenses.gpl3Plus;
      mainProgram = "rass";
    };
  };

  # ECA (Editor Code Assistant) server binary. The eca-emacs client
  # (lisp/init-ai.el) auto-detects `eca' on PATH instead of downloading it.
  # Built inline (not via the overlay) because the dev shell's `pkgs' has no
  # overlay applied — same approach as rassumfrassum above.
  eca = import ./nix/eca-server.nix { inherit pkgs; };
in
{
  # The custom emacs-lisp language module lives in nix/. Importing it
  # adds `languages.emacs-lisp` to the option tree below. Disabled
  # while Emacs is removed from the dev shell (see top-of-file note).
  # imports = [ ./nix/devenv-emacs-lisp.nix ];

  # https://devenv.sh/packages/
  packages = with pkgs; [
    # Nix tooling
    nil
    nixfmt-rfc-style

    # Nix linting
    statix
    deadnix

    # Meson build tooling.  meson-mode and apheleia use the Meson CLI for
    # formatting, and compile-multi commands assume Ninja-backed builddirs.
    meson
    ninja

    # SonarLint language server for in-editor code quality analysis.
    # Start in Emacs with M-x jotain-sonarlint.
    sonarlint-ls

    # rassumfrassum (`rass`) LSP multiplexer.  init-prog.el routes TS/TSX
    # and Python eglot connections through it when this binary is on PATH.
    rassumfrassum

    # ECA server (`eca`) for the eca-emacs client.  On PATH so eca-emacs
    # uses it directly instead of downloading a server at runtime.
    eca

    # tagref (`tagref`) cross-reference checker.  Backs the tagref.el Emacs
    # integration (M-x tagref-check, xref navigation) wired in init-prog.el.
    tagref

    # Dockerfile language server (`docker-langserver`) — Eglot auto-attaches
    # it in dockerfile-mode via the entry registered in init-prog.el.
    dockerfile-language-server

    # Documentation build chain (`just info`, `just docs`).  Declared
    # here so both the recipe and interactive invocations have them on
    # PATH; the Nix derivations still pull their own copies.
    pandoc
    texinfo

    # Fonts used by the Emacs configuration (init-ui.el looks them up by name).
    # These are only active while you're inside the devenv shell; on your real
    # system they come from home-manager or equivalent.
    nerd-fonts.jetbrains-mono
    nerd-fonts.iosevka
    google-fonts
  ];

  # https://devenv.sh/languages/
  languages = {
    nix.enable = true;

    # emacs-lisp language support is temporarily disabled — see
    # top-of-file note. Re-enabling: uncomment, restore the
    # `imports` entry, and the `jotainEmacs` let-binding.
    #
    # emacs-lisp = {
    #   enable = true;
    #   package = jotainEmacs;
    #   lsp.enable = false;
    #   elsa.enable = false;
    # };
  };

  # https://devenv.sh/binary-caching/
  # Pull from the personal jylhis cache and nix-community (the latter
  # hosts the emacs-overlay binaries used for Emacs 31). devenv
  # automatically adds `devenv` and `nixpkgs` caches, so only the
  # project-specific ones are declared here. Pushing is opt-in and
  # configured in CI (or via devenv.local.nix).
  cachix = {
    enable = true;
    pull = [
      "jylhis"
      "nix-community"
    ];
  };

  # https://devenv.sh/integrations/claude-code/
  # Wires up Claude Code (CLI) so that running `claude` from inside
  # the devenv shell picks up the project's tooling automatically.
  claude.code.enable = true;

  # https://devenv.sh/integrations/treefmt/
  treefmt = {
    enable = true;
    config.programs = import ./nix/treefmt.nix;
  };

  # https://devenv.sh/scripts/
  # emacs-smoke and emacs-run are disabled while Emacs is out of the
  # dev shell. Re-enable alongside the languages.emacs-lisp block.
  #
  # scripts = {
  #   emacs-smoke = {
  #     description = "Byte-compile the whole configuration, error on any warning.";
  #     exec = ''
  #       set -euo pipefail
  #       emacs --batch \
  #         --eval "(setq byte-compile-error-on-warn t)" \
  #         -f batch-byte-compile early-init.el init.el lisp/*.el
  #     '';
  #   };
  #
  #   emacs-run = {
  #     description = "Launch this configuration in isolation (ignores ~/.emacs.d).";
  #     exec = ''
  #       set -euo pipefail
  #       emacs --init-directory="$DEVENV_ROOT" "$@"
  #     '';
  #   };
  # };

  # https://devenv.sh/tests/
  # Emacs is no longer installed into the dev shell, so the previous
  # seven Emacs-provenance assertions have moved into a Nix-side flake
  # check — see `checks.<system>.emacs-binaries` in nix/checks.nix.
  # The flake-check version builds jotainEmacs and verifies binaries
  # exist + run cleanly without leaking host config, which is the
  # build-side equivalent.
  #
  # The remaining shell tooling still gets a sanity check so CI's
  # `devenv test` job doesn't pass green for the wrong reason.
  enterTest = ''
    set -euo pipefail

    echo "[1/7] core Nix tools on PATH"
    for bin in nil nixfmt statix deadnix; do
      real="$(readlink -f "$(command -v "$bin")")"
      case "$real" in
        /nix/store/*) ;;
        *) echo "FAIL: $bin resolved to $real"; exit 1 ;;
      esac
    done

    echo "[2/7] Meson build tools on PATH and live in the Nix store"
    for bin in meson ninja; do
      real="$(readlink -f "$(command -v "$bin")")"
      case "$real" in
        /nix/store/*) ;;
        *) echo "FAIL: $bin resolved to $real"; exit 1 ;;
      esac
    done

    echo "[3/7] sonarlint-ls on PATH and lives in the Nix store"
    real_sonar="$(readlink -f "$(command -v sonarlint-ls)")"
    case "$real_sonar" in
      /nix/store/*) ;;
      *) echo "FAIL: sonarlint-ls resolved to $real_sonar"; exit 1 ;;
    esac

    echo "[4/7] rassumfrassum (rass) on PATH and lives in the Nix store"
    real_rass="$(readlink -f "$(command -v rass)")"
    case "$real_rass" in
      /nix/store/*) ;;
      *) echo "FAIL: rass resolved to $real_rass"; exit 1 ;;
    esac

    echo "[5/7] docs toolchain (pandoc + makeinfo) on PATH"
    real_pandoc="$(readlink -f "$(command -v pandoc)")"
    case "$real_pandoc" in
      /nix/store/*) ;;
      *) echo "FAIL: pandoc resolved to $real_pandoc"; exit 1 ;;
    esac
    real_makeinfo="$(readlink -f "$(command -v makeinfo)")"
    case "$real_makeinfo" in
      /nix/store/*) ;;
      *) echo "FAIL: makeinfo resolved to $real_makeinfo"; exit 1 ;;
    esac

    echo "[6/7] eca server on PATH and lives in the Nix store"
    real_eca="$(readlink -f "$(command -v eca)")"
    case "$real_eca" in
      /nix/store/*) ;;
      *) echo "FAIL: eca resolved to $real_eca"; exit 1 ;;
    esac

    echo "[7/7] tagref on PATH and lives in the Nix store"
    real_tagref="$(readlink -f "$(command -v tagref)")"
    case "$real_tagref" in
      /nix/store/*) ;;
      *) echo "FAIL: tagref resolved to $real_tagref"; exit 1 ;;
    esac

    # NOTE: there's no longer a runtime assertion that `emacs` is absent
    # from the dev shell — a host Emacs installed via home-manager would
    # show up under /nix/store/ and trip the check on dev machines.
    # The build-side guarantee (jotainEmacs produces working binaries)
    # lives in `checks.<system>.emacs-binaries` in nix/checks.nix.

    echo "Dev-shell tooling checks passed."
  '';
}
