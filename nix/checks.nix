# nix/checks.nix — Flake check derivations for Jotain.
#
# Application and configuration checks live here (Nix linting, Elisp
# validation).  Dev-environment assertions live in devenv.nix enterTest.
{
  pkgs,
  src,
  treefmtCheck,
}:
let
  inherit (pkgs) lib;

  hmStubModule = {
    options = {
      assertions = lib.mkOption {
        type = lib.types.listOf lib.types.unspecified;
        default = [ ];
      };
      home.sessionVariables = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = { };
      };
      home.packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ ];
      };
      xdg.configHome = lib.mkOption {
        type = lib.types.str;
        default = "/tmp/jotain-home/.config";
      };
      xdg.configFile = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      systemd.user.services = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      systemd.user.sockets = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      launchd.agents = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      programs = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
      fonts = lib.mkOption {
        type = lib.types.attrsOf lib.types.anything;
        default = { };
      };
    };
  };

  evalHomeModule =
    jotainConfig:
    lib.evalModules {
      modules = [
        hmStubModule
        ../module.nix
        {
          services.jotain = {
            enable = true;
          }
          // jotainConfig;
        }
      ];
      specialArgs = { inherit pkgs; };
    };

  defaultModule = evalHomeModule { };
  graphicalModule = evalHomeModule {
    startWithUserSession = "graphical";
  };
  jylhisModule = evalHomeModule {
    emacsBackend = "jylhis";
  };

  # Minimal stand-in for the nix-on-droid module system so the Jotain
  # nix-on-droid module evaluates here on x86_64 (eval only — the actual
  # aarch64 activation package is built on-device, not in CI).
  nixOnDroidStubModule = {
    options = {
      assertions = lib.mkOption {
        type = lib.types.listOf lib.types.unspecified;
        default = [ ];
      };
      environment.packages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ ];
      };
      environment.sessionVariables = lib.mkOption {
        type = lib.types.attrsOf lib.types.str;
        default = { };
      };
    };
  };

  evalNixOnDroidModule =
    jotainConfig:
    lib.evalModules {
      modules = [
        nixOnDroidStubModule
        ../module-nix-on-droid.nix
        {
          services.jotain = {
            enable = true;
          }
          // jotainConfig;
        }
      ];
      specialArgs = { inherit pkgs; };
    };

  nixOnDroidModule = evalNixOnDroidModule { };
in
{
  # ── Package builds ────────────────────────────────────────────────
  packages-default = pkgs.jotainEmacsPackages;
  packages-emacs = pkgs.jotainEmacs;
  packages-jylhis-emacs = pkgs.jylhisEmacs;
  packages-info = pkgs.jotainInfo;

  # ── Option documentation ─────────────────────────────────────────
  options-doc = import ./options-doc.nix { inherit pkgs src; };

  # ── Package reference documentation ──────────────────────────────
  packages-doc = import ./packages-doc.nix { inherit pkgs src; };

  # ── Checked-in Mintlify .mdx must match the generator ────────────
  #
  # docs/configuration/package-reference.mdx is checked in so the
  # Mintlify site can serve it without running Nix. The generator in
  # nix/packages-doc.nix is the source of truth, so we verify the
  # tracked file is byte-identical to the freshly generated one.
  packages-doc-in-sync =
    let
      generated = import ./packages-doc.nix { inherit pkgs src; };
    in
    pkgs.runCommandLocal "check-packages-doc-in-sync"
      {
        inherit src;
        generatedMdx = "${generated}/package-reference.mdx";
      }
      ''
        if ! diff -u "$src/docs/configuration/package-reference.mdx" "$generatedMdx"; then
          echo "" >&2
          echo "docs/configuration/package-reference.mdx is out of sync with" >&2
          echo "the ;;; @doc markers in lisp/init-*.el." >&2
          echo "Refresh it with: just docs-refresh-packages" >&2
          exit 1
        fi
        touch $out
      '';

  # ── Vendored design system must match the pinned upstream rev ────
  #
  # website/public/ds is committed so website/public stays a no-build
  # shell, which is exactly how it drifted a whole major version behind
  # the Emacs themes built from the same repo. Both now read
  # nix/design-pin.nix, and this check makes the drift fatal.
  ds-in-sync =
    let
      dsAssets = import ./ds-assets.nix { inherit pkgs; };
    in
    pkgs.runCommandLocal "check-ds-in-sync"
      {
        inherit src dsAssets;
      }
      ''
        if ! diff -r "$dsAssets" "$src/website/public/ds"; then
          echo "" >&2
          echo "website/public/ds is out of sync with the jylhis/design rev" >&2
          echo "pinned in nix/design-pin.nix." >&2
          echo "Re-vendor it with: just ds-sync" >&2
          exit 1
        fi
        touch $out
      '';

  # ── flake.lock and devenv.lock must pin the same shared revs ─────
  #
  # nix flake check never reads devenv.lock, so without this check the
  # only gate on lock drift is PR CI's `just verify` step — and nothing
  # at all guards pushes to main/next. Dependabot's "nix" ecosystem
  # bumps flake.lock alone, which is exactly how the two drift apart.
  # Same script the Justfile recipe runs, so the two can't disagree.
  locks-in-sync =
    pkgs.runCommandLocal "check-locks-in-sync"
      {
        inherit src;
        nativeBuildInputs = [ pkgs.jq ];
      }
      ''
        # Checked separately so an untracked/renamed script reports
        # itself instead of masquerading as lock drift.
        if [ ! -f "$src/scripts/verify-locks.sh" ]; then
          echo "scripts/verify-locks.sh is missing from the flake source." >&2
          echo "If you just created it, git add it — the flake source is" >&2
          echo "tracked files only." >&2
          exit 1
        fi
        if ! bash "$src/scripts/verify-locks.sh" "$src"; then
          echo "" >&2
          echo "flake.lock and devenv.lock disagree on a shared input's rev." >&2
          echo "Re-sync them with: just sync-devenv" >&2
          exit 1
        fi
        touch $out
      '';

  # ── Home Manager module evaluation ───────────────────────────────
  module-eval =
    pkgs.runCommandLocal "check-module-eval"
      {
        defaultEditorConfigured = if defaultModule.config.home.sessionVariables ? EDITOR then "1" else "0";
        jylhisPackageName = (builtins.head jylhisModule.config.home.packages).name;
        graphicalTarget =
          if pkgs.stdenv.hostPlatform.isLinux then
            builtins.toJSON graphicalModule.config.systemd.user.services.jotain.Install.WantedBy
          else
            toString (builtins.length graphicalModule.config.launchd.agents.jotain.config.ProgramArguments);
      }
      ''
        touch $out
      '';

  # ── nix-on-droid module evaluation ───────────────────────────────
  # Eval-only: the terminal Jotain Emacs the module selects builds on
  # aarch64 (on-device), but here we only assert the module evaluates
  # and wires EDITOR through environment.sessionVariables.
  nix-on-droid-module-eval =
    pkgs.runCommandLocal "check-nix-on-droid-module-eval"
      {
        editorConfigured =
          if nixOnDroidModule.config.environment.sessionVariables ? EDITOR then "1" else "0";
        packageCount = toString (builtins.length nixOnDroidModule.config.environment.packages);
      }
      ''
        test "$editorConfigured" = "1" || { echo "EDITOR not set"; exit 1; }
        test "$packageCount" -ge 1 || { echo "no packages installed"; exit 1; }
        touch $out
      '';

  jylhis-emacs-smoke =
    pkgs.runCommandLocal "check-jylhis-emacs-smoke"
      {
        nativeBuildInputs = [ pkgs.jylhisEmacs ];
      }
      ''
        emacs --batch --eval '(princ emacs-version)' >/dev/null
        touch $out
      '';

  # Build-side equivalent of the Emacs-provenance assertions that used
  # to live in devenv.nix `enterTest`. Emacs is no longer installed in
  # the dev shell, so we verify here that the jotainEmacs derivation
  # ships the expected binaries and that they run cleanly without
  # touching anything outside the store.
  emacs-binaries =
    pkgs.runCommandLocal "check-emacs-binaries"
      {
        emacs = pkgs.jotainEmacs;
      }
      ''
        set -euo pipefail
        for bin in emacs emacsclient etags; do
          test -x "$emacs/bin/$bin" || { echo "missing $bin"; exit 1; }
        done
        "$emacs/bin/emacs" --batch --version | grep -q "GNU Emacs"

        isolated_home="$(mktemp -d)"
        HOME="$isolated_home" "$emacs/bin/emacs" --batch \
          --no-init-file --no-site-file \
          --eval '(princ (format "user-init-file=%S\n" user-init-file))' \
          --eval '(princ (format "user-emacs-directory=%S\n" user-emacs-directory))' \
          > "$isolated_home/out" 2> "$isolated_home/err"
        host_leaks=$(grep -E "/home/|/Users/" \
          "$isolated_home/out" "$isolated_home/err" \
          | grep -v "$isolated_home" || true)
        if [ -n "$host_leaks" ]; then
          echo "FAIL: emacs touched a path outside the store:"
          echo "$host_leaks"
          exit 1
        fi
        if [ -e "$isolated_home/.emacs.d" ] || [ -e "$isolated_home/.emacs" ]; then
          echo "FAIL: emacs created config under HOME=$isolated_home"
          exit 1
        fi

        touch $out
      '';

  # ── Nix formatting (via shared treefmt config) ────────────────────
  formatting = treefmtCheck;

  # ── Nix static analysis ──────────────────────────────────────────
  statix =
    pkgs.runCommandLocal "check-statix"
      {
        nativeBuildInputs = [ pkgs.statix ];
        inherit src;
      }
      ''
        cd $src
        statix check .
        touch $out
      '';

  # ── Nix dead code detection ──────────────────────────────────────
  deadnix =
    pkgs.runCommandLocal "check-deadnix"
      {
        nativeBuildInputs = [ pkgs.deadnix ];
        inherit src;
      }
      ''
        cd $src
        deadnix --fail .
        touch $out
      '';

  # ── Elisp syntax (balanced parens) ───────────────────────────────
  elisp-lint =
    pkgs.runCommandLocal "check-elisp-lint"
      {
        nativeBuildInputs = [ pkgs.jotainEmacs ];
        inherit src;
      }
      ''
        cd $src
        emacs -Q --batch --eval '
          (let ((failed nil))
            (dolist (f (append (list "early-init.el" "init.el")
                               (directory-files "lisp" t "^\\(init-.*\\|devenv\\)\\.el$")
                               (directory-files "test" t "\\.el$")))
              (condition-case err
                  (with-temp-buffer
                    (insert-file-contents f)
                    (emacs-lisp-mode)
                    (check-parens)
                    (message "OK: %s" (file-name-nondirectory f)))
                (error
                 (message "FAIL %s: %S" (file-name-nondirectory f) err)
                 (setq failed t))))
            (when failed (kill-emacs 1)))'
        touch $out
      '';

  # ── Elisp byte-compilation (warnings as errors) ─────────────────
  elisp-compile =
    pkgs.runCommandLocal "check-elisp-compile"
      {
        nativeBuildInputs = [ pkgs.jotainEmacsPackages ];
        inherit src;
      }
      ''
        cp -r $src work
        chmod -R u+w work
        cd work
        emacs --batch \
          -L lisp \
          --eval "(require 'pcre2el)" \
          --eval "(setq byte-compile-error-on-warn t)" \
          -f batch-byte-compile early-init.el init.el lisp/devenv.el lisp/init-*.el
        touch $out
      '';

  # ── Elisp unit tests (ERT, batch) ────────────────────────────────
  # Pure-function tests only: no devenv binary, no network, no
  # subprocesses — safe inside the Nix sandbox.
  elisp-test =
    pkgs.runCommandLocal "check-elisp-test"
      {
        nativeBuildInputs = [ pkgs.jotainEmacsPackages ];
        inherit src;
      }
      ''
        cd $src
        emacs --batch -L lisp -L test \
          --eval '(dolist (f (directory-files "test" t "\\.el$")) (load f nil t))' \
          -l ert -f ert-run-tests-batch-and-exit
        touch $out
      '';
}
