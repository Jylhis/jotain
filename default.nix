{ pkgs, lib, stdenv, ... }:
let
  jotainEmacs = pkgs.callPackage ./emacs.nix { devMode = false; };

  # CLI wrapper
  jotainCLI = pkgs.writeShellScriptBin "jot" ''
    #!/usr/bin/env bash

    export JOTAIN_HOME="${placeholder "out"}"
    export JOTAIN_ELISP_DIR="${placeholder "out"}/share/emacs/site-lisp/jotain"

    # Use installed Emacs
    export PATH="${jotainEmacs}/bin:$PATH"

    # Run CLI commands
    exec "${placeholder "out"}/libexec/jotain/jot-impl" "$@"
  '';

in
stdenv.mkDerivation {
  pname = "jotain";
  version = "0.1.0";

  src = lib.cleanSource ./.;

  buildInputs = [ jotainEmacs ];

  installPhase = ''
    mkdir -p $out/share/emacs/site-lisp/jotain
    mkdir -p $out/libexec/jotain
    mkdir -p $out/bin
    mkdir -p $out/share/jotain

    # Install elisp files if they exist
    if [ -d elisp ]; then
      cp -r elisp/* $out/share/emacs/site-lisp/jotain/
    fi

    # Install init files (substitute Nix store paths)
    cp init.el $out/share/jotain/
    substituteInPlace $out/share/jotain/init.el \
      --replace-fail '@sitelisp@' "$out/share/emacs/site-lisp/jotain"
    cp early-init.el $out/share/jotain/

    # Install CLI if it exists
    if [ -d cli ]; then
      cp -r cli/* $out/libexec/jotain/
      chmod +x $out/libexec/jotain/jot 2>/dev/null || true
      # Create symlink to main CLI
      if [ -f $out/libexec/jotain/jot ]; then
        ln -s $out/libexec/jotain/jot $out/bin/jot
      fi
    fi
  '';

  meta = with lib; {
    description = "A NixOS-native Emacs distribution";
    homepage = "https://github.com/Jylhis/jotain";
    license = licenses.gpl3Plus;
    platforms = platforms.all;
    maintainers = [ ];
  };
  passthru = {
    # Expose runtime dependencies (from jotainEmacs) for use by home-manager module
    inherit (jotainEmacs) cliTools lspServers treeSitterGrammars allRuntimeDeps;

    # Test sources (shared across all test targets)
    testSources = pkgs.lib.fileset.toSource {
      root = ./.;
      fileset = pkgs.lib.fileset.unions [
        ./init.el
        ./early-init.el
        ./elisp
        ./tests
      ];
    };

    # Ultra-fast smoke tests (< 1 second)
    # Note: testSources is defined above in passthru, but we can't reference it in the same set
    # So we inline the path directly
    smoke-test =
      let
        testSources = pkgs.lib.fileset.toSource {
          root = ./.;
          fileset = pkgs.lib.fileset.unions [
            ./init.el
            ./early-init.el
            ./elisp
            ./tests
          ];
        };
      in
      pkgs.runCommand "emacs-smoke-tests" { } ''
        ${jotainEmacs}/bin/emacs -Q --batch \
          --eval "(setq user-emacs-directory \"${testSources}/\")" \
          --eval "(add-to-list 'load-path \"${testSources}/elisp\")" \
          --eval "(add-to-list 'load-path \"${testSources}/tests\")" \
          --eval "(require 'ert)" \
          --load "${testSources}/tests/test-helpers.el" \
          --load "${testSources}/tests/test-suite-smoke.el" \
          --eval "(ert-run-tests-batch-and-exit '(tag smoke))"
        touch $out
      '';

    # Fast unit tests (< 5 seconds, excludes slow filesystem tests)
    fast-tests =
      let
        testSources = pkgs.lib.fileset.toSource {
          root = ./.;
          fileset = pkgs.lib.fileset.unions [
            ./init.el
            ./early-init.el
            ./elisp
            ./tests
          ];
        };
      in
      pkgs.runCommand "emacs-fast-tests" { } ''
        ${jotainEmacs}/bin/emacs -Q --batch \
          --eval "(setq user-emacs-directory \"${testSources}/\")" \
          --eval "(add-to-list 'load-path \"${testSources}/elisp\")" \
          --eval "(add-to-list 'load-path \"${testSources}/tests\")" \
          --eval "(require 'ert)" \
          --load "${testSources}/tests/test-helpers.el" \
          --load "${testSources}/tests/test-suite-fast.el" \
          --eval "(ert-run-tests-batch-and-exit '(or (tag fast) (tag smoke)))"
        touch $out
      '';

    # Full test suite (includes slow tests)
    tests =
      let
        # Only include files needed for ERT tests (improves caching)
        testSources = pkgs.lib.fileset.toSource {
          root = ./.;
          fileset = pkgs.lib.fileset.unions [
            ./init.el
            ./early-init.el
            ./elisp
            ./tests
          ];
        };
      in
      pkgs.runCommand "emacs-config-tests"
        {
          buildInputs = [ pkgs.git ];
        }
        ''
          # Create a temporary home directory for the test
          export HOME=$(mktemp -d)
          export XDG_CONFIG_HOME="$HOME/.config"
          export XDG_CACHE_HOME="$HOME/.cache"
          export XDG_DATA_HOME="$HOME/.local/share"

          # Create a writable emacs directory
          mkdir -p "$HOME/.emacs.d"
          # Copy ONLY test sources (filtered via lib.fileset for better caching)
          cp -r ${testSources}/* "$HOME/.emacs.d/" || true

          echo "Running Emacs configuration tests..."
          ${jotainEmacs}/bin/emacs -Q --batch \
            --eval "(progn \
                      (setq user-emacs-directory \"$HOME/.emacs.d/\") \
                      (setq package-check-signature nil) \
                      (setq package-archives nil) \
                      (setq package-vc-heuristic-alist nil) \
                      (fset 'yes-or-no-p (lambda (&rest _) t)) \
                      (fset 'y-or-n-p (lambda (&rest _) t)) \
                      (fset 'package-vc-install-from-checkout (lambda (&rest _) nil)) \
                      (add-to-list 'load-path (expand-file-name \"elisp\" user-emacs-directory)) \
                      (add-to-list 'load-path (expand-file-name \"tests\" user-emacs-directory)))" \
            --eval "(require 'ert)" \
            --eval "(require 'cl-lib)" \
            --load "$HOME/.emacs.d/tests/test-helpers.el" \
            --load "$HOME/.emacs.d/tests/test-all.el" \
            --eval "(ert-run-tests-batch-and-exit)"
          echo "All tests passed!"
          touch $out
        '';
  };
}
