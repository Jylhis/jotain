{ pkgs ? import <nixpkgs> { config.allowUnfree = true; }
, emacs ? pkgs.emacs
, devMode ? false
,
}:
let
  # Custom packages
  work-manager = pkgs.callPackage ./lisp/work-manager {
    inherit (emacs.pkgs) trivialBuild magit org-jira;
  };

  emacsWithPackages = emacs.pkgs.withPackages (
    epkgs: with epkgs; [
      adoc-mode
      modus-themes
      sideline
      sideline-flymake
      sideline-eglot
      ansible
      auth-source-1password
      auto-dark
      avy
      awk-ts-mode
      bitbake-ts-mode
      breadcrumb
      cape
      cmake-mode
      compile-multi
      compile-multi-embark
      compile-multi-nerd-icons
      consult
      consult-compile-multi
      consult-dash
      consult-eglot
      consult-eglot-embark
      consult-flyspell
      corfu
      csv-mode
      cuda-mode
      dape
      dash-docs
      demangle-mode
      devicetree-ts-mode
      diff-hl
      diminish
      direnv
      docker-compose-mode
      dockerfile-mode
      drag-stuff
      dtrt-indent
      editorconfig
      elisp-lint
      elsa
      embark
      embark-consult
      emojify
      expand-region
      projection
      projection-multi-embark
      projection-multi
      projection-dape
      git-commit-ts-mode
      gitlab-ci-mode
      gnuplot
      go-mode
      haskell-mode
      haskell-ts-mode
      helpful
      hl-todo
      jq-ts-mode
      just-mode
      just-ts-mode
      kind-icon
      kkp
      ligature
      logview
      magit
      magit-todos
      marginalia
      markdown-mode
      mermaid-mode
      mermaid-ts-mode
      modern-cpp-font-lock
      multiple-cursors
      nerd-icons
      nerd-icons-completion
      nerd-icons-corfu
      nerd-icons-dired
      nerd-icons-ibuffer
      nix-mode
      nix-ts-mode
      # obsidian
      orderless
      org-appear
      org-jira
      org-modern
      ox-gfm
      ox-hugo
      ox-jira
      ox-slack
      package-lint
      pkgs.sqlite # consult-dash dependency
      pretty-sha-path
      rainbow-delimiters
      smartparens
      sops
      sql-indent
      ssh-config-mode
      super-save
      terraform-mode
      tree-sitter-langs
      treesit-auto
      treesit-fold
      treesit-grammars.with-all-grammars
      vertico
      vterm
      vundo
      web-mode
      web-server # For claude-code-ide
      websocket # For claude-code-ide
      wgrep
      yaml-mode
      zoxide

      # Custom packages
      work-manager
    ]
  );
in
emacsWithPackages.overrideAttrs (oldAttrs: {
  passthru = (oldAttrs.passthru or { }) // {
    # Test sources (shared across all test targets)
    testSources = pkgs.lib.fileset.toSource {
      root = ./.;
      fileset = pkgs.lib.fileset.unions [
        ./init.el
        ./early-init.el
        ./config
        ./lisp
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
            ./config
            ./lisp
            ./tests
          ];
        };
      in
      pkgs.runCommand "emacs-smoke-tests" { } ''
        ${emacsWithPackages}/bin/emacs -Q --batch \
          --eval "(setq user-emacs-directory \"${testSources}/\")" \
          --eval "(add-to-list 'load-path \"${testSources}/lisp\")" \
          --eval "(add-to-list 'load-path \"${testSources}/tests\")" \
          --eval "(add-to-list 'load-path \"${testSources}/config\")" \
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
            ./config
            ./lisp
            ./tests
          ];
        };
      in
      pkgs.runCommand "emacs-fast-tests" { } ''
        ${emacsWithPackages}/bin/emacs -Q --batch \
          --eval "(setq user-emacs-directory \"${testSources}/\")" \
          --eval "(add-to-list 'load-path \"${testSources}/lisp\")" \
          --eval "(add-to-list 'load-path \"${testSources}/tests\")" \
          --eval "(add-to-list 'load-path \"${testSources}/config\")" \
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
            ./config
            ./lisp
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
          ${emacsWithPackages}/bin/emacs -Q --batch \
            --eval "(progn \
                      (setq user-emacs-directory \"$HOME/.emacs.d/\") \
                      (setq package-check-signature nil) \
                      (setq package-archives nil) \
                      (setq package-vc-heuristic-alist nil) \
                      (fset 'yes-or-no-p (lambda (&rest _) t)) \
                      (fset 'y-or-n-p (lambda (&rest _) t)) \
                      (fset 'package-vc-install-from-checkout (lambda (&rest _) nil)) \
                      (add-to-list 'load-path (expand-file-name \"lisp\" user-emacs-directory)) \
                      (add-to-list 'load-path (expand-file-name \"tests\" user-emacs-directory)) \
                      (add-to-list 'load-path (expand-file-name \"config\" user-emacs-directory)))" \
            --eval "(require 'ert)" \
            --eval "(require 'cl-lib)" \
            --load "$HOME/.emacs.d/tests/test-helpers.el" \
            --load "$HOME/.emacs.d/tests/test-all.el" \
            --eval "(ert-run-tests-batch-and-exit)"
          echo "All tests passed!"
          touch $out
        '';
  };
})
