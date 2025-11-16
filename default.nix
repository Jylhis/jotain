{ pkgs ? import <nixpkgs> { config.allowUnfree = true; }
, emacs ? pkgs.emacs
, devMode ? false
,
}:
let
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
    ]
  );
in
emacsWithPackages.overrideAttrs (oldAttrs: {
  passthru = (oldAttrs.passthru or { }) // {
    # Smoke tests - ultra-fast validation (< 1 second)
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
      pkgs.runCommand "jotain-smoke-tests" { } ''
        ${emacsWithPackages}/bin/emacs -Q --batch \
          --eval "(setq user-emacs-directory \"${testSources}/\")" \
          --eval "(add-to-list 'load-path \"${testSources}/elisp\")" \
          --eval "(add-to-list 'load-path \"${testSources}/tests\")" \
          --eval "(require 'ert)" \
          --load "${testSources}/tests/test-jotain.el" \
          --eval "(ert-run-tests-batch-and-exit '(tag smoke))"
        touch $out
      '';

    # Full tests - comprehensive validation
    tests =
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
      pkgs.runCommand "jotain-tests"
        {
          buildInputs = [ pkgs.git ];
        }
        ''
          # Create temporary home directory
          export HOME=$(mktemp -d)
          export XDG_CONFIG_HOME="$HOME/.config"
          export XDG_CACHE_HOME="$HOME/.cache"
          export XDG_DATA_HOME="$HOME/.local/share"

          # Setup emacs directory
          mkdir -p "$HOME/.emacs.d"
          cp -r ${testSources}/* "$HOME/.emacs.d/" || true

          echo "Running Jotain tests..."
          ${emacsWithPackages}/bin/emacs -Q --batch \
            --eval "(setq user-emacs-directory \"$HOME/.emacs.d/\")" \
            --eval "(add-to-list 'load-path (expand-file-name \"elisp\" user-emacs-directory))" \
            --eval "(add-to-list 'load-path (expand-file-name \"tests\" user-emacs-directory))" \
            --eval "(require 'ert)" \
            --load "$HOME/.emacs.d/tests/test-jotain.el" \
            --eval "(ert-run-tests-batch-and-exit)"
          echo "All tests passed!"
          touch $out
        '';
  };
})
