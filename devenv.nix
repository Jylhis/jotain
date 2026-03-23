{ pkgs, lib, config, ... }:

let
  jotainEmacs = pkgs.callPackage ./emacs.nix { devMode = true; };
in
{
  imports = [
    ./nix/devenv/emacs-lisp.nix
  ];

  # Languages
  languages = {
    nix.enable = true;
    emacs-lisp = {
      enable = true;
      package = jotainEmacs;
    };
  };

  # Formatting (shared config with flake.nix)
  treefmt = {
    enable = true;
    config = import ./nix/treefmt.nix { inherit pkgs; };
  };

  # Git hooks — treefmt runs as pre-commit during `devenv test`
  git-hooks.hooks.treefmt.enable = true;

  packages = [
    # MCP servers
    pkgs.mcp-nixos
    pkgs.mcp-language-server

    # Tools
    pkgs.ripgrep
    pkgs.fd
    pkgs.git

    # Nix tools
    pkgs.nix-prefetch-git

    # Testing
    pkgs.just
  ];

  cachix.pull = [ "jylhis" ];

  # Disable container features (we don't need nix2container/mk-shell-bin)
  containers = lib.mkForce { };

  # Tests — run with `devenv test`
  enterTest = ''
    echo "Running ERT smoke and fast tests..."
    emacs -Q --batch \
      --eval "(progn \
        (add-to-list 'load-path \"$DEVENV_ROOT/lisp\") \
        (add-to-list 'load-path \"$DEVENV_ROOT/modules\") \
        (add-to-list 'load-path \"$DEVENV_ROOT/tests\"))" \
      --eval "(setq user-emacs-directory \"$DEVENV_ROOT/\")" \
      --eval "(require 'ert)" \
      --eval "(require 'cl-lib)" \
      --load "$DEVENV_ROOT/tests/test-helpers.el" \
      --load "$DEVENV_ROOT/tests/test-all.el" \
      --eval "(ert-run-tests-batch-and-exit '(or (tag smoke) (tag fast)))"
    echo "Tests passed!"
  '';

  # Flake output packages — build with `devenv build`
  outputs = {
    jotain = pkgs.callPackage ./nix/package.nix { };
    emacs = pkgs.callPackage ./emacs.nix { devMode = false; };
    emacs-dev = jotainEmacs;
  };

  # Claude Code integration
  claude.code = {
    enable = true;
    mcpServers = {
      # Local devenv MCP server
      devenv = {
        type = "stdio";
        command = "devenv";
        args = [ "mcp" ];
        env = {
          DEVENV_ROOT = config.devenv.root;
        };
      };
    };
  };

  env = {
    JOTAIN_DEV_MODE = "1";
  };

  # Development wrapper for emacs that uses local sources
  scripts.emacs-dev.exec = ''
    # Dynamically find project root (look for flake.nix)
    PROJECT_ROOT="$PWD"
    while [ ! -f "$PROJECT_ROOT/flake.nix" ] && [ "$PROJECT_ROOT" != "/" ]; do
      PROJECT_ROOT="$(dirname "$PROJECT_ROOT")"
    done

    if [ ! -f "$PROJECT_ROOT/flake.nix" ]; then
      echo "Error: Could not find project root (no flake.nix found)"
      exit 1
    fi

    # Use local sources
    export JOTAIN_DEV_MODE=1
    export JOTAIN_ROOT="$PROJECT_ROOT"
    export JOTAIN_LISP_DIR="$PROJECT_ROOT/lisp"
    export JOTAIN_MODULES_DIR="$PROJECT_ROOT/modules"

    # Prepend local lisp and modules so live edits are picked up without a Nix rebuild
    EMACSLOADPATH="$PROJECT_ROOT/lisp:$PROJECT_ROOT/modules:''${EMACSLOADPATH:-}"
    export EMACSLOADPATH

    # Isolated user directory
    export JOTAIN_DEV_HOME="$PROJECT_ROOT/.dev-home"
    export XDG_CONFIG_HOME="$JOTAIN_DEV_HOME/.config"
    export XDG_DATA_HOME="$JOTAIN_DEV_HOME/.local/share"
    export XDG_CACHE_HOME="$JOTAIN_DEV_HOME/.cache"
    export XDG_STATE_HOME="$JOTAIN_DEV_HOME/.local/state"

    mkdir -p "$XDG_CONFIG_HOME/emacs"
    mkdir -p "$XDG_DATA_HOME/emacs"
    mkdir -p "$XDG_CACHE_HOME/emacs"
    mkdir -p "$XDG_STATE_HOME/emacs"

    # Symlink config files
    if [ -f "$PROJECT_ROOT/early-init.el" ]; then
      ln -sfn "$PROJECT_ROOT/early-init.el" "$XDG_CONFIG_HOME/emacs/early-init.el"
    fi

    if [ -f "$PROJECT_ROOT/init.el" ]; then
      ln -sfn "$PROJECT_ROOT/init.el" "$XDG_CONFIG_HOME/emacs/init.el"
    fi

    # Create symlinks for lisp and modules directories so user-emacs-directory paths work
    ln -sfn "$PROJECT_ROOT/lisp" "$XDG_CONFIG_HOME/emacs/lisp"
    ln -sfn "$PROJECT_ROOT/modules" "$XDG_CONFIG_HOME/emacs/modules"

    # Run Emacs with explicit init directory to ensure XDG location is used
    exec ${jotainEmacs}/bin/emacs --init-directory="$XDG_CONFIG_HOME/emacs" "$@"
  '';


  enterShell = ''
    export JOTAIN_ROOT="$PWD"
    export JOTAIN_LISP_DIR="$PWD/lisp"
    export JOTAIN_MODULES_DIR="$PWD/modules"

    mkdir -p .dev-home
  '';
}
