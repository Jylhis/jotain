# Development shell with XDG isolation
{ pkgs
, jotainEmacs
, ...
}:

let
  # Use toString to get the actual path, not the Nix store derivation
  projectRoot = toString ../..;

  # Development wrapper for jot CLI that uses local sources
  devJot = pkgs.writeShellScriptBin "jot" ''
    #!/usr/bin/env bash

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
    export JOTAIN_ELISP_DIR="$PROJECT_ROOT/elisp"
    export JOTAIN_CLI_DIR="$PROJECT_ROOT/cli"

    # Isolated user directory
    export JOTAIN_DEV_HOME="$PROJECT_ROOT/.dev-home"
    mkdir -p "$JOTAIN_DEV_HOME"

    # Override XDG paths to isolate from system
    export XDG_CONFIG_HOME="$JOTAIN_DEV_HOME/.config"
    export XDG_DATA_HOME="$JOTAIN_DEV_HOME/.local/share"
    export XDG_CACHE_HOME="$JOTAIN_DEV_HOME/.cache"
    export XDG_STATE_HOME="$JOTAIN_DEV_HOME/.local/state"

    mkdir -p "$XDG_CONFIG_HOME/emacs"
    mkdir -p "$XDG_DATA_HOME/emacs"
    mkdir -p "$XDG_CACHE_HOME/emacs"
    mkdir -p "$XDG_STATE_HOME/emacs"

    # Use development Emacs
    export PATH="${jotainEmacs}/bin:$PATH"

    # Run local CLI script if it exists, otherwise show help
    if [ -f "$PROJECT_ROOT/cli/jot" ]; then
      exec bash "$PROJECT_ROOT/cli/jot" "$@"
    else
      echo "CLI not yet created. Use 'emacs' to launch development Emacs."
      exit 1
    fi
  '';

  # Development wrapper for emacs that uses local sources
  devEmacsWrapper = pkgs.writeShellScriptBin "emacs-dev" ''
        #!/usr/bin/env bash

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
        export JOTAIN_ELISP_DIR="$PROJECT_ROOT/elisp"

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

        # Create early-init.el if it exists
        if [ -f "$PROJECT_ROOT/templates/early-init.el" ]; then
          cp "$PROJECT_ROOT/templates/early-init.el" "$XDG_CONFIG_HOME/emacs/early-init.el"
        fi

        # Create init.el that loads from local sources
        cat > "$XDG_CONFIG_HOME/emacs/init.el" <<'EOF'
    ;; Development init.el - loads Jotain from local sources

    ;; Add local elisp directory to load path
    (add-to-list 'load-path (expand-file-name "elisp" (getenv "JOTAIN_ROOT")))
    (add-to-list 'load-path (expand-file-name "elisp/core" (getenv "JOTAIN_ROOT")))
    (add-to-list 'load-path (expand-file-name "elisp/ui" (getenv "JOTAIN_ROOT")))
    (add-to-list 'load-path (expand-file-name "elisp/completion" (getenv "JOTAIN_ROOT")))
    (add-to-list 'load-path (expand-file-name "elisp/editor" (getenv "JOTAIN_ROOT")))
    (add-to-list 'load-path (expand-file-name "elisp/programming" (getenv "JOTAIN_ROOT")))

    ;; Load Jotain if it exists
    (when (file-exists-p (expand-file-name "elisp/jotain.el" (getenv "JOTAIN_ROOT")))
      (require 'jotain))

    ;; Development mode indicator
    (setq frame-title-format '("Jotain DEV - %b"))
    (message "Jotain loaded from: %s" (getenv "JOTAIN_ELISP_DIR"))
    EOF

        # Run Emacs
        exec ${jotainEmacs}/bin/emacs "$@"
  '';

  # LSP servers and development tools
  devTools = with pkgs; [
    # LSP servers
    nil # Nix LSP
    bash-language-server # Shell LSP

    # Formatters
    nixpkgs-fmt
    shfmt

    # Linters
    shellcheck

    # Tools
    ripgrep
    fd
    git

    # Nix tools
    nix-prefetch-git

    # Testing
    just
  ];

in
pkgs.mkShell {
  name = "jotain-dev";

  buildInputs = [
    # Emacs with all dependencies
    jotainEmacs

    # Development wrappers
    devJot
    devEmacsWrapper
  ] ++ devTools;

  shellHook = ''
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo "  Jotain Development Environment"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    echo ""
    echo "Development mode enabled - using local sources"
    echo ""
    echo "Commands:"
    echo "  emacs-dev        - Run Emacs (isolated, local sources)"
    echo "  jot              - Run Jotain CLI (local sources)"
    echo "  just test        - Run tests"
    echo "  just build       - Build package"
    echo "  just check       - Run flake checks"
    echo ""
    echo "Isolation:"
    echo "  User dir: .dev-home/ (isolated from system)"
    echo "  Sources:  $PWD/elisp"
    echo ""
    echo "Changes to elisp/ and cli/ take effect immediately!"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

    # Set up development environment
    export JOTAIN_DEV_MODE=1
    export JOTAIN_ROOT="$PWD"
    export JOTAIN_ELISP_DIR="$PWD/elisp"
    export JOTAIN_CLI_DIR="$PWD/cli"

    # Add .dev-home to .gitignore if not already there
    if [ -f .gitignore ] && ! grep -q "^\.dev-home" .gitignore 2>/dev/null; then
      echo ".dev-home/" >> .gitignore
    fi

    # Create .dev-home directory
    mkdir -p .dev-home
  '';
}
