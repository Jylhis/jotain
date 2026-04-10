# Development shell with XDG isolation
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-25.11-small.tar.gz") { }
, jotainEmacs ? pkgs.callPackage ./emacs.nix { inherit pkgs; }
, ...
}:

let
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

    # Prepend local elisp so local files shadow the Nix-built jotain-modules
    EMACSLOADPATH="$PROJECT_ROOT/elisp:''${EMACSLOADPATH:-}"
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

    # Copy template files (don't symlink, as we'll modify init.el)
    if [ -f "$PROJECT_ROOT/early-init.el" ]; then
      ln -sfn "$PROJECT_ROOT/early-init.el" "$XDG_CONFIG_HOME/emacs/early-init.el"
    fi

    if [ -f "$PROJECT_ROOT/init.el" ]; then
      ln -sfn "$PROJECT_ROOT/init.el" "$XDG_CONFIG_HOME/emacs/init.el"
    fi

    # Create symlink for elisp directory so template's user-emacs-directory paths work
    ln -sfn "$PROJECT_ROOT/elisp" "$XDG_CONFIG_HOME/emacs/elisp"

    # Run Emacs with explicit init directory to ensure XDG location is used
    exec ${jotainEmacs}/bin/emacs --init-directory="$XDG_CONFIG_HOME/emacs" "$@"
  '';

  # mcp-language-server pre-configured for Nix with nil LSP
  mcpLanguageServerNix = pkgs.writeShellScriptBin "mcp-language-server-nix" ''
    exec ${pkgs.mcp-language-server}/bin/mcp-language-server \
      --workspace "$PWD" \
      --lsp "${pkgs.nil}/bin/nil"
  '';

  # LSP servers and development tools
  devTools = with pkgs; [
    # LSP servers
    nil # Nix LSP
    bash-language-server # Shell LSP

    # MCP servers
    mcp-nixos
    mcp-language-server

    # Formatters
    nixpkgs-fmt
    shfmt

    # Linters
    shellcheck
    statix
    deadnix

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
    devEmacsWrapper

    # MCP server wrappers
    mcpLanguageServerNix
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
    echo "  just test        - Run tests"
    echo "  just build       - Build package"
    echo "  just check       - Run flake checks"
    echo ""
    echo "Isolation:"
    echo "  User dir: .dev-home/ (isolated from system)"
    echo "  Sources:  $PWD/elisp"
    echo ""
    echo "Changes to elisp/ take effect immediately!"
    echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"

    # Set up development environment
    export JOTAIN_DEV_MODE=1
    export JOTAIN_ROOT="$PWD"
    export JOTAIN_ELISP_DIR="$PWD/elisp"

    # Add .dev-home to .gitignore if not already there
    if [ -f .gitignore ] && ! grep -q "^\.dev-home" .gitignore 2>/dev/null; then
      echo ".dev-home/" >> .gitignore
    fi

    # Create .dev-home directory
    mkdir -p .dev-home
  '';
}
