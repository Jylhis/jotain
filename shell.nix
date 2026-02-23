# Development shell with XDG isolation
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-25.05-small.tar.gz") { }
, jotainEmacs ? pkgs.callPackage ./emacs.nix { inherit pkgs; }
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

    # Prepend local elisp so local files shadow the Nix-built jotain-modules
    EMACSLOADPATH="$PROJECT_ROOT/elisp:''${EMACSLOADPATH:-}"
    export EMACSLOADPATH

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
    ln -sfn "$PROJECT_ROOT/config" "$XDG_CONFIG_HOME/emacs/config"


    # Run Emacs with explicit init directory to ensure XDG location is used
    exec ${jotainEmacs}/bin/emacs --init-directory="$XDG_CONFIG_HOME/emacs" "$@"
  '';

  # MCP server packages built from GitHub sources
  mcpServerLib = pkgs.emacsPackages.trivialBuild {
    pname = "mcp-server-lib";
    version = "unstable";
    src = pkgs.fetchFromGitHub {
      owner = "laurynas-biveinis";
      repo = "mcp-server-lib.el";
      rev = "8c494fbb79cc7aba789d5ca583e3736345b39a15";
      hash = "sha256-zagpSpsXzxsWxEx3PW9SyuXfy1nWhivvPUzkeN8a2AE=";
    };
    postPatch = ''
      rm -f mcp-server-lib-test.el mcp-server-lib-ert.el mcp-server-lib-bytecode-handler-test.el
    '';
  };

  orgMcp = pkgs.emacsPackages.trivialBuild {
    pname = "org-mcp";
    version = "unstable";
    src = pkgs.fetchFromGitHub {
      owner = "laurynas-biveinis";
      repo = "org-mcp";
      rev = "536a24325ce2417e109e698de705abac1a0c7f79";
      hash = "sha256-OYcBLNEtUjc6J2mG/kRX/SJz19R17fqbgOu4KNI/Vkw=";
    };
    packageRequires = [ mcpServerLib ];
    postPatch = ''
      rm -f org-mcp-test.el
    '';
  };

  emacsWithOrgMcp = pkgs.emacsWithPackages (_: [ mcpServerLib orgMcp ]);

  # mcp-language-server pre-configured for Nix with nil LSP
  mcpLanguageServerNix = pkgs.writeShellScriptBin "mcp-language-server-nix" ''
    exec ${pkgs.mcp-language-server}/bin/mcp-language-server \
      --workspace "$PWD" \
      --lsp "${pkgs.nil}/bin/nil"
  '';

  # org-mcp stdio bridge: starts a minimal Emacs daemon, bridges MCP stdio
  orgMcpServer = pkgs.writeShellScriptBin "org-mcp-stdio" ''
    set -euo pipefail

    SOCKET=$(mktemp -u "/tmp/org-mcp-XXXXXX")

    cleanup() {
      ${emacsWithOrgMcp}/bin/emacsclient -s "''${SOCKET}" --no-wait \
        -e "(kill-emacs)" >/dev/null 2>&1 || true
      rm -f "''${SOCKET}"
    }
    trap cleanup EXIT INT TERM

    ${emacsWithOrgMcp}/bin/emacs --daemon="''${SOCKET}" >/dev/null 2>&1 &

    for _i in $(seq 1 30); do
      [ -S "''${SOCKET}" ] && break
      sleep 0.1
    done
    [ -S "''${SOCKET}" ] || { echo "org-mcp: emacs daemon failed to start" >&2; exit 1; }

    ${emacsWithOrgMcp}/bin/emacsclient -s "''${SOCKET}" \
      -e "(progn (require 'mcp-server-lib) (require 'org-mcp) (org-mcp-enable) (mcp-server-lib-start))" \
      >/dev/null 2>&1

    while IFS= read -r line; do
      b64=$(printf '%s' "$line" | base64 -w0)
      expr="(base64-encode-string (encode-coding-string (or (mcp-server-lib-process-jsonrpc (base64-decode-string \"''${b64}\") \"org-mcp\") \"\") 'utf-8 t) t)"
      resp=$(${emacsWithOrgMcp}/bin/emacsclient -s "''${SOCKET}" -e "''${expr}" 2>/dev/null)
      out=$(printf '%s' "''${resp}" | sed 's/^"//;s/"$//' | base64 -d 2>/dev/null) || true
      [ -n "''${out}" ] && printf '%s\n' "''${out}"
    done
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

    # MCP server wrappers
    mcpLanguageServerNix
    orgMcpServer
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

    # Prepend local CLI to PATH for direct access
    export PATH="$PWD/cli:$PATH"

    # Add .dev-home to .gitignore if not already there
    if [ -f .gitignore ] && ! grep -q "^\.dev-home" .gitignore 2>/dev/null; then
      echo ".dev-home/" >> .gitignore
    fi

    # Create .dev-home directory
    mkdir -p .dev-home
  '';
}
