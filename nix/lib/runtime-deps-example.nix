# Example usage of runtime-deps.nix
#
# This file demonstrates various ways to use the runtime dependencies
# defined in runtime-deps.nix for different deployment scenarios.

{ pkgs ? import <nixpkgs> { } }:

let
  jotainLib = import ./. { inherit pkgs; inherit (pkgs) lib; };
  runtimeDeps = jotainLib.runtimeDeps;

in
{
  # Example 1: Home Manager configuration with all runtime dependencies
  homeManagerFull = {
    # Note: Fonts and runtime deps combined in home.packages
    home.packages = runtimeDeps.fonts ++ runtimeDeps.allRuntimeDeps;
    fonts.fontconfig.enable = true;
  };

  # Example 2: Selective installation (LSP servers + essential tools only)
  homeManagerMinimal = {
    home.packages = runtimeDeps.lspServers ++ [
      pkgs.ripgrep
      pkgs.fd
      pkgs.git
    ];
  };

  # Example 3: Development shell with all LSP servers and tools
  devShell = pkgs.mkShell {
    buildInputs = runtimeDeps.devEnv;

    shellHook = ''
      echo "Jotain development environment loaded"
      echo "LSP servers: ${toString (builtins.length runtimeDeps.lspServers)}"
      echo "CLI tools: ${toString (builtins.length runtimeDeps.cliTools)}"
    '';
  };

  # Example 4: NixOS system configuration
  nixosSystemConfig = {
    environment.systemPackages = runtimeDeps.allRuntimeDeps;
    fonts.packages = runtimeDeps.fonts;
  };

  # Example 5: Custom selection by category
  customSelection = {
    home.packages =
      runtimeDeps.lspServers # All LSP servers
      ++ [ pkgs.ripgrep pkgs.fd ] # Just search tools from CLI tools
      ++ runtimeDeps.treesitterGrammars; # Optional: system-wide grammars
  };

  # Example 6: Accessing individual categories
  categories = {
    lspCount = builtins.length runtimeDeps.lspServers;
    cliCount = builtins.length runtimeDeps.cliTools;
    fontCount = builtins.length runtimeDeps.fonts;
    grammarCount = builtins.length runtimeDeps.treesitterGrammars;

    # List all LSP server names
    lspNames = builtins.map (p: p.pname or p.name or "unknown") runtimeDeps.lspServers;
  };
}
