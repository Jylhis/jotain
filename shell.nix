# Traditional nix-shell compatibility wrapper
# This file provides backward compatibility for users who prefer nix-shell
# over nix develop. It delegates to the flake's devShell.
#
# Usage:
#   nix-shell                    # Use default devShell
#   nix-shell --arg ci true      # Use CI devShell
#   nix-shell --argstr emacsDir ./custom-config  # Custom config directory

{
  # Optional argument for CI environment
  ci ? false,
  # Optional argument for custom Emacs configuration directory
  emacsDir ? null,
}:

let
  # Load the flake
  flake = builtins.getFlake (toString ./.);

  # Detect the current system
  system = builtins.currentSystem;

  # Select the appropriate devShell based on arguments
  devShell = if ci then flake.devShells.${system}.ci else flake.devShells.${system}.default;

in
# Return the selected devShell
devShell.overrideAttrs (old: {
  shellHook =
    (old.shellHook or "")
    + (
      if emacsDir != null then
        ''
          # Override DOOMDIR if emacsDir is specified
          export DOOMDIR="${toString emacsDir}"
          echo ""
          echo "📁 Using custom Emacs configuration directory:"
          echo "   DOOMDIR: $DOOMDIR"
          echo ""
        ''
      else
        ""
    );
})
