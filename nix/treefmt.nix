# Shared treefmt configuration
#
# Used by both flake-parts (flake.nix) and devenv (devenv.nix).
# Returns treefmt-nix module config (programs, settings, formatters).
{ pkgs }:

{
  programs.nixpkgs-fmt.enable = true;
  programs.shellcheck.enable = true;
  programs.shfmt = {
    enable = true;
    indent_size = 2;
  };

  settings.global.excludes = [
    # Git
    ".git/**"
    # Nix build artifacts
    "result"
    "result-*"
    # Development artifacts
    ".dev-home/**"
    ".devenv*"
    ".direnv/**"
    # Test artifacts
    "tests/.gitignore"
    # Lock files
    "flake.lock"
    "devenv.lock"
  ];

  # Custom formatters
  settings.formatter = {
    # Emacs Lisp formatter using Emacs built-in indentation
    elisp = {
      command = pkgs.writeShellScriptBin "format-elisp" ''
        for file in "$@"; do
          ${pkgs.emacs}/bin/emacs --batch \
            -l elisp-mode \
            "$file" \
            --eval '(indent-region (point-min) (point-max))' \
            -f save-buffer 2>/dev/null
        done
      '';
      includes = [ "*.el" ];
    };
  };
}
