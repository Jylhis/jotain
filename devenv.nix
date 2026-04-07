{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
{
  imports = [ ./nix/devenv-emacs-lisp.nix ];

  # https://devenv.sh/languages/
  # languages.rust.enable = true;
  languages = {
    nix = {
      enable = true;
    };
    python = {
      enable = true;
    };
    emacs-lisp = {

      enable = true;
      lsp.enable = false;
      elsa.enable = false;
    };
  };
  treefmt = {
    enable = true;
    config.programs = {
      nixfmt.enable = true;
    };
  };

  # Pre-commit binary lives in the dev shell so contributors can
  # `pre-commit install` once and let `.pre-commit-config.yaml` (in
  # the repo root) drive the actual hooks.  This avoids pulling in
  # the cachix/git-hooks.nix flake input.
  packages = [ pkgs.pre-commit ];

  enterShell = ''
    if [ -d .git ] && [ ! -f .git/hooks/pre-commit ]; then
      pre-commit install --install-hooks >/dev/null 2>&1 || true
    fi
  '';

  claude.code = {
    enable = true;
    mcpServers = {
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
}
