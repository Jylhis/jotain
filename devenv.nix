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

  claude.code = {
    enable = true;
    hooks = {
      setup-web-env = {
        hookType = "SessionStart";
        command = "bash ${config.devenv.root}/scripts/setup-web-env.sh";
      };
    };
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
