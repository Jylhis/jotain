# nix/user-programs.nix — the programs a user runs for the configured Emacs: the
# wrapped `emacs`, the EDITOR and VISUAL scripts, `emacsctl` and the byte-compiled
# config. One definition for module.nix (Home Manager) and for module-system.nix
# (services.emacsConfig.users.<user>, installed in the user's profile), so a host that
# builds them in NixOS and one that builds them in Home Manager get the same store paths.
{
  pkgs,
  pkgsWithOverlay,
  # The selected Emacs distribution (cfg.package or emacsConfigPackages).
  package,
  # Where the config is deployed; --init-directory is pinned here so a stray
  # ~/.emacs.d/ is never picked up.
  initDirectory,
  nativeCompile,
  # Booleans: devenv, sonarlint, dockerfileLsp, onePassword, sops, claudeCode.
  tools,
}:
let
  inherit (pkgs) lib;

  # Shared runtime binaries (nix/runtime-deps.nix) plus the opt-in tools,
  # prepended to PATH in emacsWrapper, since the systemd user
  # service does not inherit the login-shell PATH.
  runtimeDeps =
    import ./runtime-deps.nix { inherit pkgs pkgsWithOverlay; }
    ++ lib.optional tools.devenv pkgs.devenv
    ++ lib.optional tools.sonarlint pkgs.sonarlint-ls
    ++ lib.optional tools.dockerfileLsp pkgs.dockerfile-language-server
    ++ lib.optional tools.onePassword pkgs._1password-cli
    ++ lib.optional tools.sops pkgs.sops
    ++ lib.optional tools.claudeCode pkgs.claude-code;

  # Byte-compiled config, so the daemon loads .elc instead of
  # interpreting .el (and deferred native compilation, which never fires
  # for plain .el loads, can fill var/eln-cache).
  #
  # Same derivation as nix/checks.nix' `elisp-compile', so on the default
  # configuration `home-manager switch' substitutes CI's cachix artifact.
  # A custom `services.emacsConfig.package' or nixpkgs pin builds it locally.
  # `.core' is the inner emacsWithPackages result (see
  # nix/config-compiled.nix); the `or' covers a custom package.
  compiledConfig = import ./config-compiled.nix {
    inherit pkgs nativeCompile;
    emacs = package.core or package;
  };

  runtimePath = lib.makeBinPath runtimeDeps;

  # `emacs` with --init-directory always set, for the daemon and any
  # interactive invocation.
  emacsWrapper = pkgs.writeShellScriptBin "emacs" ''
    export PATH=${runtimePath}''${PATH:+:$PATH}
    ${lib.optionalString nativeCompile ''
      export EMACS_CONFIG_ELN_PATH=${compiledConfig}/share/emacs/native-lisp
    ''}
    exec ${package}/bin/emacs --init-directory=${lib.escapeShellArg initDirectory} "$@"
  '';

  # EDITOR fallback without a daemon; via emacsWrapper to keep
  # --init-directory.
  editorFallback = pkgs.writeShellScript "emacs-config-editor-fallback" ''
    exec ${emacsWrapper}/bin/emacs -nw -- "$@"
  '';
in
{
  inherit
    package
    compiledConfig
    emacsWrapper
    runtimeDeps
    ;

  # EDITOR — terminal-friendly emacsclient (works over SSH, in git commit, etc.)
  editorScript = pkgs.writeShellScriptBin "emacs-config-editor" ''
    exec ${lib.getBin package}/bin/emacsclient \
      --tty \
      --alternate-editor=${editorFallback} \
      -- \
      "$@"
  '';

  # VISUAL — opens a GUI emacsclient frame.
  visualScript = pkgs.writeShellScriptBin "emacs-config-visual" ''
    exec ${lib.getBin package}/bin/emacsclient \
      --create-frame \
      --alternate-editor=${emacsWrapper}/bin/emacs \
      -- \
      "$@"
  '';

  # `emacsctl <start|stop|status|restart|logs>`: drives the systemd user
  # service.
  emacsctlScript = pkgs.writeShellScriptBin "emacsctl" ''
    case "''${1:-status}" in
      start)   exec systemctl --user start emacs-config ;;
      stop)    exec systemctl --user stop emacs-config ;;
      restart) exec systemctl --user restart emacs-config ;;
      status)  exec systemctl --user status emacs-config ;;
      logs)    exec journalctl --user -u emacs-config -f ;;
      *) echo "usage: emacsctl {start|stop|status|restart|logs}" >&2; exit 2 ;;
    esac
  '';
}
